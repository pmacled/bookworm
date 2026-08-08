library(testthat)
source(file.path("R", "app_config.R"))
source(file.path("R", "supabase_client.R"))

test_that("gotrue parser extracts user id and token", {
  body <- list(access_token = "tok", user = list(id = "u-123"))
  parsed <- .gotrue_parse(body)
  expect_true(parsed$ok)
  expect_equal(parsed$user_id, "u-123")
  expect_equal(parsed$access_token, "tok")
})

test_that("gotrue parser reports errors", {
  parsed <- .gotrue_parse(list(error_description = "bad creds"))
  expect_false(parsed$ok)
  expect_match(parsed$error, "bad creds")
})

test_that("a connectivity failure returns an error result instead of throwing", {
  # SUPABASE_URL unset => the request targets "/auth/v1/token?..." with no host.
  withr_vars <- c(SUPABASE_URL = "", SUPABASE_ANON_KEY = "")
  old <- Sys.getenv(names(withr_vars), unset = NA)
  do.call(Sys.setenv, as.list(withr_vars))
  on.exit({
    for (n in names(old)) if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, setNames(list(old[[n]]), n))
  }, add = TRUE)

  res <- gotrue_sign_in("nobody@example.com", "hunter2")
  expect_false(res$ok)
  expect_true(nzchar(res$error))
  expect_true(is.na(res$user_id))
})

test_that("friendly_auth_error rewrites known GoTrue messages", {
  expect_match(friendly_auth_error("Invalid login credentials"), "email or password")
  expect_match(friendly_auth_error("User already registered"), "already")
  # Unknown messages pass through unchanged.
  expect_equal(friendly_auth_error("teapot"), "teapot")
  # Empty or missing collapses to a generic message.
  expect_true(nzchar(friendly_auth_error("")))
  expect_true(nzchar(friendly_auth_error(NA_character_)))
})
