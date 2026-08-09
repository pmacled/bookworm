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

test_that("an unconfigured deployment (empty SUPABASE_URL) returns an error result without attempting a request", {
  # This exercises the early "not configured" guard in .gotrue_request, not the
  # tryCatch around req_perform() — the guard short-circuits before any request is
  # built, so no network call (successful or failing) ever happens here.
  withr_vars <- c(SUPABASE_URL = "", SUPABASE_ANON_KEY = "")
  old <- Sys.getenv(names(withr_vars), unset = NA)
  do.call(Sys.setenv, as.list(withr_vars))
  on.exit(
    {
      for (n in names(old)) {
        if (is.na(old[[n]])) {
          Sys.unsetenv(n)
        } else {
          do.call(Sys.setenv, setNames(list(old[[n]]), n))
        }
      }
    },
    add = TRUE
  )

  res <- gotrue_sign_in("nobody@example.com", "hunter2")
  expect_false(res$ok)
  expect_equal(res$error, "Saving is not configured on this deployment.")
  expect_true(is.na(res$user_id))
})

test_that("a transport failure from the performing step returns an error result instead of throwing", {
  # SUPABASE_URL must be non-empty here so the "not configured" guard doesn't
  # short-circuit before the injected performer runs — this test's whole point is to
  # reach the tryCatch around the request-performing step with a stub that throws,
  # simulating a DNS failure / refused connection / TLS error without any real network
  # access.
  old <- Sys.getenv("SUPABASE_URL", unset = NA)
  Sys.setenv(SUPABASE_URL = "https://example.invalid")
  on.exit(
    if (is.na(old)) {
      Sys.unsetenv("SUPABASE_URL")
    } else {
      Sys.setenv(SUPABASE_URL = old)
    },
    add = TRUE
  )

  boom <- function(req) stop("simulated transport failure")
  res <- gotrue_sign_in("nobody@example.com", "hunter2", perform = boom)
  expect_false(res$ok)
  expect_equal(res$error, "Could not reach the sign-in service.")
  expect_true(is.na(res$user_id))
  expect_true(is.na(res$access_token))
})

test_that("friendly_auth_error rewrites known GoTrue messages", {
  expect_match(
    friendly_auth_error("Invalid login credentials"),
    "email or password"
  )
  expect_match(friendly_auth_error("User already registered"), "already")
  # Unknown messages pass through unchanged.
  expect_equal(friendly_auth_error("teapot"), "teapot")
  # Empty or missing collapses to a generic message.
  expect_true(nzchar(friendly_auth_error("")))
  expect_true(nzchar(friendly_auth_error(NA_character_)))
})
