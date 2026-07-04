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
