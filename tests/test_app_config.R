library(testthat)
source(file.path("R", "app_config.R"))

test_that("%||% returns fallback for NULL and empty", {
  expect_equal(NULL %||% 5, 5)
  expect_equal(character(0) %||% "x", "x")
  expect_equal(3 %||% 9, 3)
})

test_that("outcome codes include the core outcomes", {
  expect_true(all(c("1B", "HR", "BB", "K", "FC") %in% APP_CONFIG$outcome_codes))
})
