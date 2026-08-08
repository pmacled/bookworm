library(testthat)
source(file.path("R", "app_config.R"))
source(file.path("R", "boxscore.R"))

test_that("%||% returns fallback for NULL and empty", {
  expect_equal(NULL %||% 5, 5)
  expect_equal(character(0) %||% "x", "x")
  expect_equal(3 %||% 9, 3)
})

test_that("outcome codes include the core outcomes", {
  expect_true(all(c("1B", "HR", "BB", "K", "FC") %in% APP_CONFIG$outcome_codes))
})

test_that("outcome_codes is derived from outcome_meta", {
  expect_identical(APP_CONFIG$outcome_codes, names(APP_CONFIG$outcome_meta))
  expect_length(APP_CONFIG$outcome_codes, 18L)
  expect_true("ITPHR" %in% APP_CONFIG$outcome_codes)
})

test_that("every outcome has a label, description, and valid category", {
  valid <- c("hit", "on_base", "out", "other")
  for (code in names(APP_CONFIG$outcome_meta)) {
    m <- APP_CONFIG$outcome_meta[[code]]
    expect_true(nzchar(m$label %||% ""),       info = paste(code, "needs a label"))
    expect_true(nzchar(m$description %||% ""), info = paste(code, "needs a description"))
    expect_true(m$category %in% valid,         info = paste(code, "has category", m$category))
  }
})

test_that("the out categories match the reducer's out list", {
  outs <- names(Filter(function(m) identical(m$category, "out"), APP_CONFIG$outcome_meta))
  expect_setequal(outs, c("K", "KL", "GO", "FO", "LO", "PO"))
})

test_that("the hit categories match the box score's hit list", {
  hits <- names(Filter(function(m) identical(m$category, "hit"), APP_CONFIG$outcome_meta))
  expect_setequal(hits, .HIT)
})
