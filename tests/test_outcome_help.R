library(testthat)
suppressMessages({
  library(shiny)
  library(bslib)
  library(htmltools)
})
source(file.path("R", "app_config.R"))
source(file.path("R", "outcome_help.R"))

test_that("the help panel names every outcome code and its label", {
  html <- as.character(outcome_help_ui())
  for (code in APP_CONFIG$outcome_codes) {
    expect_true(
      grepl(code, html, fixed = TRUE),
      info = paste("missing code:", code)
    )
    expect_true(
      grepl(APP_CONFIG$outcome_meta[[code]]$label, html, fixed = TRUE),
      info = paste("missing label for:", code)
    )
  }
})

test_that("the help panel groups by category", {
  html <- as.character(outcome_help_ui())
  for (heading in c("Hits", "Reached base", "Outs", "Other")) {
    expect_true(
      grepl(heading, html, fixed = TRUE),
      info = paste("missing heading:", heading)
    )
  }
})
