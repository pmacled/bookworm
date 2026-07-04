library(testthat)
source(file.path("R", "app_config.R"))
source(file.path("R", "rules_engine.R"))

test_that("default config is valid", {
  cfg <- default_ruleset_config()
  expect_equal(cfg$starting_count$balls, 1L)
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("coerce merges partial over defaults", {
  cfg <- coerce_ruleset_config(list(innings = 5, starting_count = list(balls = 0, strikes = 2)))
  expect_equal(cfg$innings, 5L)
  expect_equal(cfg$starting_count$strikes, 2L)
  expect_equal(cfg$foul_out_rule, "out")  # untouched default
})

test_that("validation rejects bad starting count and unknown enums", {
  bad <- default_ruleset_config()
  bad$starting_count$strikes <- 3L
  expect_false(validate_ruleset_config(bad)$ok)

  bad2 <- default_ruleset_config()
  bad2$foul_out_rule <- "explode"
  expect_false(validate_ruleset_config(bad2)$ok)

  bad3 <- default_ruleset_config()
  bad3$batting_gender_rule$type <- "every_n"
  bad3$batting_gender_rule$n <- NA_integer_   # every_n requires n
  expect_false(validate_ruleset_config(bad3)$ok)
})
