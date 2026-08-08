library(testthat)
source(file.path("R", "app_config.R"))
source(file.path("R", "rules_engine.R"))

test_that("default config is valid", {
  cfg <- default_ruleset_config()
  expect_equal(cfg$starting_count$balls, 0L)
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("coerce merges partial over defaults", {
  cfg <- coerce_ruleset_config(list(innings = 5, starting_count = list(balls = 0, strikes = 2)))
  expect_equal(cfg$innings, 5L)
  expect_equal(cfg$starting_count$strikes, 2L)
  expect_equal(cfg$foul_out_rule, "unlimited")  # untouched default
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

test_that("foul_out_rule accepts unlimited", {
  cfg <- default_ruleset_config(); cfg$foul_out_rule <- "unlimited"
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("batting_size defaults to unlimited (NA) and validates", {
  expect_true(is.na(default_ruleset_config()$batting_size))
  expect_equal(coerce_ruleset_config(list(batting_size = 10))$batting_size, 10L)
  expect_equal(coerce_ruleset_config(list(batting_size = 0))$batting_size, NA_integer_)  # 0 => unlimited
  bad <- default_ruleset_config(); bad$batting_size <- -3L
  expect_false(validate_ruleset_config(bad)$ok)
})

test_that("the new default is Anything Goes: 0-0 count, unlimited fouls, no gender rule", {
  cfg <- default_ruleset_config()
  expect_equal(cfg$starting_count$balls, 0L)
  expect_equal(cfg$starting_count$strikes, 0L)
  expect_equal(cfg$foul_out_rule, "unlimited")
  expect_equal(cfg$batting_gender_rule$type, "none")
  expect_true(is.na(cfg$run_cap$per_inning))
  expect_length(cfg$mercy_rule$tiers, 0L)
  expect_true(is.na(cfg$home_run_rule$over_fence_limit))
  expect_true(is.na(cfg$pinch_runner$max_per_inning))
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("legacy scalar run-cap keys migrate into run_cap", {
  cfg <- coerce_ruleset_config(list(run_cap_per_inning = 5L, open_last_inning = FALSE))
  expect_equal(cfg$run_cap$per_inning, 5L)
  expect_false(cfg$run_cap$open_last_inning)
  expect_true(cfg$run_cap$same_play_runs_count)   # new field takes its default
  expect_null(cfg$run_cap_per_inning)             # old key is gone, not shadowing
})

test_that("legacy scalar mercy keys migrate into a single tier", {
  cfg <- coerce_ruleset_config(list(mercy_rule = list(differential = 10L, after_inning = 4L)))
  expect_length(cfg$mercy_rule$tiers, 1L)
  expect_equal(cfg$mercy_rule$tiers[[1]]$differential, 10L)
  expect_equal(cfg$mercy_rule$tiers[[1]]$after_inning, 4L)
  expect_null(cfg$mercy_rule$differential)
})

test_that("a legacy mercy differential with no after_inning defaults to inning 1", {
  cfg <- coerce_ruleset_config(list(mercy_rule = list(differential = 10L)))
  expect_equal(cfg$mercy_rule$tiers[[1]]$after_inning, 1L)
})

test_that("legacy batting-gender type names migrate", {
  a <- coerce_ruleset_config(list(batting_gender_rule = list(type = "no_two_males_consecutive")))
  expect_equal(a$batting_gender_rule$type, "max_consecutive_males")
  expect_equal(a$batting_gender_rule$n, 1L)

  b <- coerce_ruleset_config(list(batting_gender_rule = list(type = "every_other")))
  expect_equal(b$batting_gender_rule$type, "max_consecutive_same_gender")
  expect_equal(b$batting_gender_rule$n, 1L)

  c3 <- coerce_ruleset_config(list(batting_gender_rule = list(type = "every_n", n = 4L)))
  expect_equal(c3$batting_gender_rule$type, "min_females_per_n")
  expect_equal(c3$batting_gender_rule$n, 4L)
})

test_that("the legacy courtesy_runner boolean migrates", {
  on  <- coerce_ruleset_config(list(courtesy_runner = TRUE))
  expect_true(is.na(on$pinch_runner$max_per_game))     # unlimited
  off <- coerce_ruleset_config(list(courtesy_runner = FALSE))
  expect_equal(off$pinch_runner$max_per_game, 0L)
  expect_null(on$courtesy_runner)
})

test_that("migration is idempotent", {
  once  <- coerce_ruleset_config(list(run_cap_per_inning = 5L,
             mercy_rule = list(differential = 10L, after_inning = 4L),
             batting_gender_rule = list(type = "every_other")))
  twice <- coerce_ruleset_config(once)
  expect_identical(once, twice)
})

test_that("validation rejects the new enums", {
  bad <- default_ruleset_config()
  bad$home_run_rule$over_limit_result <- "explode"
  expect_false(validate_ruleset_config(bad)$ok)

  bad2 <- default_ruleset_config()
  bad2$pinch_runner$eligibility <- "whoever"
  expect_false(validate_ruleset_config(bad2)$ok)

  bad3 <- default_ruleset_config()
  bad3$batting_gender_rule$type <- "max_consecutive_males"   # requires n
  bad3$batting_gender_rule$n <- NA_integer_
  expect_false(validate_ruleset_config(bad3)$ok)

  bad4 <- default_ruleset_config()
  bad4$mercy_rule$tiers <- list(list(after_inning = 3L))     # missing differential
  expect_false(validate_ruleset_config(bad4)$ok)
})
