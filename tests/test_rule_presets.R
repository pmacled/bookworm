library(testthat)
for (f in c("app_config.R","rules_engine.R","rule_presets.R"))
  source(file.path("R", f))

test_that("every preset is valid and round-trips through coercion", {
  for (id in names(RULE_PRESETS)) {
    cfg <- preset_ruleset(id)
    v <- validate_ruleset_config(cfg)
    expect_true(v$ok, info = paste(id, ":", paste(v$errors, collapse = "; ")))
    expect_identical(cfg, coerce_ruleset_config(cfg), info = paste(id, "is not idempotent"))
    expect_equal(cfg$preset, id)
  }
})

test_that("every preset has a label and a description", {
  for (id in names(RULE_PRESETS)) {
    expect_true(nzchar(RULE_PRESETS[[id]]$label), info = id)
    expect_true(nzchar(RULE_PRESETS[[id]]$description), info = id)
  }
})

test_that("anything_goes is the default and matches default_ruleset_config", {
  expect_equal(names(RULE_PRESETS)[1], "anything_goes")
  d <- default_ruleset_config()
  a <- preset_ruleset("anything_goes")
  expect_equal(a$starting_count, d$starting_count)
  expect_equal(a$foul_out_rule, d$foul_out_rule)
  expect_equal(a$batting_gender_rule$type, "none")
})

test_that("the standard presets differ in the four ways that matter", {
  bb <- preset_ruleset("standard_baseball")
  sp <- preset_ruleset("standard_slowpitch")
  fp <- preset_ruleset("standard_fastpitch")
  expect_equal(bb$innings, 9L);  expect_equal(sp$innings, 7L)
  expect_equal(bb$fielding$fielder_count, 9L)
  expect_equal(sp$fielding$fielder_count, 10L)
  expect_equal(bb$foul_out_rule, "unlimited")
  expect_equal(sp$foul_out_rule, "out")
  expect_length(bb$mercy_rule$tiers, 0L)
  expect_length(sp$mercy_rule$tiers, 3L)
  expect_equal(fp$pinch_runner$allowed_for, "pitcher_catcher")
})

test_that("the GameOn presets reuse STANDARD_COED_FIELDING", {
  for (id in c("gameon_summer", "gameon_spring")) {
    cfg <- preset_ruleset(id)
    expect_equal(cfg$fielding$min_females, STANDARD_COED_FIELDING$min_females)
    expect_equal(cfg$fielding$max_males,   STANDARD_COED_FIELDING$max_males)
    expect_length(cfg$fielding$tiers, length(STANDARD_COED_FIELDING$tiers))
    expect_equal(cfg$fielding$fielder_count, 10L)
    expect_equal(cfg$batting_gender_rule$type, "max_consecutive_males")
    expect_equal(cfg$batting_gender_rule$n, 2L)
    expect_equal(cfg$home_run_rule$over_fence_limit, 3L)
    expect_equal(cfg$home_run_rule$over_limit_result, "out")
    expect_equal(cfg$pinch_runner$max_per_inning, 1L)
    expect_equal(cfg$pinch_runner$eligibility, "same_gender")
    expect_equal(cfg$innings, 7L)
  }
})

test_that("GameOn Summer and Spring differ only in the starting count", {
  su <- preset_ruleset("gameon_summer"); sp <- preset_ruleset("gameon_spring")
  expect_equal(su$starting_count, list(balls = 0L, strikes = 0L))
  expect_equal(sp$starting_count, list(balls = 1L, strikes = 1L))
  su$starting_count <- NULL; sp$starting_count <- NULL
  su$preset <- NULL; sp$preset <- NULL
  expect_identical(su, sp)
})

test_that("ruleset_is_genderless separates the genderless presets from GameOn", {
  for (id in c("anything_goes", "standard_baseball", "standard_slowpitch",
               "standard_fastpitch"))
    expect_true(ruleset_is_genderless(preset_ruleset(id)), info = id)
  for (id in c("gameon_summer", "gameon_spring"))
    expect_false(ruleset_is_genderless(preset_ruleset(id)), info = id)
})

test_that("a genderless ruleset stops being genderless when a gender rule is added", {
  cfg <- preset_ruleset("anything_goes")
  expect_true(ruleset_is_genderless(cfg))
  cfg$fielding$min_females <- 2L
  expect_false(ruleset_is_genderless(cfg))
})

test_that("preset_ruleset rejects an unknown id", {
  expect_error(preset_ruleset("nope"), "unknown preset")
})

test_that("preset_choices maps labels to ids", {
  ch <- preset_choices()
  expect_equal(unname(ch[["Anything Goes"]]), "anything_goes")
  expect_length(ch, length(RULE_PRESETS))
})
