library(testthat)
for (f in c("app_config.R", "rules_engine.R", "rule_presets.R")) {
  source(file.path("R", f))
}

test_that("every preset is valid and round-trips through coercion", {
  for (id in names(RULE_PRESETS)) {
    cfg <- preset_ruleset(id)
    v <- validate_ruleset_config(cfg)
    expect_true(v$ok, info = paste(id, ":", paste(v$errors, collapse = "; ")))
    expect_identical(
      cfg,
      coerce_ruleset_config(cfg),
      info = paste(id, "is not idempotent")
    )
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
  expect_equal(bb$innings, 9L)
  expect_equal(sp$innings, 7L)
  expect_equal(bb$fielding$fielder_count, 9L)
  expect_equal(sp$fielding$fielder_count, 10L)
  expect_equal(bb$foul_out_rule, "unlimited")
  expect_equal(sp$foul_out_rule, "out")
  expect_length(bb$mercy_rule$tiers, 0L)
  expect_length(sp$mercy_rule$tiers, 3L)
  expect_equal(fp$pinch_runner$allowed_for, "pitcher_catcher")
})

test_that("preset configs pin every value their description promises", {
  # Each preset's `description` is user-facing copy the app shows verbatim; if the
  # config value it promises drifts (e.g. a typo'd key like `batting_sizee` that
  # .merge_ruleset silently treats as an unrelated extra field, leaving the real
  # `batting_size` at its NA default), nothing else in this file catches it because
  # no other test reads these specific fields.
  bb <- preset_ruleset("standard_baseball")
  fp <- preset_ruleset("standard_fastpitch")

  # "9 batters" (Standard Baseball)
  expect_equal(bb$batting_size, 9L)
  # "9 fielders, 9 batters" (Standard Fastpitch)
  expect_equal(fp$batting_size, 9L)
  expect_equal(fp$fielding$fielder_count, 9L)
  # "USA Softball mercy schedule" (Standard Fastpitch)
  expect_equal(fp$mercy_rule$tiers, .USA_MERCY)
})

test_that("the GameOn presets reuse STANDARD_COED_FIELDING", {
  for (id in c("gameon_summer", "gameon_spring")) {
    cfg <- preset_ruleset(id)
    expect_equal(cfg$fielding$min_females, STANDARD_COED_FIELDING$min_females)
    expect_equal(cfg$fielding$max_males, STANDARD_COED_FIELDING$max_males)
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
  su <- preset_ruleset("gameon_summer")
  sp <- preset_ruleset("gameon_spring")
  expect_equal(su$starting_count, list(balls = 0L, strikes = 0L))
  expect_equal(sp$starting_count, list(balls = 1L, strikes = 1L))
  su$starting_count <- NULL
  sp$starting_count <- NULL
  su$preset <- NULL
  sp$preset <- NULL
  expect_identical(su, sp)
})

test_that("ruleset_is_genderless separates the genderless presets from GameOn", {
  for (id in c(
    "anything_goes",
    "standard_baseball",
    "standard_slowpitch",
    "standard_fastpitch"
  )) {
    expect_true(ruleset_is_genderless(preset_ruleset(id)), info = id)
  }
  for (id in c("gameon_summer", "gameon_spring")) {
    expect_false(ruleset_is_genderless(preset_ruleset(id)), info = id)
  }
})

test_that("ruleset_is_genderless flips on each gender-referencing field independently", {
  # Each sub-test starts from a fresh, genderless baseline and changes exactly one
  # field, so a predicate missing that field's check cannot be masked by any other
  # field also differing (the trap the standard/GameOn presets don't expose, since
  # every gender field they set differs from the baseline in combination).
  base <- preset_ruleset("anything_goes")
  expect_true(ruleset_is_genderless(base), info = "baseline")

  cfg <- base
  cfg$batting_gender_rule <- list(type = "max_consecutive_males", n = 2L)
  expect_false(ruleset_is_genderless(cfg), info = "batting_gender_rule$type")

  cfg <- base
  cfg$male_walk_rule <- "two_bases_then_female"
  expect_false(ruleset_is_genderless(cfg), info = "male_walk_rule")

  cfg <- base
  cfg$fielding$min_females <- 2L
  expect_false(ruleset_is_genderless(cfg), info = "fielding$min_females")

  cfg <- base
  cfg$fielding$max_males <- 6L
  expect_false(ruleset_is_genderless(cfg), info = "fielding$max_males")

  cfg <- base
  cfg$fielding$tiers <- STANDARD_COED_FIELDING$tiers
  expect_false(ruleset_is_genderless(cfg), info = "fielding$tiers")

  cfg <- base
  cfg$home_run_rule$limit_by_gender <- list(F = 3L)
  expect_false(
    ruleset_is_genderless(cfg),
    info = "home_run_rule$limit_by_gender"
  )

  cfg <- base
  cfg$pinch_runner$eligibility <- "same_gender"
  expect_false(
    ruleset_is_genderless(cfg),
    info = "pinch_runner$eligibility = same_gender"
  )

  cfg <- base
  cfg$pinch_runner$eligibility <- "last_same_gender_out"
  expect_false(
    ruleset_is_genderless(cfg),
    info = "pinch_runner$eligibility = last_same_gender_out"
  )
})

test_that("preset_ruleset rejects an unknown id", {
  expect_error(preset_ruleset("nope"), "unknown preset")
})

test_that("preset_choices maps labels to ids", {
  ch <- preset_choices()
  expect_equal(unname(ch[["Anything Goes"]]), "anything_goes")
  expect_length(ch, length(RULE_PRESETS))
})
