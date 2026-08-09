library(testthat)
source(file.path("R", "app_config.R"))
source(file.path("R", "rules_engine.R"))

test_that("default config is valid", {
  cfg <- default_ruleset_config()
  expect_equal(cfg$starting_count$balls, 0L)
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("coerce merges partial over defaults", {
  cfg <- coerce_ruleset_config(list(
    innings = 5,
    starting_count = list(balls = 0, strikes = 2)
  ))
  expect_equal(cfg$innings, 5L)
  expect_equal(cfg$starting_count$strikes, 2L)
  expect_equal(cfg$foul_out_rule, "unlimited") # untouched default
})

test_that("validation rejects bad starting count and unknown enums", {
  bad <- default_ruleset_config()
  bad$starting_count$strikes <- 3L
  v <- validate_ruleset_config(bad)
  expect_false(v$ok)
  expect_equal(v$errors, "starting strikes must be 0-2")

  bad2 <- default_ruleset_config()
  bad2$foul_out_rule <- "explode"
  v2 <- validate_ruleset_config(bad2)
  expect_false(v2$ok)
  expect_equal(v2$errors, "invalid foul_out_rule")

  bad3 <- default_ruleset_config()
  bad3$batting_gender_rule$type <- "every_n"
  bad3$batting_gender_rule$n <- NA_integer_ # every_n requires n
  v3 <- validate_ruleset_config(bad3)
  expect_false(v3$ok)
  # `every_n` is a *legacy* alias: reaching validation un-migrated means both the
  # enum check and the requires-n check must speak up, not just one of them.
  expect_equal(
    v3$errors,
    c("invalid batting_gender_rule type", "every_n batting rule requires n")
  )
})

# --- Finding 2: a cleared numericInput sends NA, and validation must SAY SO, not throw ---

test_that("a cleared starting count is rejected with a message, not an NA crash", {
  # `!is.numeric(NA_integer_) || NA_integer_ < 0` evaluates to NA, and `if (NA)`
  # is an error -- so validate_ruleset_config() itself used to blow up on the
  # single most reachable bad input there is: an emptied numeric box.
  for (fld in c("balls", "strikes")) {
    cfg <- default_ruleset_config()
    cfg$starting_count[[fld]] <- NA_integer_
    v <- validate_ruleset_config(cfg) # must not error
    expect_false(v$ok, info = fld)
    expect_match(v$errors, sprintf("starting %s", fld), all = FALSE)
  }
})

test_that("cleared innings is rejected with a message, not an NA crash", {
  cfg <- default_ruleset_config()
  cfg$innings <- NA_integer_
  v <- validate_ruleset_config(cfg)
  expect_false(v$ok)
  expect_equal(v$errors, "innings must be >= 1")
})

test_that("a zero-length numeric (a NULL input coerced by as.integer) is rejected, not crashed on", {
  cfg <- default_ruleset_config()
  cfg$innings <- integer(0)
  cfg$starting_count$balls <- integer(0)
  v <- validate_ruleset_config(cfg)
  expect_false(v$ok)
  expect_true(all(
    c("starting balls must be 0-3", "innings must be >= 1") %in% v$errors
  ))
})

test_that("a cleared batting_size is unlimited, not an error", {
  # batting_size is the one numeric where NA is a legitimate value (unlimited).
  cfg <- default_ruleset_config()
  cfg$batting_size <- NA_integer_
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("batting_size_rule defaults to \"max\" and rejects unknown values and exact-without-size", {
  expect_equal(default_ruleset_config()$batting_size_rule, "max")
  # Unknown values coerce back to "max".
  expect_equal(
    coerce_ruleset_config(list(batting_size_rule = "wat"))$batting_size_rule,
    "max"
  )
  # "exact" is only valid alongside a concrete batting_size.
  bad <- coerce_ruleset_config(list(batting_size_rule = "exact"))
  expect_false(validate_ruleset_config(bad)$ok)
  good <- coerce_ruleset_config(list(
    batting_size_rule = "exact",
    batting_size = 9L
  ))
  expect_true(validate_ruleset_config(good)$ok)
})

# --- Finding 8: .merge_ruleset() must not drop a key on an explicit NULL ---

test_that("an explicit NULL override keeps the default instead of deleting the key", {
  # `x[[v]] <- NULL` *removes* the element. Integer fields are accidentally
  # restored by coerce's trailing .as_int_or_na() lines; string and logical
  # fields have no such backstop, so the key simply vanished and validation
  # then threw "argument is of length zero".
  cfg <- coerce_ruleset_config(list(foul_out_rule = NULL))
  expect_equal(cfg$foul_out_rule, "unlimited")
  expect_true(validate_ruleset_config(cfg)$ok)

  cfg2 <- coerce_ruleset_config(list(
    male_walk_rule = NULL,
    short_lineup_auto_out = NULL,
    innings = NULL,
    home_run_rule = list(over_limit_result = NULL)
  ))
  expect_equal(cfg2$male_walk_rule, "none")
  expect_false(cfg2$short_lineup_auto_out)
  expect_equal(cfg2$innings, 7L)
  expect_equal(cfg2$home_run_rule$over_limit_result, "out")
  expect_true(validate_ruleset_config(cfg2)$ok)
})

test_that("batting_size defaults to unlimited (NA) and validates", {
  expect_true(is.na(default_ruleset_config()$batting_size))
  expect_equal(coerce_ruleset_config(list(batting_size = 10))$batting_size, 10L)
  expect_equal(
    coerce_ruleset_config(list(batting_size = 0))$batting_size,
    NA_integer_
  ) # 0 => unlimited
  bad <- default_ruleset_config()
  bad$batting_size <- -3L
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
  cfg <- coerce_ruleset_config(list(
    run_cap_per_inning = 5L,
    open_last_inning = FALSE
  ))
  expect_equal(cfg$run_cap$per_inning, 5L)
  expect_false(cfg$run_cap$open_last_inning)
  expect_true(cfg$run_cap$same_play_runs_count) # new field takes its default
  expect_null(cfg$run_cap_per_inning) # old key is gone, not shadowing
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("legacy scalar mercy keys migrate into a single tier", {
  cfg <- coerce_ruleset_config(list(
    mercy_rule = list(differential = 10L, after_inning = 4L)
  ))
  expect_length(cfg$mercy_rule$tiers, 1L)
  expect_equal(cfg$mercy_rule$tiers[[1]]$differential, 10L)
  expect_equal(cfg$mercy_rule$tiers[[1]]$after_inning, 4L)
  expect_null(cfg$mercy_rule$differential)
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("a legacy mercy differential with no after_inning defaults to inning 1", {
  cfg <- coerce_ruleset_config(list(mercy_rule = list(differential = 10L)))
  expect_equal(cfg$mercy_rule$tiers[[1]]$after_inning, 1L)
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("legacy batting-gender type names migrate", {
  a <- coerce_ruleset_config(list(
    batting_gender_rule = list(type = "no_two_males_consecutive")
  ))
  expect_equal(a$batting_gender_rule$type, "max_consecutive_males")
  expect_equal(a$batting_gender_rule$n, 1L)
  expect_true(validate_ruleset_config(a)$ok)

  b <- coerce_ruleset_config(list(
    batting_gender_rule = list(type = "every_other")
  ))
  expect_equal(b$batting_gender_rule$type, "max_consecutive_same_gender")
  expect_equal(b$batting_gender_rule$n, 1L)
  expect_true(validate_ruleset_config(b)$ok)

  c3 <- coerce_ruleset_config(list(
    batting_gender_rule = list(type = "every_n", n = 4L)
  ))
  expect_equal(c3$batting_gender_rule$type, "min_females_per_n")
  expect_equal(c3$batting_gender_rule$n, 4L)
  expect_true(validate_ruleset_config(c3)$ok)
})

test_that("the legacy courtesy_runner boolean migrates", {
  on <- coerce_ruleset_config(list(courtesy_runner = TRUE))
  expect_true(is.na(on$pinch_runner$max_per_game)) # unlimited
  expect_null(on$courtesy_runner)
  expect_true(validate_ruleset_config(on)$ok)

  off <- coerce_ruleset_config(list(courtesy_runner = FALSE))
  expect_equal(off$pinch_runner$max_per_game, 0L)
  expect_true(validate_ruleset_config(off)$ok)
})

test_that("courtesy_runner = TRUE does not blank out the rest of pinch_runner", {
  # Regression: migration's TRUE branch sets no key at all (pinch_runner stays
  # list()), and merging an *empty* unnamed list used to be treated the same as
  # merging a non-empty array -- wiping the default's eligibility/allowed_for
  # instead of leaving them untouched.
  cfg <- coerce_ruleset_config(list(courtesy_runner = TRUE))
  expect_equal(cfg$pinch_runner$eligibility, "anyone")
  expect_equal(cfg$pinch_runner$allowed_for, "anyone")
  expect_length(cfg$pinch_runner, 5L)
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("coercing an empty or NULL config returns full, valid defaults", {
  # Regression: an empty top-level list has no names either, so the same
  # "unnamed list -> replace wholesale" rule that correctly replaces `tiers`
  # was, before the fix, also nuking the *entire* default config to list().
  for (cfg in list(
    coerce_ruleset_config(list()),
    coerce_ruleset_config(NULL)
  )) {
    expect_equal(cfg$preset, "anything_goes")
    expect_equal(cfg$foul_out_rule, "unlimited")
    expect_equal(cfg$male_walk_rule, "none")
    expect_false(cfg$short_lineup_auto_out)
    expect_length(cfg, 14L)
    expect_true(validate_ruleset_config(cfg)$ok)
  }
})

test_that("an explicitly empty fielding override keeps the rest of the fielding defaults", {
  cfg <- coerce_ruleset_config(list(fielding = list()))
  expect_equal(cfg$fielding$min_females, 0L)
  expect_true(is.na(cfg$fielding$max_males))
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("migration is idempotent", {
  once <- coerce_ruleset_config(list(
    run_cap_per_inning = 5L,
    mercy_rule = list(differential = 10L, after_inning = 4L),
    batting_gender_rule = list(type = "every_other")
  ))
  twice <- coerce_ruleset_config(once)
  expect_identical(once, twice)
  expect_true(validate_ruleset_config(once)$ok)
})

test_that("validation rejects the new enums", {
  bad <- default_ruleset_config()
  bad$home_run_rule$over_limit_result <- "explode"
  expect_false(validate_ruleset_config(bad)$ok)

  bad2 <- default_ruleset_config()
  bad2$pinch_runner$eligibility <- "whoever"
  expect_false(validate_ruleset_config(bad2)$ok)

  bad3 <- default_ruleset_config()
  bad3$batting_gender_rule$type <- "max_consecutive_males" # requires n
  bad3$batting_gender_rule$n <- NA_integer_
  expect_false(validate_ruleset_config(bad3)$ok)

  bad4 <- default_ruleset_config()
  bad4$mercy_rule$tiers <- list(list(after_inning = 3L)) # missing differential
  expect_false(validate_ruleset_config(bad4)$ok)
})
