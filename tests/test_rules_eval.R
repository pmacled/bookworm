library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

test_that("no_two_males_consecutive flags a male after a male", {
  cfg <- coerce_ruleset_config(list(batting_gender_rule = list(type = "no_two_males_consecutive")))
  expect_false(next_batter_gender_ok(cfg, prev_genders = c("M"), next_gender = "M"))
  expect_true(next_batter_gender_ok(cfg, prev_genders = c("M"), next_gender = "F"))
})

test_that("next_batter_gender_ok fails open when the rule needs n but never got one", {
  # A rule type that requires n, with n left NA (e.g. a persisted event that
  # never re-validated), must not crash tail(x, NA + 1L).
  cfg <- coerce_ruleset_config(list(batting_gender_rule = list(type = "max_consecutive_males")))
  expect_true(is.na(cfg$batting_gender_rule$n))
  expect_true(next_batter_gender_ok(cfg, prev_genders = c("M"), next_gender = "M"))
})

test_that("next_batter_gender_ok is NA-safe: an unknown gender never satisfies the rule", {
  cfg <- coerce_ruleset_config(list(batting_gender_rule = list(type = "max_consecutive_males", n = 1L)))
  expect_true(next_batter_gender_ok(cfg, prev_genders = c(NA_character_), next_gender = "M"))
  expect_true(next_batter_gender_ok(cfg, prev_genders = c("M"), next_gender = NA_character_))

  cfg2 <- coerce_ruleset_config(list(batting_gender_rule = list(type = "min_females_per_n", n = 2L)))
  expect_false(next_batter_gender_ok(cfg2, prev_genders = c(NA_character_), next_gender = NA_character_))
})

test_that("run cap limits non-open innings", {
  cfg <- coerce_ruleset_config(list(run_cap_per_inning = 5L, innings = 7L,
                                    open_last_inning = TRUE))
  cfg$run_cap$same_play_runs_count <- FALSE   # legacy clamping behaviour
  expect_equal(apply_run_cap(cfg, runs_before = 0L, runs_on_play = 8L, inning = 3L)$runs, 5L)
  expect_equal(apply_run_cap(cfg, runs_before = 0L, runs_on_play = 8L, inning = 7L)$runs, 8L)
})

test_that("at the shipping default, a play that crosses the cap still counts in full (grand slam preserved)", {
  cfg <- coerce_ruleset_config(list(run_cap = list(per_inning = 5L)))
  expect_true(cfg$run_cap$same_play_runs_count)   # shipping default, not overridden
  cr <- apply_run_cap(cfg, runs_before = 3L, runs_on_play = 4L, inning = 1L)
  expect_equal(cr$runs, 4L)     # the play in progress counts in full, even past the cap
  expect_true(cr$cap_hit)
})

test_that("at the shipping default, once the cap is reached an earlier play, the next play scores zero", {
  cfg <- coerce_ruleset_config(list(run_cap = list(per_inning = 5L)))
  cr <- apply_run_cap(cfg, runs_before = 5L, runs_on_play = 2L, inning = 1L)
  expect_equal(cr$runs, 0L)     # the cap stops the *next* batter, not the one that hit it
  expect_true(cr$cap_hit)
})

test_that("apply_run_cap reports cap_hit accurately when the cap is not reached", {
  cfg <- coerce_ruleset_config(list(run_cap = list(per_inning = 5L)))
  cr <- apply_run_cap(cfg, runs_before = 1L, runs_on_play = 1L, inning = 1L)
  expect_equal(cr$runs, 1L)
  expect_false(cr$cap_hit)
})

test_that("mercy ends the game", {
  cfg <- coerce_ruleset_config(list(mercy_rule = list(differential = 10L, after_inning = 4L)))
  st <- list(inning = 5L, half = "top", score = list(home = 15L, away = 3L),
             ruleset = cfg, outs = 0L)
  expect_true(game_should_end(cfg, st))
})

test_that("reducer surfaces a gender-order warning for the batter due up", {
  cfg <- coerce_ruleset_config(list(batting_gender_rule = list(type = "no_two_males_consecutive")))
  lineup <- list(make_player("m1","M1","M",1L,1L,6L), make_player("m2","M2","M",2L,2L,4L))
  start <- new_event("game_start", list(ruleset = cfg, first_bat = "away",
    home = list(team_id="H", name="Home", lineup = lineup),
    away = list(team_id="A", name="Away", lineup = lineup)), seq = 1L)
  pa1 <- new_event("plate_appearance", list(team="away", batter_id="m1", outcome="1B",
    reached=1L, rbi=0L, outs_on_play=0L, advances=list()), seq = 2L)
  s <- fold_events(list(start, pa1))   # m2 (M) now due up after m1 (M)
  expect_true(any(vapply(s$warnings, function(x) identical(x$code, "batting_gender"), logical(1))))
})

test_that("mercy with differential but no after_inning does not crash and can end", {
  cfg <- coerce_ruleset_config(list(mercy_rule = list(differential = 10L)))  # after_inning stays NA
  st <- list(inning = 3L, half = "top", score = list(home = 15L, away = 3L),
             ruleset = cfg, outs = 0L)
  expect_true(game_should_end(cfg, st))   # must not error
})

test_that("game_should_end does not crash on a mercy tier missing after_inning entirely", {
  # A hand-built tier (as opposed to one that passed through coerce_ruleset_config)
  # may omit a key outright, so t$after_inning is NULL, not NA.
  cfg <- default_ruleset_config()
  cfg$mercy_rule$tiers <- list(list(differential = 10L))
  st <- list(inning = 1L, half = "top", score = list(home = 15L, away = 3L),
             ruleset = cfg, outs = 0L)
  expect_true(game_should_end(cfg, st))   # must not error; defaults after_inning to 1
})
