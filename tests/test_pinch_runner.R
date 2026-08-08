library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R",
            "rule_pinch_runner.R"))
  source(file.path("R", f))

pr_cfg <- function(...) coerce_ruleset_config(list(pinch_runner = list(...)))

p <- function(id, gender = "M", pos = "SS") make_player(id, id, gender, 1L, 1L, pos)

base_state <- function(log = list(), pa_log = list())
  list(inning = 3L, half = "top", batting_team = "away",
       lineups = list(away = list(), home = list()),
       pinch_runner_log = log, pa_log = pa_log)

test_that("unlimited defaults allow anything", {
  r <- evaluate_pinch_runner(pr_cfg(), base_state(), p("r1"), p("r2", "F"))
  expect_true(r$ok)
})

test_that("max_per_inning counts only this inning and half", {
  cfg <- pr_cfg(max_per_inning = 1L)
  used_this <- list(list(inning = 3L, half = "top", team = "away",
                         out_player_id = "x", in_player_id = "y"))
  expect_false(evaluate_pinch_runner(cfg, base_state(used_this), p("r1"), p("r2"))$ok)

  used_other <- list(list(inning = 2L, half = "top", team = "away",
                          out_player_id = "x", in_player_id = "y"))
  expect_true(evaluate_pinch_runner(cfg, base_state(used_other), p("r1"), p("r2"))$ok)

  # Same inning number, other half of it: must not count against this half's
  # limit. A implementation that matches on inning alone (ignoring half) would
  # wrongly block this.
  used_other_half <- list(list(inning = 3L, half = "bottom", team = "away",
                               out_player_id = "x", in_player_id = "y"))
  expect_true(evaluate_pinch_runner(cfg, base_state(used_other_half), p("r1"), p("r2"))$ok)
})

test_that("max_per_game counts every inning for this team", {
  cfg <- pr_cfg(max_per_game = 2L)
  used <- lapply(1:2, function(i) list(inning = i, half = "top", team = "away",
                                       out_player_id = "x", in_player_id = "y"))
  expect_false(evaluate_pinch_runner(cfg, base_state(used), p("r1"), p("r2"))$ok)

  # The other team's substitutions must not count against this team's cap. A
  # count that ignores `team` would wrongly block this (2 "home" entries + a
  # limit of 2, even though "away" has used none).
  used_other_team <- lapply(1:2, function(i) list(inning = i, half = "top", team = "home",
                                                   out_player_id = "x", in_player_id = "y"))
  expect_true(evaluate_pinch_runner(cfg, base_state(used_other_team), p("r1"), p("r2"))$ok)
})

test_that("max_per_game = 0 forbids pinch runners entirely", {
  expect_false(evaluate_pinch_runner(pr_cfg(max_per_game = 0L), base_state(),
                                     p("r1"), p("r2"))$ok)
})

test_that("max_per_player_per_game counts appearances by the incoming runner", {
  cfg <- pr_cfg(max_per_player_per_game = 1L)
  used <- list(list(inning = 1L, half = "top", team = "away",
                    out_player_id = "x", in_player_id = "r2"))
  expect_false(evaluate_pinch_runner(cfg, base_state(used), p("r1"), p("r2"))$ok)
  expect_true(evaluate_pinch_runner(cfg, base_state(used), p("r1"), p("r3"))$ok)
})

test_that("same_gender eligibility", {
  cfg <- pr_cfg(eligibility = "same_gender")
  expect_true(evaluate_pinch_runner(cfg, base_state(), p("r1", "F"), p("r2", "F"))$ok)
  expect_false(evaluate_pinch_runner(cfg, base_state(), p("r1", "F"), p("r2", "M"))$ok)
})

test_that("last_out eligibility requires the most recent out", {
  cfg <- pr_cfg(eligibility = "last_out")
  pal <- list(
    list(team = "away", batter_id = "o1", outcome = "K",  outs_on_play = 1L),
    list(team = "away", batter_id = "o2", outcome = "GO", outs_on_play = 1L),
    list(team = "away", batter_id = "o3", outcome = "1B", outs_on_play = 0L))
  st <- base_state(pa_log = pal)
  expect_true(evaluate_pinch_runner(cfg, st, p("r1"), p("o2"))$ok)
  expect_false(evaluate_pinch_runner(cfg, st, p("r1"), p("o1"))$ok)
})

test_that("last_same_gender_out looks past outs by the other gender", {
  cfg <- pr_cfg(eligibility = "last_same_gender_out")
  st <- base_state(pa_log = list(
    list(team = "away", batter_id = "f1", outcome = "K",  outs_on_play = 1L),
    list(team = "away", batter_id = "m1", outcome = "GO", outs_on_play = 1L)))
  st$lineups$away <- list(p("f1", "F"), p("m1", "M"))
  # Running for a female: the last female out is f1, not the more recent male out m1.
  expect_true(evaluate_pinch_runner(cfg, st, p("f2", "F"), p("f1", "F"))$ok)
  expect_false(evaluate_pinch_runner(cfg, st, p("f2", "F"), p("m1", "M"))$ok)
})

test_that("allowed_for = pitcher_catcher restricts who may be run for", {
  cfg <- pr_cfg(allowed_for = "pitcher_catcher")
  expect_true(evaluate_pinch_runner(cfg, base_state(),  p("r1", "M", "P"), p("r2"))$ok)
  expect_true(evaluate_pinch_runner(cfg, base_state(),  p("r1", "M", "C"), p("r2"))$ok)
  expect_false(evaluate_pinch_runner(cfg, base_state(), p("r1", "M", "SS"), p("r2"))$ok)
})

test_that("multiple failures are all reported", {
  cfg <- pr_cfg(max_per_game = 0L, eligibility = "same_gender")
  r <- evaluate_pinch_runner(cfg, base_state(), p("r1", "F"), p("r2", "M"))
  expect_false(r$ok)
  expect_gte(length(r$errors), 2L)
})

test_that("the reducer records a pinch runner in pinch_runner_log", {
  st <- initial_game_state()
  st$bases$first <- "a1"
  st$batting_team <- "away"
  evt <- new_event("substitution", list(team = "away", kind = "courtesy_runner",
    out_player_id = "a1", in_player = p("a9")))
  st2 <- apply_substitution(st, evt)
  expect_equal(st2$bases$first, "a9")
  expect_length(st2$pinch_runner_log, 1L)
  expect_equal(st2$pinch_runner_log[[1]]$out_player_id, "a1")
  expect_equal(st2$pinch_runner_log[[1]]$in_player_id, "a9")
  expect_equal(st2$pinch_runner_log[[1]]$inning, st$inning)
})
