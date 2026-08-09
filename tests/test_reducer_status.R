library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

# `status` is DERIVED from the folded state on every event, not latched once and
# left. Latching made "final" a one-way trap: tracking_module refuses all input
# once final, and nothing ever set it back, so an Undo (or a differential that
# shrinks again) could not reopen the game.

mk <- function(prefix, n = 3L) lapply(seq_len(n), function(i)
  make_player(paste0(prefix, i), paste0(prefix, i), c("M","F")[(i %% 2) + 1L], i, i, i))

start_runonly <- function(cfg) new_event("game_start", list(
  ruleset = cfg, first_bat = "away",
  home = list(team_id = "H", name = "Home", lineup = list()),
  away = list(team_id = "A", name = "Away", lineup = list())), seq = 1L)

hr_evt <- function(runs, seq) new_event("half_runs", list(runs = as.integer(runs)), seq = seq)

mercy_after_3 <- function() coerce_ruleset_config(list(
  innings = 7L, mercy_rule = list(tiers = list(list(after_inning = 3L, differential = 20L)))))

# away puts up 20 in the top of the 1st and both teams are then blanked. The
# 20-after-3 tier must hold off until three innings are actually complete.
mercy_run <- function() {
  cfg <- mercy_after_3()
  c(list(start_runonly(cfg)), lapply(2:7, function(i) hr_evt(if (i == 2L) 20L else 0L, i)))
}

test_that("mercy does not fire in the top of the 3rd for a 3-inning tier", {
  ev <- mercy_run()
  s <- fold_events(ev[1:5])                 # start + top1, bot1, top2, bot2
  expect_equal(s$inning, 3L); expect_equal(s$half, "top")
  expect_equal(abs(s$score$away - s$score$home), 20L)
  expect_equal(s$status, "in_progress")     # only TWO innings are complete
})

test_that("mercy does not fire at the end of the top of the 3rd either", {
  s <- fold_events(mercy_run()[1:6])        # ... + top3
  expect_equal(s$inning, 3L); expect_equal(s$half, "bottom")
  expect_equal(s$status, "in_progress")
})

test_that("mercy fires at the end of the bottom of the 3rd", {
  s <- fold_events(mercy_run())             # ... + bot3
  expect_equal(s$inning, 4L); expect_equal(s$half, "top")   # three innings complete
  expect_equal(s$status, "final")
  expect_true(any(vapply(s$warnings, function(w) identical(w$code, "final"), logical(1))))
})

test_that("undoing the event that triggered mercy un-finals the game", {
  ev <- mercy_run()
  expect_equal(fold_events(ev)$status, "final")
  # Undo drops the last event and re-folds; the game must reopen for scoring.
  s <- fold_events(ev[-length(ev)])
  expect_equal(s$status, "in_progress")
  expect_false(any(vapply(s$warnings, function(w) identical(w$code, "final"), logical(1))))
})

test_that("a shrinking differential un-finals the game", {
  # The trailing team catches up after mercy had already been satisfied. With a
  # latched status this game stayed final forever; derived, it simply reopens.
  cfg <- coerce_ruleset_config(list(
    innings = 7L, mercy_rule = list(tiers = list(list(after_inning = 1L, differential = 5L)))))
  ev <- list(start_runonly(cfg), hr_evt(5L, 2L), hr_evt(0L, 3L))
  expect_equal(fold_events(ev)$status, "final")     # 5-0 after one complete inning
  ev2 <- c(ev, list(hr_evt(0L, 4L), hr_evt(5L, 5L)))  # home ties it in the bottom of the 2nd
  s <- fold_events(ev2)
  expect_equal(s$score$home, 5L)
  expect_equal(s$status, "in_progress")
})

test_that("mercy is not evaluated in the middle of a half-inning", {
  cfg <- coerce_ruleset_config(list(
    innings = 7L, mercy_rule = list(tiers = list(list(after_inning = 1L, differential = 2L)))))
  start <- new_event("game_start", list(ruleset = cfg, first_bat = "away",
    home = list(team_id = "H", name = "Home", lineup = mk("h")),
    away = list(team_id = "A", name = "Away", lineup = mk("a"))), seq = 1L)
  hr <- function(id, seq) new_event("plate_appearance", list(team = "away",
    batter_id = id, outcome = "HR", reached = 4L, rbi = 1L, outs_on_play = 0L,
    advances = list(make_advance(id, 0L, 4L, scored = TRUE))), seq = seq)

  # Scoreless 1st, then two home runs in the top of the 2nd.
  base <- list(start, new_event("inning_end", list(), seq = 2L),
               new_event("inning_end", list(), seq = 3L))
  mid <- fold_events(c(base, list(hr("a1", 4L), hr("a2", 5L))))
  expect_equal(mid$inning, 2L); expect_equal(mid$half, "top")
  expect_equal(mid$score$away, 2L)
  expect_equal(mid$status, "in_progress")     # mid-half: the rule has not been reached yet

  ended <- apply_event(mid, new_event("inning_end", list(), seq = 6L))
  expect_equal(ended$half, "bottom")
  expect_equal(ended$status, "final")         # ... and it fires the moment the half ends
})

test_that("regulation completion still ends the game", {
  cfg <- coerce_ruleset_config(list(innings = 2L))
  ev <- c(list(start_runonly(cfg)),
          lapply(2:4, function(i) hr_evt(if (i == 2L) 1L else 0L, i)))
  expect_equal(fold_events(ev)$status, "in_progress")   # bottom of the 2nd still to play
  ev <- c(ev, list(hr_evt(0L, 5L)))
  s <- fold_events(ev)
  expect_equal(s$inning, 3L)                            # past the last inning
  expect_equal(s$status, "final")
})
