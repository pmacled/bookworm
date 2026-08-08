library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

# Most of the brief's run-cap cases already exist (test_rules_eval.R,
# test_reducer_pa.R, test_reducer_halfruns.R) -- see task-3-report.md for the
# coverage map. This file adds only what's missing: cases where "clamp to
# remaining room" and "clamp to the cap" diverge, an exact-boundary cap hit
# reached over multiple plays, clamp semantics under cap_ends_half = FALSE,
# and the notice-survives-the-half-ending guard on the plate_appearance path.

mk <- function(prefix, n = 4L) lapply(seq_len(n), function(i)
  make_player(paste0(prefix, i), paste(prefix, i), c("M","F")[(i %% 2) + 1L], i, i, i))

start_with <- function(cfg) new_event("game_start", list(
  ruleset = cfg, first_bat = "away",
  home = list(team_id="H", name="Home", lineup = mk("h")),
  away = list(team_id="A", name="Away", lineup = mk("a"))), seq = 1L)

adv <- function(id, from, to, scored = FALSE) make_advance(id, from, to, scored = scored)

pa <- function(batter, outcome, reached, advances = list(), outs = 0L, seq = 2L)
  new_event("plate_appearance", list(team = "away", batter_id = batter, outcome = outcome,
    reached = reached, rbi = 0L, outs_on_play = outs, advances = advances), seq = seq)

cap_cfg <- function(...) coerce_ruleset_config(utils::modifyList(
  list(innings = 7L, run_cap = list(per_inning = 5L, open_last_inning = TRUE,
                                    same_play_runs_count = TRUE, cap_ends_half = TRUE)),
  list(...)))

test_that("clamp branch truncates to remaining room, not to the cap, when runs_before is nonzero", {
  # test_rules_eval.R's clamp-branch coverage only uses runs_before = 0, where
  # "clamp to remaining room under the cap" and "clamp to the cap" happen to
  # produce the same number. Here runs_before = 4 and cap = 5: remaining room
  # is 1, so a wrong "truncate to the cap value" implementation would return
  # 5 (or pass runs_on_play through unclamped at 4) instead of 1.
  cfg <- cap_cfg(run_cap = list(per_inning = 5L, open_last_inning = TRUE,
                                same_play_runs_count = FALSE, cap_ends_half = TRUE))
  r <- apply_run_cap(cfg, runs_before = 4L, runs_on_play = 4L, inning = 1L)
  expect_equal(r$runs, 1L)
  expect_true(r$cap_hit)
})

test_that("the cap does not apply in an open last inning, and cap_hit is reported FALSE", {
  # test_rules_eval.R:67-72 exercises the open-last-inning bypass but only
  # checks $runs. This also locks down $cap_hit, which a naive `total >= cap`
  # evaluated before (or despite) the open-last-inning bypass would get wrong
  # (0 + ... well past cap here) -- the bypass must report cap_hit = FALSE too.
  cfg <- cap_cfg()
  r <- apply_run_cap(cfg, runs_before = 4L, runs_on_play = 4L, inning = 7L)
  expect_equal(r$runs, 4L)
  expect_false(r$cap_hit)
})

test_that("reaching the cap exactly (not exceeding it) over two separate plays still ends the half", {
  # test_reducer_pa.R's cap_ends_half coverage always exceeds the cap in one
  # play (a grand slam against a lower cap: total > cap). This drives the
  # total to land exactly on the cap via two ordinary single-run plays --
  # total == cap -- which would slip through a `total > cap` off-by-one
  # instead of the correct `total >= cap`.
  cfg <- cap_cfg(run_cap = list(per_inning = 2L, open_last_inning = FALSE,
                                same_play_runs_count = TRUE, cap_ends_half = TRUE))
  s <- fold_events(list(
    start_with(cfg),
    pa("a1", "HR", 4L, list(adv("a1", 0L, 4L, scored = TRUE)), seq = 2L),
    pa("a2", "HR", 4L, list(adv("a2", 0L, 4L, scored = TRUE)), seq = 3L)))
  expect_equal(s$score$away, 2L)
  expect_equal(s$half, "bottom")        # half ended on the cap, not on three outs
  expect_equal(s$outs, 0L)
  expect_equal(s$batting_team, "home")
})

test_that("cap_ends_half = FALSE keeps the half alive under clamp semantics across repeated at-cap plays", {
  # Distinct from test_reducer_pa.R's cap_ends_half = FALSE case, which uses
  # same_play_runs_count = TRUE (one play blows past the cap, the next is
  # clamped to zero). This exercises the *clamp* branch (same_play_runs_count
  # = FALSE) across three plays: a1 (1 run, room 2->1 left), a2 (1 run, room
  # exactly used up, cap now hit), a3 (1 run, but runs_before already >= cap).
  # A wrong `min(runs_on_play, cap)` clamp (instead of remaining room) would
  # let a2's run double-count against the cap and mis-total the score.
  cfg <- cap_cfg(run_cap = list(per_inning = 2L, open_last_inning = FALSE,
                                same_play_runs_count = FALSE, cap_ends_half = FALSE))
  s <- fold_events(list(
    start_with(cfg),
    pa("a1", "HR", 4L, list(adv("a1", 0L, 4L, scored = TRUE)), seq = 2L),
    pa("a2", "HR", 4L, list(adv("a2", 0L, 4L, scored = TRUE)), seq = 3L),
    pa("a3", "HR", 4L, list(adv("a3", 0L, 4L, scored = TRUE)), seq = 4L)))
  expect_equal(s$score$away, 2L)        # third run discarded
  expect_equal(s$half, "top")           # still batting
})

test_that("the run-cap notice survives the half ending via cap_ends_half on the plate_appearance path", {
  # test_reducer_halfruns.R already proves the notice survives advance_half()
  # on the half_runs path (which always ends the half unconditionally). The
  # plate_appearance path is different: cap_hit_last_play is set *before*
  # advance_half() is called (game_reducer.R's cap_ends branch), so this
  # guards against a future advance_half() change (e.g. folding in a blanket
  # state reset) wiping the flag out from under a half that just ended on
  # the cap -- exactly the ordering bug flagged in Task 1.
  cfg <- cap_cfg(run_cap = list(per_inning = 1L, open_last_inning = FALSE,
                                same_play_runs_count = TRUE, cap_ends_half = TRUE))
  s <- fold_events(list(
    start_with(cfg),
    pa("a1", "HR", 4L, list(adv("a1", 0L, 4L, scored = TRUE)), seq = 2L)))
  expect_equal(s$half, "bottom")   # confirm the half really did end on the cap
  codes <- vapply(s$warnings, function(w) w$code, character(1))
  expect_true("run_cap" %in% codes)
})
