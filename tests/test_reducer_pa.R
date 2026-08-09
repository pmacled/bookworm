library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))
mk_lineup <- function(prefix, genders = c("M","F","M","F"))
  lapply(seq_along(genders), function(i)
    make_player(paste0(prefix, i), paste(prefix, i), genders[i], i, i, i))
start_evt <- function(ruleset = default_ruleset_config()) new_event("game_start", list(
  ruleset = ruleset, first_bat = "away",
  home = list(team_id="H", name="Home", lineup = mk_lineup("h")),
  away = list(team_id="A", name="Away", lineup = mk_lineup("a"))), seq = 1L)
pa <- function(batter, outcome, reached, rbi = 0L, outs = 0L, advances = list(), seq = 2L)
  new_event("plate_appearance", list(team="away", batter_id=batter, outcome=outcome,
    reached=reached, rbi=rbi, outs_on_play=outs, advances=advances), seq = seq)

test_that("a single puts the batter on first", {
  s <- fold_events(list(start_evt(), pa("a1", "1B", 1L)))
  expect_equal(s$bases$first, "a1")
  expect_equal(s$outs, 0L)
})

test_that("home run with a runner on first scores two and adds RBIs", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", 1L, seq = 2L),
    pa("a2", "HR", 4L, rbi = 2L, seq = 3L,
       advances = list(make_advance("a1", 1L, 4L, scored = TRUE),
                       make_advance("a2", 0L, 4L, scored = TRUE)))
  ))
  expect_equal(s$score$away, 2L)
  expect_equal(s$runs_this_half, 2L)
  expect_true(is.na(s$bases$first))
})

test_that("pa_log grows and records bases_after", {
  s <- fold_events(list(start_evt(), pa("a1", "1B", 1L)))
  expect_equal(length(s$pa_log), 1L)
  expect_equal(s$pa_log[[1]]$bases_after$first, "a1")
})

test_that("suggest_advances forces the batter to first on a walk", {
  s <- fold_events(list(start_evt()))
  adv <- suggest_advances(s, "BB")
  expect_true(any(vapply(adv, function(a) a$to == 1L, logical(1))))
})

test_that("suggest_advances scores everyone on an inside-the-park home run, same as HR", {
  # ITPHR must bump runners 4 bases like HR. A reducer that doesn't recognize
  # the code falls through the switch's default of 0L and returns no advances
  # at all (as if nothing happened), which this distinguishes from the fix.
  s <- fold_events(list(start_evt(), pa("a1", "1B", 1L)))
  adv <- suggest_advances(s, "ITPHR")
  runner <- Filter(function(a) a$runner_id == "a1", adv)
  expect_equal(length(runner), 1L)
  expect_equal(runner[[1]]$to, 4L)
  expect_true(runner[[1]]$scored)
})

test_that("at the shipping default, a cap-crossing play ends the half mid-inning (cap_ends_half)", {
  rs <- coerce_ruleset_config(list(run_cap = list(per_inning = 3L)))
  expect_true(rs$run_cap$cap_ends_half)   # shipping default
  # A grand slam (4 runs) against a cap of 3, with the bases loaded from the
  # start (no prior plays needed -- advances alone carry the runners home).
  s <- fold_events(list(start_evt(rs),
    pa("a4", "HR", 4L, rbi = 4L,
       advances = list(make_advance("a1", 1L, 4L, scored = TRUE),
                       make_advance("a2", 2L, 4L, scored = TRUE),
                       make_advance("a3", 3L, 4L, scored = TRUE)))))
  expect_equal(s$score$away, 4L)          # the play in progress counted in full (grand slam)
  expect_equal(s$half, "bottom")          # ...and the half ended even though outs = 0
  expect_equal(s$outs, 0L)
})

test_that("at the shipping default, once the cap is reached the next play in the half scores zero", {
  rs <- coerce_ruleset_config(list(run_cap = list(per_inning = 3L, cap_ends_half = FALSE)))
  s <- fold_events(list(start_evt(rs),
    pa("a4", "HR", 4L, rbi = 4L, seq = 2L,
       advances = list(make_advance("a1", 1L, 4L, scored = TRUE),
                       make_advance("a2", 2L, 4L, scored = TRUE),
                       make_advance("a3", 3L, 4L, scored = TRUE))),
    pa("a2", "HR", 4L, seq = 3L)))         # a second, unrelated solo shot
  expect_equal(s$score$away, 4L)          # first play (grand slam) counted in full;
                                           # the second play's run is stopped by the cap
  expect_equal(s$half, "top")             # cap_ends_half = FALSE: half kept going
  expect_true(any(vapply(s$warnings, function(w) identical(w$code, "run_cap"), logical(1))))
})
