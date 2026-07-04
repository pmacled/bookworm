library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))
mk_lineup <- function(prefix, genders = c("M","F","M","F"))
  lapply(seq_along(genders), function(i)
    make_player(paste0(prefix, i), paste(prefix, i), genders[i], i, i, i))
start_evt <- function() new_event("game_start", list(
  ruleset = default_ruleset_config(), first_bat = "away",
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
