library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

start_evt <- function(ruleset = default_ruleset_config()) new_event("game_start", list(
  ruleset = ruleset, first_bat = "away",
  home = list(team_id="H", name="Home", lineup = list()),   # empty -> run-only
  away = list(team_id="A", name="Away", lineup = list())
), seq = 1L)

test_that("half_runs adds runs to the batting team and advances the half", {
  s <- fold_events(list(start_evt(),
    new_event("half_runs", list(team = "away", runs = 3L), seq = 2L)))
  expect_equal(s$score$away, 3L)
  expect_equal(s$half, "bottom")           # away's top half ended
  expect_equal(s$batting_team, "home")
  expect_equal(s$line_score$away, 3L)
})

test_that("half_runs respects the run cap", {
  rs <- default_ruleset_config()
  rs$run_cap$per_inning <- 5L; rs$run_cap$open_last_inning <- TRUE
  rs$run_cap$same_play_runs_count <- FALSE   # legacy clamping behaviour
  s <- fold_events(list(start_evt(rs),
    new_event("half_runs", list(team = "away", runs = 9L), seq = 2L)))
  expect_equal(s$score$away, 5L)           # capped (inning 1, not the open last inning)
})

test_that("half_runs surfaces a run_cap notice when the cap applies to the entry", {
  rs <- default_ruleset_config()
  rs$run_cap$per_inning <- 5L
  s <- fold_events(list(start_evt(rs),
    new_event("half_runs", list(team = "away", runs = 9L), seq = 2L)))
  expect_true(any(vapply(s$warnings,
    function(w) identical(w$code, "run_cap"), logical(1))))
})
