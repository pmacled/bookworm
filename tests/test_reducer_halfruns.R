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

test_that("half_runs respects the run cap at the shipping default", {
  # same_play_runs_count defaults to TRUE, but half_runs is a bulk total for the
  # whole half, not a single play in progress, so it must clamp regardless.
  rs <- default_ruleset_config()
  rs$run_cap$per_inning <- 5L; rs$run_cap$open_last_inning <- TRUE
  expect_true(rs$run_cap$same_play_runs_count)   # shipping default, not overridden
  s <- fold_events(list(start_evt(rs),
    new_event("half_runs", list(team = "away", runs = 9L), seq = 2L)))
  expect_equal(s$score$away, 5L)           # capped (inning 1, not the open last inning)
})

test_that("half_runs at or below the cap passes through unchanged", {
  # Regression: the clamp branch used to compute remaining room under the cap
  # and return *that* rather than min(runs_on_play, room) -- inflating any
  # at-or-below-cap entry up to the cap. This entry (3) must stay 3, not
  # become 5.
  rs <- default_ruleset_config()
  rs$run_cap$per_inning <- 5L
  s <- fold_events(list(start_evt(rs),
    new_event("half_runs", list(team = "away", runs = 3L), seq = 2L)))
  expect_equal(s$score$away, 3L)
  expect_equal(s$line_score$away, 3L)
  expect_false(any(vapply(s$warnings, function(w) identical(w$code, "run_cap"), logical(1))))
})

test_that("half_runs of zero stays zero under a run cap", {
  rs <- default_ruleset_config()
  rs$run_cap$per_inning <- 5L
  s <- fold_events(list(start_evt(rs),
    new_event("half_runs", list(team = "away", runs = 0L), seq = 2L)))
  expect_equal(s$score$away, 0L)
  expect_equal(s$line_score$away, 0L)
})

test_that("half_runs surfaces a run_cap notice when the cap applies to the entry", {
  rs <- default_ruleset_config()
  rs$run_cap$per_inning <- 5L
  s <- fold_events(list(start_evt(rs),
    new_event("half_runs", list(team = "away", runs = 9L), seq = 2L)))
  hit <- Filter(function(w) identical(w$code, "run_cap"), s$warnings)
  expect_length(hit, 1L)
  # The scorer needs to know the cap value...
  expect_match(hit[[1]]$message, "5", fixed = TRUE)
  # ...but NOT the "runs in the same at-bat still count" caveat. A bulk
  # half-inning entry has no at-bats and is always clamped, so that sentence
  # would describe behaviour the scorer did not get.
  expect_false(grepl("at-bat", hit[[1]]$message, fixed = TRUE))
})
