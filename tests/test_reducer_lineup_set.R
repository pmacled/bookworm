library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

mk <- function(prefix, genders) lapply(seq_along(genders), function(i)
  make_player(paste0(prefix, i), paste(prefix, i), genders[i], i, i, i))

start_runonly <- function(cfg = default_ruleset_config())
  new_event("game_start", list(ruleset = cfg, first_bat = "away",
    home = list(team_id="H", name="Home", lineup = mk("h", c("M","F","M","F"))),
    away = list(team_id="A", name="Away", lineup = list())), seq = 1L)

test_that("lineup_set is a known event type and validates its payload", {
  expect_true("lineup_set" %in% EVENT_TYPES)
  ok <- new_event("lineup_set", list(team = "away", lineup = list()))
  expect_true(validate_event(ok)$ok)
  expect_false(validate_event(new_event("lineup_set", list(team = "x", lineup = list())))$ok)
  expect_false(validate_event(new_event("lineup_set", list(team = "away")))$ok)
})

test_that("lineup_set installs a lineup on a run-only team and sets the batter", {
  s <- fold_events(list(
    start_runonly(),
    new_event("lineup_set", list(team = "away", lineup = mk("a", c("M","F","M"))), seq = 2L)))
  expect_length(s$lineups$away, 3L)
  expect_equal(s$current_batter$player_id, "a1")
})

test_that("lineup_set clamps a batting index that overruns the new lineup", {
  s0 <- fold_events(list(start_runonly()))
  s0$batting_index$home <- 9L
  evt <- new_event("lineup_set", list(team = "home", lineup = mk("h", c("M","F"))), seq = 2L)
  s <- apply_event(s0, evt)
  expect_length(s$lineups$home, 2L)
  # index 9 %% 2 == 1 -> second batter; must not error or return NULL
  expect_false(is.null(s$current_batter))
})

test_that("a late lineup triggers a retroactive batting-order violation", {
  cfg <- coerce_ruleset_config(list(
    batting_gender_rule = list(type = "max_consecutive_males", n = 1L)))
  # Two male plate appearances are recorded before the lineup identifies their genders.
  pa <- function(id, seq) new_event("plate_appearance", list(team = "away",
    batter_id = id, outcome = "1B", reached = 1L, rbi = 0L, outs_on_play = 0L,
    advances = list(make_advance(id, 0L, 1L))), seq = seq)
  s <- fold_events(list(
    start_runonly(cfg),
    new_event("lineup_set", list(team = "away", lineup = mk("a", c("M","M","F"))), seq = 2L),
    pa("a1", 3L), pa("a2", 4L)))
  codes <- vapply(s$warnings, function(w) w$code, character(1))
  expect_true("batting_gender" %in% codes)
})

test_that("lineup_set re-evaluates already-recorded plate appearances", {
  cfg <- coerce_ruleset_config(list(
    batting_gender_rule = list(type = "max_consecutive_males", n = 1L)))
  pa <- function(id, seq) new_event("plate_appearance", list(team = "away",
    batter_id = id, outcome = "1B", reached = 1L, rbi = 0L, outs_on_play = 0L,
    advances = list(make_advance(id, 0L, 1L))), seq = seq)
  # Batters recorded first; the lineup naming them both male arrives afterwards.
  s <- fold_events(list(
    start_runonly(cfg),
    pa("a1", 2L), pa("a2", 3L),
    new_event("lineup_set", list(team = "away", lineup = mk("a", c("M","M","F"))), seq = 4L)))
  hits <- Filter(function(w) identical(w$code, "batting_gender_retro"), s$warnings)
  expect_length(hits, 1L)
  expect_match(hits[[1]]$message, "a2")
  expect_equal(hits[[1]]$severity, "violation")
})
