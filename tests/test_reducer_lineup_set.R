library(testthat)
for (f in c(
  "app_config.R",
  "rules_engine.R",
  "game_events.R",
  "game_reducer.R"
)) {
  source(file.path("R", f))
}

# name == player_id (both paste0, no separator) so that later assertions on the
# violation message can match a batter's id unambiguously.
mk <- function(prefix, genders) {
  lapply(seq_along(genders), function(i) {
    make_player(paste0(prefix, i), paste0(prefix, i), genders[i], i, i, i)
  })
}

start_runonly <- function(cfg = default_ruleset_config()) {
  new_event(
    "game_start",
    list(
      ruleset = cfg,
      first_bat = "away",
      home = list(
        team_id = "H",
        name = "Home",
        lineup = mk("h", c("M", "F", "M", "F"))
      ),
      away = list(team_id = "A", name = "Away", lineup = list())
    ),
    seq = 1L
  )
}

test_that("lineup_set is a known event type and validates its payload", {
  expect_true("lineup_set" %in% EVENT_TYPES)
  ok <- new_event("lineup_set", list(team = "away", lineup = list()))
  expect_true(validate_event(ok)$ok)
  expect_false(
    validate_event(new_event(
      "lineup_set",
      list(team = "x", lineup = list())
    ))$ok
  )
  expect_false(validate_event(new_event("lineup_set", list(team = "away")))$ok)
})

test_that("lineup_set installs a lineup on a run-only team and sets the batter", {
  s <- fold_events(list(
    start_runonly(),
    new_event(
      "lineup_set",
      list(team = "away", lineup = mk("a", c("M", "F", "M"))),
      seq = 2L
    )
  ))
  expect_length(s$lineups$away, 3L)
  expect_equal(s$current_batter$player_id, "a1")
})

test_that("lineup_set clamps a batting index that overruns the new lineup", {
  s0 <- fold_events(list(start_runonly()))
  s0$batting_index$home <- 9L
  evt <- new_event(
    "lineup_set",
    list(team = "home", lineup = mk("h", c("M", "F"))),
    seq = 2L
  )
  s <- apply_event(s0, evt)
  expect_length(s$lineups$home, 2L)
  # index 9 %% 2 == 1 (a min()-based clamp would instead floor to 2, the lineup
  # length, which is out of range for a 0-based index into a 2-player lineup).
  expect_equal(s$batting_index$home, 1L)
})

test_that("current_batter is untouched when lineup_set targets the team not at bat", {
  # away is up (and empty/run-only); lineup_set names home, which is not batting.
  # current_batter is a single state slot scoped to whoever is actually up, so it
  # must not be reassigned to a home player while away is still at the plate.
  s0 <- fold_events(list(start_runonly()))
  expect_null(s0$current_batter)
  evt <- new_event(
    "lineup_set",
    list(team = "home", lineup = mk("h2", c("M", "F"))),
    seq = 2L
  )
  s <- apply_event(s0, evt)
  expect_null(s$current_batter)
  expect_equal(s$batting_team, "away")
})

test_that("lineup_set re-evaluates already-recorded plate appearances", {
  cfg <- coerce_ruleset_config(list(
    batting_gender_rule = list(type = "max_consecutive_males", n = 1L)
  ))
  pa <- function(id, seq) {
    new_event(
      "plate_appearance",
      list(
        team = "away",
        batter_id = id,
        outcome = "1B",
        reached = 1L,
        rbi = 0L,
        outs_on_play = 0L,
        advances = list(make_advance(id, 0L, 1L))
      ),
      seq = seq
    )
  }
  # Two male plate appearances are recorded while away's lineup is still empty --
  # genders unknown -- and the lineup naming them both male arrives only after.
  s <- fold_events(list(
    start_runonly(cfg),
    pa("a1", 2L),
    pa("a2", 3L),
    new_event(
      "lineup_set",
      list(team = "away", lineup = mk("a", c("M", "M", "F"))),
      seq = 4L
    )
  ))
  hits <- Filter(
    function(w) identical(w$code, "batting_gender_retro"),
    s$warnings
  )
  expect_length(hits, 1L)
  expect_match(hits[[1]]$message, "a2")
  expect_equal(hits[[1]]$severity, "violation")
})

test_that("the retroactive violation names the earliest offender, not the last", {
  cfg <- coerce_ruleset_config(list(
    batting_gender_rule = list(type = "max_consecutive_males", n = 1L)
  ))
  pa <- function(id, seq) {
    new_event(
      "plate_appearance",
      list(
        team = "away",
        batter_id = id,
        outcome = "1B",
        reached = 1L,
        rbi = 0L,
        outs_on_play = 0L,
        advances = list(make_advance(id, 0L, 1L))
      ),
      seq = seq
    )
  }
  # Three straight male batters: a2 is already the earliest break (a1 alone is
  # fine; a1-then-a2 is two males in a row). a3 also "violates" if you keep
  # scanning without stopping at the first hit -- a wrong implementation that
  # returns the last match found, rather than the first, would report a3.
  s <- fold_events(list(
    start_runonly(cfg),
    pa("a1", 2L),
    pa("a2", 3L),
    pa("a3", 4L),
    new_event(
      "lineup_set",
      list(team = "away", lineup = mk("a", c("M", "M", "M"))),
      seq = 5L
    )
  ))
  hits <- Filter(
    function(w) identical(w$code, "batting_gender_retro"),
    s$warnings
  )
  expect_length(hits, 1L)
  expect_match(hits[[1]]$message, "a2", fixed = TRUE)
  expect_false(grepl("a3", hits[[1]]$message, fixed = TRUE))
})

test_that("a batter no longer in the lineup is skipped, not counted as a gender", {
  cfg <- coerce_ruleset_config(list(
    batting_gender_rule = list(type = "max_consecutive_males", n = 1L)
  ))
  pa <- function(id, seq) {
    new_event(
      "plate_appearance",
      list(
        team = "away",
        batter_id = id,
        outcome = "1B",
        reached = 1L,
        rbi = 0L,
        outs_on_play = 0L,
        advances = list(make_advance(id, 0L, 1L))
      ),
      seq = seq
    )
  }
  # a1, a2 and a3 all bat while the lineup names all three male. a2 is then
  # substituted out of the batting order entirely (a real, common occurrence),
  # so by the time the roster is reaffirmed, a2's batter_id no longer resolves
  # to a gender. The correct rule replay skips a2 (unknown != a broken streak)
  # and compares a1 directly against a3 -- two known males back to back -- so
  # a3 is the violator. Silently treating the missing gender as "not male" would
  # instead let the streak-of-known-males counter reset and raise nothing.
  events <- list(
    start_runonly(cfg),
    new_event(
      "lineup_set",
      list(team = "away", lineup = mk("a", c("M", "M", "M"))),
      seq = 2L
    ),
    pa("a1", 3L),
    pa("a2", 4L),
    new_event(
      "substitution",
      list(
        team = "away",
        kind = "batting",
        order_slot = 2L,
        in_player = make_player("a2sub", "a2sub", "F", 2L, 2L, NA_character_)
      ),
      seq = 5L
    ),
    pa("a3", 6L)
  )
  s <- fold_events(events)
  # Re-affirm the (now post-substitution) roster to trigger another retro pass.
  s2 <- apply_event(
    s,
    new_event(
      "lineup_set",
      list(team = "away", lineup = s$lineups$away),
      seq = 7L
    )
  )
  hits <- Filter(
    function(w) identical(w$code, "batting_gender_retro"),
    s2$warnings
  )
  expect_length(hits, 1L)
  expect_match(hits[[1]]$message, "a3", fixed = TRUE)
})
