library(testthat)
library(shiny)
for (f in c(
  "app_config.R",
  "rules_engine.R",
  "rule_presets.R",
  "rule_home_run.R",
  "game_events.R",
  "game_reducer.R",
  "disposition.R",
  "disposition_ui.R",
  "storage.R",
  "tracking_module.R"
)) {
  source(file.path("R", f))
}
mk_lineup <- function(prefix) {
  lapply(1:4, function(i) {
    make_player(
      paste0(prefix, i),
      paste(prefix, i),
      c("M", "F", "M", "F")[i],
      i,
      i,
      i
    )
  })
}
start <- new_event(
  "game_start",
  list(
    ruleset = default_ruleset_config(),
    first_bat = "away",
    home = list(team_id = "H", name = "Home", lineup = mk_lineup("h")),
    away = list(team_id = "A", name = "Away", lineup = mk_lineup("a"))
  ),
  seq = 1L
)

test_that("partition_warnings splits violations and notices", {
  w <- list(
    list(severity = "violation", code = "min_females", message = "Need 4 F"),
    list(severity = "notice", code = "run_cap", message = "cap reached")
  )
  p <- partition_warnings(w)
  expect_equal(p$violations, "Need 4 F")
  expect_equal(p$notices, "cap reached")
})

test_that("record_outcome_event for 1B puts batter on first with reached=1", {
  s <- fold_events(list(start))
  e <- record_outcome_event(s, "1B", "away")
  expect_equal(e$payload$reached, 1L)
  expect_equal(e$payload$outs_on_play, 0L)
  s2 <- fold_events(list(start, e))
  expect_equal(s2$bases$first, "a1")
})

test_that("record_outcome_event for K records one out", {
  s <- fold_events(list(start))
  e <- record_outcome_event(s, "K", "away")
  expect_equal(e$payload$outs_on_play, 1L)
  expect_true(is.na(e$payload$reached))
})

test_that("record_half_runs_event targets the batting team", {
  s <- list(batting_team = "home")
  e <- record_half_runs_event(s, 4)
  expect_equal(e$type, "half_runs")
  expect_equal(e$payload$team, "home")
  expect_equal(e$payload$runs, 4L)
})

test_that("run-only half: entering runs advances the half via storage", {
  library(shiny)
  for (f in c("storage.R")) {
    source(file.path("R", f))
  }
  st <- make_storage("guest")
  gid <- st$create_game(list(name = "T"))
  gs <- new_event(
    "game_start",
    list(
      ruleset = default_ruleset_config(),
      first_bat = "away",
      home = list(team_id = "H", name = "Home", lineup = list()),
      away = list(team_id = "A", name = "Away", lineup = list())
    ),
    seq = 1L
  ) # both run-only
  testServer(
    tracking_server,
    args = list(storage = st, game_id = gid, game_start_event = gs),
    {
      session$flushReact()
      session$setInputs(half_runs_n = 3, half_runs_go = 1)
      s <- state()
      expect_equal(s$score$away, 3L)
      expect_equal(s$half, "bottom")
    }
  )
})

test_that("resuming a game with existing events does not re-append the start event", {
  st <- make_storage("guest")
  gid <- st$create_game(list(name = "T"))
  # Pre-seed the log so the game already exists.
  st$append_event(gid, start)
  expect_equal(length(st$load_events(gid)), 1L)

  testServer(
    tracking_server,
    args = list(
      storage = st,
      game_id = gid,
      game_start_event = NULL,
      read_only = FALSE
    ),
    {
      session$flushReact()
      # No extra start event appended; events load as-is.
      expect_equal(length(events()), 1L)
      expect_equal(state()$status, "in_progress")
    }
  )
})

test_that("read-only mode suppresses outcome recording", {
  st <- make_storage("guest")
  gid <- st$create_game(list(name = "T"))
  st$append_event(gid, start)

  testServer(
    tracking_server,
    args = list(
      storage = st,
      game_id = gid,
      game_start_event = NULL,
      read_only = TRUE
    ),
    {
      session$flushReact()
      before <- length(events())
      session$setInputs(o_1B = 1) # would record a hit if editable
      session$flushReact()
      expect_equal(length(events()), before) # unchanged
      expect_null(output$secondary_actions)
    }
  )
})

test_that("undo then record does not resurrect the undone event", {
  st <- make_storage("guest")
  gid <- st$create_game(list(name = "T"))
  mk <- function(prefix) {
    lapply(1:4, function(i) {
      make_player(
        paste0(prefix, i),
        paste(prefix, i),
        c("M", "F", "M", "F")[i],
        i,
        i,
        i
      )
    })
  }
  gs <- new_event(
    "game_start",
    list(
      ruleset = default_ruleset_config(),
      first_bat = "away",
      home = list(team_id = "H", name = "Home", lineup = mk("h")),
      away = list(team_id = "A", name = "Away", lineup = mk("a"))
    ),
    seq = 1L
  )
  shiny::testServer(
    tracking_server,
    args = list(storage = st, game_id = gid, game_start_event = gs),
    {
      session$flushReact() # let ignoreInit observers do their initial skip
      # (testServer defers this to the first setInputs otherwise)
      session$setInputs(o_1B = 1) # a1 to first — bases empty, commits in one tap
      # a2 grounds out. With a1 on first this opens the disposition modal; commit it:
      # a1 holds at first, a2 is out at the plate.
      session$setInputs(o_GO = 1)
      session$setInputs(disp_a1 = "1", disp_a2 = "OUT", disp_rbi = 0)
      session$setInputs(disp_commit = 1)
      session$setInputs(undo = 1) # undo the GO
      # a2 strikes out instead. a1 still on first, so commit the modal the same way.
      session$setInputs(o_K = 1)
      session$setInputs(disp_a1 = "1", disp_a2 = "OUT", disp_rbi = 0)
      session$setInputs(disp_commit = 1)
      s <- state()
      expect_equal(s$outs, 1L) # only ONE out — GO was undone, not double-applied
      expect_equal(s$bases$first, "a1") # a1 still on first
    }
  )
})
