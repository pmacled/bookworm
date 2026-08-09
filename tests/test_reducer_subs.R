library(testthat)
for (f in c(
  "app_config.R",
  "rules_engine.R",
  "game_events.R",
  "game_reducer.R"
)) {
  source(file.path("R", f))
}
mk_lineup <- function(prefix, genders = c("M", "F", "M", "F")) {
  lapply(seq_along(genders), function(i) {
    make_player(paste0(prefix, i), paste(prefix, i), genders[i], i, i, i)
  })
}
start_evt <- function() {
  new_event(
    "game_start",
    list(
      ruleset = default_ruleset_config(),
      first_bat = "away",
      home = list(team_id = "H", name = "Home", lineup = mk_lineup("h")),
      away = list(team_id = "A", name = "Away", lineup = mk_lineup("a"))
    ),
    seq = 1L
  )
}

test_that("batting sub replaces the player in the order slot", {
  sub <- new_event(
    "substitution",
    list(
      team = "away",
      kind = "batting",
      out_player_id = "a1",
      order_slot = 1L,
      in_player = make_player("x9", "Pinch", "F", 99L, 1L, NA_integer_)
    ),
    seq = 2L
  )
  s <- fold_events(list(start_evt(), sub))
  slot1 <- Filter(function(p) p$order_slot == 1L, s$lineups$away)[[1]]
  expect_equal(slot1$player_id, "x9")
})

test_that("courtesy runner swaps the runner on base", {
  pa1 <- new_event(
    "plate_appearance",
    list(
      team = "away",
      batter_id = "a1",
      outcome = "1B",
      reached = 1L,
      rbi = 0L,
      outs_on_play = 0L,
      advances = list()
    ),
    seq = 2L
  )
  cr <- new_event(
    "substitution",
    list(
      team = "away",
      kind = "courtesy_runner",
      out_player_id = "a1",
      in_player = make_player(
        "cr1",
        "Runner",
        "M",
        50L,
        NA_integer_,
        NA_integer_
      )
    ),
    seq = 3L
  )
  s <- fold_events(list(start_evt(), pa1, cr))
  expect_equal(s$bases$first, "cr1")
})

test_that("a batting substitution keeps every player field", {
  st <- initial_game_state()
  st$lineups$away <- list(make_player("a1", "A1", "M", 1L, 1L, "SS"))
  evt <- new_event(
    "substitution",
    list(
      team = "away",
      kind = "batting",
      order_slot = 1L,
      in_player = make_player("a9", "Sub", "F", 22L, NA_integer_, "2B")
    )
  )
  s <- apply_substitution(st, evt)
  p <- s$lineups$away[[1]]
  expect_equal(p$player_id, "a9")
  expect_equal(p$name, "Sub")
  expect_equal(p$gender, "F")
  expect_equal(p$jersey_number, 22L)
  expect_equal(p$order_slot, 1L)
  expect_equal(p$position, "2B")
})

test_that("a defensive substitution keeps the batting order slot", {
  st <- initial_game_state()
  st$lineups$home <- list(make_player("h1", "H1", "M", 1L, 3L, "P"))
  evt <- new_event(
    "substitution",
    list(
      team = "home",
      kind = "defensive",
      out_player_id = "h1",
      position = "LF",
      in_player = make_player("h9", "Sub", "M", 44L, NA_integer_, NA_character_)
    )
  )
  s <- apply_substitution(st, evt)
  p <- s$lineups$home[[1]]
  expect_equal(p$player_id, "h9")
  expect_equal(p$position, "LF")
  expect_equal(p$order_slot, 3L)
})

test_that("a full substitution inherits the outgoing slot and given position", {
  st <- initial_game_state()
  st$lineups$away <- list(make_player("a1", "A1", "M", 1L, 4L, "SS"))
  evt <- new_event(
    "substitution",
    list(
      team = "away",
      kind = "full",
      out_player_id = "a1",
      position = "CF",
      in_player = make_player("a9", "Sub", "F", 22L, NA_integer_, NA_character_)
    )
  )
  s <- apply_substitution(st, evt)
  p <- s$lineups$away[[1]]
  expect_equal(p$player_id, "a9")
  expect_equal(p$order_slot, 4L)
  expect_equal(p$position, "CF")
})

test_that("a full substitution falls back to the outgoing player's position", {
  st <- initial_game_state()
  st$lineups$away <- list(make_player("a1", "A1", "M", 1L, 4L, "SS"))
  evt <- new_event(
    "substitution",
    list(
      team = "away",
      kind = "full",
      out_player_id = "a1",
      position = NA_character_,
      in_player = make_player("a9", "Sub", "F", 22L, NA_integer_, NA_character_)
    )
  )
  s <- apply_substitution(st, evt)
  expect_equal(s$lineups$away[[1]]$position, "SS")
})

test_that("a full substitution of an on-base runner swaps the runner too", {
  st <- initial_game_state()
  st$lineups$away <- list(make_player("a1", "A1", "M", 1L, 4L, "SS"))
  st$bases$second <- "a1"
  evt <- new_event(
    "substitution",
    list(
      team = "away",
      kind = "full",
      out_player_id = "a1",
      position = "CF",
      in_player = make_player("a9", "Sub", "F", 22L, NA_integer_, NA_character_)
    )
  )
  s <- apply_substitution(st, evt)
  expect_equal(s$bases$second, "a9")
})
