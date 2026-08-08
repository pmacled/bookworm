library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

mk_lineup <- function(prefix, genders = c("M","F","M","F")) {
  lapply(seq_along(genders), function(i)
    make_player(paste0(prefix, i), paste(prefix, i), genders[i], i, i, i))
}
start_evt <- function() new_event("game_start", list(
  ruleset = default_ruleset_config(), first_bat = "away",
  home = list(team_id = "H", name = "Home", lineup = mk_lineup("h")),
  away = list(team_id = "A", name = "Away", lineup = mk_lineup("a"))
), seq = 1L)

test_that("game_start seeds count, batting team, current batter", {
  s <- fold_events(list(start_evt()))
  expect_equal(s$batting_team, "away")
  expect_equal(s$count$balls, 0L)
  expect_equal(s$current_batter$player_id, "a1")
  expect_equal(s$outs, 0L)
})

test_that("three outs flips to bottom half and resets outs/count", {
  outs3 <- lapply(1:3, function(i) new_event("plate_appearance",
    list(team = "away", batter_id = paste0("a", i), outcome = "GO",
         reached = NA_integer_, rbi = 0L, outs_on_play = 1L, advances = list()),
    seq = 1L + i))
  s <- fold_events(c(list(start_evt()), outs3))
  expect_equal(s$half, "bottom")
  expect_equal(s$outs, 0L)
  expect_equal(s$batting_team, "home")
  expect_equal(s$count$balls, 0L)          # reset to starting count
  expect_equal(s$current_batter$player_id, "h1")
})

test_that("count_override sets the live count", {
  s <- fold_events(list(start_evt(), new_event("count_override",
        list(balls = 3L, strikes = 2L), seq = 2L)))
  expect_equal(s$count, list(balls = 3L, strikes = 2L))
})
