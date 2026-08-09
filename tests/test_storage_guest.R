library(testthat)
for (f in c(
  "app_config.R",
  "game_events.R",
  "rules_engine.R",
  "game_reducer.R",
  "json_io.R",
  "storage.R"
)) {
  source(file.path("R", f))
}

test_that("guest storage assigns increasing seq and reloads events", {
  st <- make_storage("guest")
  gid <- st$create_game(list(name = "Test"))
  e1 <- st$append_event(
    gid,
    new_event("count_override", list(balls = 0L, strikes = 0L))
  )
  e2 <- st$append_event(
    gid,
    new_event("count_override", list(balls = 1L, strikes = 0L))
  )
  expect_equal(e1$seq, 1L)
  expect_equal(e2$seq, 2L)
  evs <- st$load_events(gid)
  expect_equal(length(evs), 2L)
})

test_that("guest storage lists games", {
  st <- make_storage("guest")
  st$create_game(list(name = "G1"))
  expect_equal(nrow(st$list_games()), 1L)
})

test_that("guest storage deletes a game and its events", {
  st <- make_storage("guest")
  gid <- st$create_game(list(name = "G1"))
  st$append_event(
    gid,
    new_event("count_override", list(balls = 0L, strikes = 0L))
  )
  expect_equal(nrow(st$list_games()), 1L)
  expect_true(st$delete_game(gid))
  expect_equal(nrow(st$list_games()), 0L)
  expect_equal(length(st$load_events(gid)), 0L)
})

test_that("guest list_games includes team and can_delete columns", {
  st <- make_storage("guest")
  st$create_game(list(name = "G1"))
  lg <- st$list_games()
  expect_true(all(c("home_team", "away_team", "can_delete") %in% names(lg)))
  expect_true(all(lg$can_delete))
})
