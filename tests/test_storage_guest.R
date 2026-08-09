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
