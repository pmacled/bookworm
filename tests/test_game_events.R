library(testthat)
source(file.path("R", "app_config.R"))
source(file.path("R", "game_events.R"))

test_that("new_event builds a well-formed event", {
  e <- new_event("plate_appearance", list(team = "away", batter_id = "p1",
    outcome = "1B", reached = 1L, rbi = 0L, outs_on_play = 0L, advances = list()))
  expect_equal(e$type, "plate_appearance")
  expect_true(validate_event(e)$ok)
})

test_that("validate_event rejects unknown type and bad outcome", {
  expect_false(validate_event(new_event("nope", list()))$ok)
  bad <- new_event("plate_appearance", list(team = "away", batter_id = "p1", outcome = "ZZ"))
  expect_false(validate_event(bad)$ok)
})

test_that("make_player and make_advance shape fields", {
  p <- make_player("p1", "Sam", "F", 9L, 1L, 6L)
  expect_equal(p$gender, "F")
  a <- make_advance("p1", 1L, 4L, scored = TRUE)
  expect_true(a$scored)
})
