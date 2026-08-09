library(testthat)
suppressMessages({ library(shiny); library(bslib); library(htmltools) })
for (f in c("app_config.R","rules_engine.R","rule_presets.R","game_events.R",
            "game_reducer.R","setup_module.R","lineup_modal.R"))
  source(file.path("R", f))

st <- function(away = list()) {
  s <- initial_game_state()
  s$lineups$away <- away
  s$teams <- list(away = list(team_id = "A", name = "Otters"),
                  home = list(team_id = "H", name = "Badgers"))
  s
}

test_that("a pre-filled row carries its existing values", {
  html <- as.character(.player_row(NS("track"), "lu_away", 1L, order = 1L,
    show_gender = TRUE,
    values = list(name = "Ann", gender = "F", jersey = 7L, position = "SS")))
  expect_true(grepl('value="Ann"', html, fixed = TRUE))
  expect_true(grepl('value="7"', html, fixed = TRUE))
})

test_that("a row with no values is blank and still has its ids", {
  html <- as.character(.player_row(NS("track"), "lu_away", 2L, order = 2L,
                                   show_gender = TRUE))
  expect_true(grepl('id="track-lu_away_name_2"', html, fixed = TRUE))
  expect_false(grepl('value="Ann"', html, fixed = TRUE))
})

test_that("the modal renders at least twelve rows for an empty lineup", {
  html <- as.character(lineup_modal_ui(NS("track"), st(), "away",
                                       show_gender = TRUE, n_rows = 12L))
  for (i in 1:12)
    expect_true(grepl(paste0("track-lu_away_name_", i), html, fixed = TRUE), info = i)
})

test_that("the modal pre-fills an existing lineup and adds spare rows", {
  lu <- list(make_player("a1", "Ann", "F", 7L, 1L, "SS"),
             make_player("a2", "Bo",  "M", 3L, 2L, "P"))
  html <- as.character(lineup_modal_ui(NS("track"), st(lu), "away",
                                       show_gender = TRUE, n_rows = 5L))
  expect_true(grepl('value="Ann"', html, fixed = TRUE))
  expect_true(grepl('value="Bo"', html, fixed = TRUE))
  expect_true(grepl("track-lu_away_name_5", html, fixed = TRUE))
})

test_that("the modal names the team and omits gender when genderless", {
  html <- as.character(lineup_modal_ui(NS("track"), st(), "away",
                                       show_gender = FALSE, n_rows = 3L))
  expect_true(grepl("Otters", html, fixed = TRUE))
  expect_false(grepl("lu_away_gender_1", html, fixed = TRUE))
})

test_that("build_lineup_set_event produces a valid event that skips blank rows", {
  input <- list(
    lu_away_name_1 = "Ann", lu_away_gender_1 = "F", lu_away_jersey_1 = "7",
    lu_away_pos_1 = "SS",
    lu_away_name_2 = "",    lu_away_gender_2 = "M", lu_away_jersey_2 = "",
    lu_away_pos_2 = "",
    lu_away_name_3 = "Bo",  lu_away_gender_3 = "M", lu_away_jersey_3 = "3",
    lu_away_pos_3 = "P")
  evt <- build_lineup_set_event(input, "away", 1:3, show_gender = TRUE)
  expect_equal(evt$type, "lineup_set")
  expect_equal(evt$payload$team, "away")
  expect_length(evt$payload$lineup, 2L)
  expect_equal(evt$payload$lineup[[1]]$name, "Ann")
  expect_equal(evt$payload$lineup[[2]]$order_slot, 2L)
  expect_true(validate_event(evt)$ok)
})

test_that("an all-blank grid produces a valid empty lineup", {
  evt <- build_lineup_set_event(list(), "home", 1:5, show_gender = TRUE)
  expect_length(evt$payload$lineup, 0L)
  expect_true(validate_event(evt)$ok)
})
