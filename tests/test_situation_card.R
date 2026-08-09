library(testthat)
suppressMessages({ library(shiny); library(bslib); library(htmltools) })
for (f in c("app_config.R","brand_colors.R","rules_engine.R","rule_presets.R",
            "game_events.R","game_reducer.R","boxscore.R","situation_card.R"))
  source(file.path("R", f))

mk <- function(prefix, n = 3L) lapply(seq_len(n), function(i)
  make_player(paste0(prefix, i), paste0(toupper(prefix), i), "M", i, i, i))
st <- function() {
  s <- initial_game_state()
  s$lineups$away <- mk("a"); s$lineups$home <- mk("h")
  s$teams <- list(away = list(team_id = "A", name = "Otters"),
                  home = list(team_id = "H", name = "Badgers"))
  s$current_batter <- s$lineups$away[[1]]
  s$score <- list(home = 2L, away = 5L)
  s$inning <- 4L; s$half <- "top"; s$outs <- 2L
  s$count <- list(balls = 1L, strikes = 2L)
  s
}

test_that("the card shows both team names and the score", {
  html <- as.character(situation_card_ui(st()))
  expect_true(grepl("Otters", html, fixed = TRUE))
  expect_true(grepl("Badgers", html, fixed = TRUE))
  expect_true(grepl(">5<", html, fixed = TRUE))
  expect_true(grepl(">2<", html, fixed = TRUE))
})

test_that("the half-inning is an arrow, not the words top and bottom", {
  html <- as.character(situation_card_ui(st()))
  expect_true(grepl("&#9650;", html, fixed = TRUE) || grepl("\u25b2", html, fixed = TRUE))
  expect_false(grepl(">top<", html, fixed = TRUE))
})

test_that("the count and outs are shown", {
  html <- as.character(situation_card_ui(st()))
  expect_true(grepl("1-2", html, fixed = TRUE))
  expect_true(grepl("2 out", html, fixed = TRUE))
})

test_that("the current batter and their line are shown", {
  s <- st()
  s$pa_log <- list(
    list(team = "away", batter_id = "a1", outcome = "1B", rbi = 1L, reached = 1L),
    list(team = "away", batter_id = "a1", outcome = "K",  rbi = 0L, reached = NA_integer_))
  html <- as.character(situation_card_ui(s))
  expect_true(grepl("A1", html, fixed = TRUE))
  expect_true(grepl("1-for-2", html, fixed = TRUE))
})

test_that("a final game says FINAL", {
  s <- st(); s$status <- "final"
  expect_true(grepl("FINAL", as.character(situation_card_ui(s)), fixed = TRUE))
})

test_that("the card renders with no current batter", {
  s <- st(); s$current_batter <- NULL
  expect_silent(html <- as.character(situation_card_ui(s)))
  expect_true(grepl("Otters", html, fixed = TRUE))
})

test_that("batter_line_text summarises hits, RBI, and walks", {
  expect_equal(batter_line_text(list(AB = 2L, H = 1L, RBI = 1L, BB = 0L, K = 0L, R = 0L)),
               "1-for-2, RBI")
  expect_equal(batter_line_text(list(AB = 1L, H = 0L, RBI = 0L, BB = 0L, K = 1L, R = 0L)),
               "0-for-1, K")
  expect_equal(batter_line_text(list(AB = 0L, H = 0L, RBI = 0L, BB = 1L, K = 0L, R = 0L)),
               "0-for-0, BB")
  expect_equal(batter_line_text(list(AB = 0L, H = 0L, RBI = 0L, BB = 0L, K = 0L, R = 0L)),
               "first at-bat")
})
