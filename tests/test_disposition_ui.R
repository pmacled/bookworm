library(testthat)
suppressMessages({
  library(shiny)
  library(bslib)
  library(htmltools)
})
for (f in c(
  "app_config.R",
  "rules_engine.R",
  "rule_presets.R",
  "rule_home_run.R",
  "game_events.R",
  "game_reducer.R",
  "disposition.R",
  "disposition_ui.R"
)) {
  source(file.path("R", f))
}

mk <- function(prefix, n = 4L) {
  lapply(seq_len(n), function(i) {
    make_player(
      paste0(prefix, i),
      paste(prefix, i),
      c("M", "F")[(i %% 2) + 1L],
      i,
      i,
      i
    )
  })
}
st <- function() {
  s <- initial_game_state()
  s$lineups$away <- mk("a")
  s$batting_team <- "away"
  s$current_batter <- s$lineups$away[[1]]
  s$bases$first <- "a4"
  s
}

test_that("the modal renders one control group per runner", {
  s <- st()
  rows <- disposition_rows(s)
  html <- as.character(disposition_modal_ui(
    NS("track"),
    rows,
    disposition_prefill(s, "1B"),
    "1B"
  ))
  expect_true(grepl("track-disp_a4", html, fixed = TRUE))
  expect_true(grepl("track-disp_a1", html, fixed = TRUE))
  expect_true(grepl("track-disp_commit", html, fixed = TRUE))
  expect_false(grepl("track-disp_rbi", html, fixed = TRUE))
})

test_that("the modal is titled Baserunners", {
  s <- st()
  rows <- disposition_rows(s)
  html <- as.character(disposition_modal_ui(
    NS("track"),
    rows,
    disposition_prefill(s, "1B"),
    "1B"
  ))
  expect_true(grepl("Baserunners", html, fixed = TRUE))
})

test_that("the modal names each runner and their current base", {
  s <- st()
  rows <- disposition_rows(s)
  html <- as.character(disposition_modal_ui(
    NS("track"),
    rows,
    disposition_prefill(s, "1B"),
    "1B"
  ))
  expect_true(
    grepl("a 4", html, fixed = TRUE) || grepl("a4", html, fixed = TRUE)
  )
  expect_true(grepl("first", html, ignore.case = TRUE))
  expect_true(grepl("batter", html, ignore.case = TRUE))
})

test_that("every level is offered for every runner", {
  s <- st()
  rows <- disposition_rows(s)
  html <- as.character(disposition_modal_ui(
    NS("track"),
    rows,
    disposition_prefill(s, "1B"),
    "1B"
  ))
  for (lvl in DISPOSITION_LEVELS) {
    expect_true(
      grepl(paste0('value="', lvl, '"'), html, fixed = TRUE),
      info = lvl
    )
  }
})

test_that("read_disposition_choices pulls one value per row", {
  s <- st()
  rows <- disposition_rows(s)
  input <- list(disp_a4 = "2", disp_a1 = "1")
  ch <- read_disposition_choices(input, rows)
  expect_equal(unname(ch[["a4"]]), "2")
  expect_equal(unname(ch[["a1"]]), "1")
  expect_length(ch, 2L)
})

test_that("a missing input becomes an empty string so validation catches it", {
  s <- st()
  rows <- disposition_rows(s)
  ch <- read_disposition_choices(list(disp_a1 = "1"), rows)
  expect_equal(unname(ch[["a4"]]), "")
  expect_false(validate_disposition(rows, ch)$ok)
})
