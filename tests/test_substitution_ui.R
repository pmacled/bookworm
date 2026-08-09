library(testthat)
suppressMessages({ library(shiny); library(bslib); library(htmltools) })
for (f in c("app_config.R","rules_engine.R","rule_presets.R","rule_pinch_runner.R",
            "game_events.R","game_reducer.R","setup_module.R","substitution_ui.R"))
  source(file.path("R", f))

mk <- function(prefix, n = 3L) lapply(seq_len(n), function(i)
  make_player(paste0(prefix, i), paste(prefix, i), c("M","F")[(i %% 2) + 1L], i, i,
              c("P","C","SS")[i]))
st <- function(cfg = default_ruleset_config()) {
  s <- initial_game_state(cfg)
  s$lineups$away <- mk("a"); s$lineups$home <- mk("h")
  s$batting_team <- "away"; s$current_batter <- s$lineups$away[[1]]
  s$bases$second <- "a3"
  s
}

test_that("the batting modal lists both teams' order slots", {
  html <- as.character(substitution_modal_ui(NS("track"), st(), "batting"))
  expect_true(grepl("track-sub_slot", html, fixed = TRUE))
  expect_true(grepl("track-sub_name", html, fixed = TRUE))
  expect_true(grepl("track-sub_commit", html, fixed = TRUE))
})

test_that("the pinch-runner modal offers only occupied bases", {
  html <- as.character(substitution_modal_ui(NS("track"), st(), "courtesy_runner"))
  expect_true(grepl("second", html, ignore.case = TRUE))
  expect_false(grepl(">first<", html, fixed = TRUE))
})

test_that("the defensive modal offers the fielding team's players", {
  html <- as.character(substitution_modal_ui(NS("track"), st(), "defensive"))
  expect_true(grepl("h 1", html, fixed = TRUE) || grepl("h1", html, fixed = TRUE))
  expect_true(grepl("track-sub_pos", html, fixed = TRUE))
})

test_that("build_substitution_event refuses a blank incoming name", {
  r <- build_substitution_event(list(sub_slot = "1", sub_name = "  "), st(), "batting")
  expect_true(length(r$errors) > 0)
})

test_that("build_substitution_event produces a valid batting event", {
  evt <- build_substitution_event(
    list(sub_slot = "1", sub_name = "Sub", sub_gender = "F", sub_jersey = "22",
         sub_pos = "2B"), st(), "batting")
  expect_equal(evt$type, "substitution")
  expect_equal(evt$payload$kind, "batting")
  expect_equal(evt$payload$order_slot, 1L)
  expect_equal(evt$payload$in_player$name, "Sub")
  expect_equal(evt$payload$in_player$jersey_number, 22L)
  expect_true(validate_event(evt)$ok)
})

test_that("a pinch runner past the per-inning limit is rejected", {
  cfg <- coerce_ruleset_config(list(pinch_runner = list(max_per_inning = 1L)))
  s <- st(cfg)
  s$pinch_runner_log <- list(list(inning = s$inning, half = s$half, team = "away",
                                  out_player_id = "x", in_player_id = "y"))
  r <- build_substitution_event(
    list(sub_base = "second", sub_name = "Runner", sub_gender = "M", sub_jersey = "5"),
    s, "courtesy_runner")
  expect_true(length(r$errors) > 0)
  expect_match(paste(r$errors, collapse = " "), "per inning")
})

test_that("an eligible pinch runner produces a valid event", {
  evt <- build_substitution_event(
    list(sub_base = "second", sub_name = "Runner", sub_gender = "M", sub_jersey = "5"),
    st(), "courtesy_runner")
  expect_equal(evt$payload$kind, "courtesy_runner")
  expect_equal(evt$payload$out_player_id, "a3")
  expect_true(validate_event(evt)$ok)
})
