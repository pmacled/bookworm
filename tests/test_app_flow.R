# App-level wiring test: exercises bookworm_server's observers via testServer,
# covering the auth -> setup -> track navigation seam that unit tests don't reach.
library(testthat)
suppressMessages({
  library(shiny); library(bslib); library(htmltools)
  library(jsonlite); library(DBI); library(httr2); library(uuid)
})
# Mirror global.R's load order: rule_presets.R reads STANDARD_COED_FIELDING (from
# rules_engine.R) at source time, so a naive alphabetical sweep breaks (rule_p < rule_s).
.r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
.r_first <- file.path("R", c("brand_colors.R", "app_config.R", "rules_engine.R"))
for (f in c(.r_first, setdiff(.r_files, .r_first))) source(f)
rm(.r_files, .r_first)

test_that("continue-as-guest wires storage and navigates without a nav error", {
  testServer(bookworm_server, {
    session$flushReact()
    session$setInputs(`auth-do_guest` = 1)   # click "Continue as guest"
    # The identity observer must run to completion: it calls nav_select("screen",
    # "setup") — a bogus nav_hide() here previously crashed with "argument target
    # missing". If it still threw, store() would remain NULL.
    expect_false(is.null(store()))
    expect_true(is.function(store()$create_game))
    expect_true(is.function(store()$append_event))
  })
})

test_that("starting a game creates a game and advances to tracking", {
  testServer(bookworm_server, {
    session$flushReact()
    session$setInputs(`auth-do_guest` = 1)
    # Provide the setup form inputs, then click Start.
    session$setInputs(
      `setup-start_balls` = 1, `setup-start_strikes` = 1,
      `setup-foul_out` = "out", `setup-gender_rule` = "none", `setup-gender_n` = 2,
      `setup-min_females` = 0, `setup-innings` = 7, `setup-run_cap` = 0,
      `setup-mercy_diff` = 0, `setup-away_name` = "Away", `setup-home_name` = "Home"
    )
    session$setInputs(`setup-start` = 1)      # click "Start game"
    # The game_start observer must create a game and init tracking without error.
    lg <- store()$list_games()
    expect_equal(nrow(lg), 1L)
  })
})
