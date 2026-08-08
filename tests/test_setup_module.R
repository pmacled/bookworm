library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","setup_module.R"))
  source(file.path("R", f))

test_that("build_game_start_event assembles a valid event", {
  home <- list(team_id="H", name="Home",
    lineup = list(make_player("h1","H1","M",1L,1L,6L)))
  away <- list(team_id="A", name="Away",
    lineup = list(make_player("a1","A1","F",1L,1L,4L)))
  evt <- build_game_start_event(default_ruleset_config(), home, away, "away")
  expect_equal(evt$type, "game_start")
  expect_equal(evt$payload$first_bat, "away")
  expect_true(validate_event(evt)$ok)
})

test_that("collect_lineup reads rows, skips blanks, assigns order_slot", {
  input <- list(
    t_name_1 = "Sam", t_gender_1 = "F", t_jersey_1 = 9, t_pos_1 = "SS",
    t_name_2 = "",    t_gender_2 = "M", t_jersey_2 = NA, t_pos_2 = "",   # blank name -> skipped
    t_name_3 = "Mo",  t_gender_3 = "M", t_jersey_3 = NA, t_pos_3 = ""    # blank jersey -> 0
  )
  lu <- collect_lineup(input, "t", c(1,2,3))
  expect_equal(length(lu), 2L)
  expect_equal(lu[[1]]$name, "Sam"); expect_equal(lu[[1]]$order_slot, 1L)
  expect_equal(lu[[1]]$position, "SS"); expect_equal(lu[[1]]$jersey_number, 9L)
  expect_equal(lu[[2]]$name, "Mo"); expect_equal(lu[[2]]$order_slot, 2L)
  expect_equal(lu[[2]]$jersey_number, 0L); expect_true(is.na(lu[[2]]$position))
})

test_that("collect_lineup returns an empty list when no rows have names", {
  expect_equal(length(collect_lineup(list(), "t", integer())), 0L)
})

test_that("build_game_start_event accepts an empty lineup (run-only team)", {
  home <- list(team_id="H", name="Home", lineup = list())  # empty
  away <- list(team_id="A", name="Away", lineup = list(make_player("a1","A1","F",1L,1L,"SS")))
  evt <- build_game_start_event(default_ruleset_config(), home, away, "away")
  expect_true(validate_event(evt)$ok)
  expect_equal(length(evt$payload$home$lineup), 0L)
})

test_that("setup_server produces a game_start with a run-only home team", {
  library(shiny)
  testServer(setup_server, {
    session$flushReact()                             # warm-up: see Task 16 note re
    # testServer's ignoreInit-observer-fires-on-first-setInputs quirk (test-harness
    # artifact only; not a production bug)
    session$setInputs(away_add = 1)                 # one away row
    # Fill the away row (id 1), leave home empty, set required rule inputs, start.
    session$setInputs(away_name_1 = "Sam", away_gender_1 = "F", away_jersey_1 = 9, away_pos_1 = "SS",
      away_name = "Away", home_name = "Home", start_balls = 1, start_strikes = 1,
      foul_out = "out", batting_size = "0", gender_rule = "none", innings = 7,
      run_cap = 0, mercy_diff = 0, fielding_preset = "none", start = 1)
    gs <- session$returned()
    expect_equal(gs$type, "game_start")
    expect_equal(length(gs$payload$away$lineup), 1L)
    expect_equal(length(gs$payload$home$lineup), 0L)   # run-only home
  })
})
