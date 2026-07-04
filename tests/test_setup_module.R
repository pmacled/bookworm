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
