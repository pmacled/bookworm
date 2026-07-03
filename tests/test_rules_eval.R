library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

test_that("no_two_males_consecutive flags a male after a male", {
  cfg <- coerce_ruleset_config(list(batting_gender_rule = list(type = "no_two_males_consecutive")))
  expect_false(next_batter_gender_ok(cfg, prev_genders = c("M"), next_gender = "M"))
  expect_true(next_batter_gender_ok(cfg, prev_genders = c("M"), next_gender = "F"))
})

test_that("fielding_warnings triggers below min_females", {
  cfg <- coerce_ruleset_config(list(fielding = list(min_females = 4L)))
  defense <- lapply(1:9, function(i) make_player(paste0("d",i), "x", "M", i, i, i))
  expect_true(length(fielding_warnings(cfg, defense)) > 0)
})

test_that("run cap limits non-open innings", {
  cfg <- coerce_ruleset_config(list(run_cap_per_inning = 5L, innings = 7L, open_last_inning = TRUE))
  expect_equal(apply_run_cap(cfg, runs_this_half = 8L, inning = 3L), 5L)
  expect_equal(apply_run_cap(cfg, runs_this_half = 8L, inning = 7L), 8L)  # open last
})

test_that("mercy ends the game", {
  cfg <- coerce_ruleset_config(list(mercy_rule = list(differential = 10L, after_inning = 4L)))
  st <- list(inning = 5L, half = "top", score = list(home = 15L, away = 3L),
             ruleset = cfg, outs = 0L)
  expect_true(game_should_end(cfg, st))
})

test_that("reducer surfaces a gender-order warning for the batter due up", {
  cfg <- coerce_ruleset_config(list(batting_gender_rule = list(type = "no_two_males_consecutive")))
  lineup <- list(make_player("m1","M1","M",1L,1L,6L), make_player("m2","M2","M",2L,2L,4L))
  start <- new_event("game_start", list(ruleset = cfg, first_bat = "away",
    home = list(team_id="H", name="Home", lineup = lineup),
    away = list(team_id="A", name="Away", lineup = lineup)), seq = 1L)
  pa1 <- new_event("plate_appearance", list(team="away", batter_id="m1", outcome="1B",
    reached=1L, rbi=0L, outs_on_play=0L, advances=list()), seq = 2L)
  s <- fold_events(list(start, pa1))   # m2 (M) now due up after m1 (M)
  expect_true(any(grepl("gender", s$warnings, ignore.case = TRUE)))
})

test_that("mercy with differential but no after_inning does not crash and can end", {
  cfg <- coerce_ruleset_config(list(mercy_rule = list(differential = 10L)))  # after_inning stays NA
  st <- list(inning = 3L, half = "top", score = list(home = 15L, away = 3L),
             ruleset = cfg, outs = 0L)
  expect_true(game_should_end(cfg, st))   # must not error
})
