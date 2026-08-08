library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

mk <- function(prefix, genders, positions = NA_character_) {
  positions <- rep(positions, length.out = length(genders))
  lapply(seq_along(genders), function(i)
    make_player(paste0(prefix,i), paste(prefix,i), genders[i], i, i, positions[i]))
}
has_code <- function(w, code) any(vapply(w, function(x) identical(x$code, code), logical(1)))

test_that("warnings is a list of structured items and flags batting gender order", {
  cfg <- coerce_ruleset_config(list(batting_gender_rule = list(type = "no_two_males_consecutive")))
  lu <- mk("m", c("M","M"))
  s <- fold_events(list(
    new_event("game_start", list(ruleset = cfg, first_bat = "away",
      home = list(team_id="H", name="H", lineup = lu),
      away = list(team_id="A", name="A", lineup = lu)), seq = 1L),
    new_event("plate_appearance", list(team="away", batter_id="m1", outcome="1B",
      reached=1L, rbi=0L, outs_on_play=0L, advances=list()), seq = 2L)))
  expect_true(is.list(s$warnings))
  expect_true(has_code(s$warnings, "batting_gender"))
  expect_true(all(vapply(s$warnings, function(x) x$severity %in% c("violation","notice"), logical(1))))
})

test_that("a fielding violation surfaces as a warning item during defense", {
  cfg <- coerce_ruleset_config(list(fielding = STANDARD_COED_FIELDING))
  # away bats (top); home is the defense with an all-male positioned defense.
  # Fold a plate appearance too: game_start does NOT run .refresh_flags, so a
  # non-game_start event is needed for warnings to compute.
  home <- mk("h", rep("M", 4), positions = c("P","C","SS","LF"))
  away <- mk("a", c("M","F"))
  s <- fold_events(list(
    new_event("game_start", list(ruleset = cfg, first_bat = "away",
      home = list(team_id="H", name="H", lineup = home),
      away = list(team_id="A", name="A", lineup = away)), seq = 1L),
    new_event("plate_appearance", list(team="away", batter_id="a1", outcome="1B",
      reached=1L, rbi=0L, outs_on_play=0L, advances=list()), seq = 2L)))
  expect_true(has_code(s$warnings, "min_females"))  # 0 females on the home defense
})

test_that("a fielder-count mismatch is a notice, not a violation", {
  cfg <- coerce_ruleset_config(list(fielding = list(fielder_count = 9L)))
  lu <- list(make_player("h1","H1","M",1L,1L,"P"), make_player("h2","H2","F",2L,2L,"C"))
  start <- new_event("game_start", list(ruleset = cfg, first_bat = "away",
    home = list(team_id="H", name="Home", lineup = lu),
    away = list(team_id="A", name="Away", lineup = lu)), seq = 1L)
  s <- fold_events(list(start))
  hit <- Filter(function(w) identical(w$code, "fielder_count"), s$warnings)
  expect_length(hit, 1L)
  expect_equal(hit[[1]]$severity, "notice")
})

test_that("a lineup with no positions assigned raises no fielder-count notice", {
  cfg <- coerce_ruleset_config(list(fielding = list(fielder_count = 9L)))
  lu <- list(make_player("h1","H1","M",1L,1L,NA_character_))
  start <- new_event("game_start", list(ruleset = cfg, first_bat = "away",
    home = list(team_id="H", name="Home", lineup = lu),
    away = list(team_id="A", name="Away", lineup = lu)), seq = 1L)
  s <- fold_events(list(start))
  expect_length(Filter(function(w) identical(w$code, "fielder_count"), s$warnings), 0L)
})
