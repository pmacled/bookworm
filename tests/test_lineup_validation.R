library(testthat)
for (f in c(
  "app_config.R",
  "rules_engine.R",
  "rule_presets.R",
  "game_events.R",
  "lineup_validation.R"
)) {
  source(file.path("R", f))
}

pl <- function(
  id,
  name,
  gender,
  jersey = NA_integer_,
  slot = NA_integer_,
  pos = NA_character_
) {
  make_player(id, name, gender, jersey, slot, pos)
}

lineup_of <- function(...) {
  ps <- list(...)
  lapply(seq_along(ps), function(i) {
    p <- ps[[i]]
    p$order_slot <- i
    p
  })
}

codes <- function(r) vapply(r$items, function(i) i$code, character(1))
msgs <- function(r) vapply(r$items, function(i) i$message, character(1))

test_that("every item carries a severity, a code and a message", {
  # Codes are what main's notice-dedupe machinery keys on; items without them
  # cannot be routed through it.
  cfg <- preset_ruleset("gameon_summer")
  lu <- lineup_of(
    pl("1", "A", "M", 7L, pos = "P"),
    pl("2", "B", "M", 7L, pos = "C"),
    pl("3", "C", "M", 3L, pos = "SS"),
    pl("4", "D", "M", 4L, pos = "LF")
  )
  r <- validate_lineup(cfg, lu, "Away")
  expect_gt(length(r$items), 0L)
  for (i in r$items) {
    expect_true(i$severity %in% c("violation", "notice"))
    expect_true(is.character(i$code) && nzchar(i$code))
    expect_true(is.character(i$message) && nzchar(i$message))
    expect_named(i, c("severity", "code", "message"))
  }
})

test_that("an empty lineup is legal and reported as run-only", {
  r <- validate_lineup(preset_ruleset("anything_goes"), list(), "Away")
  expect_true(r$ok)
  expect_equal(codes(r), "run_only")
  expect_equal(
    msgs(r),
    "Away has no lineup — it will be tracked by runs per inning."
  )
})

test_that("batting_size is a maximum: a short lineup is fine, an oversized one warns", {
  cfg <- preset_ruleset("standard_baseball") # batting_size 9
  # Under a size-9 rule, 2 batters is allowed (short lineups are always legal).
  short <- validate_lineup(
    cfg,
    lineup_of(pl("1", "A", "M"), pl("2", "B", "M")),
    "Away"
  )
  expect_false("batting_size" %in% codes(short))
  expect_true(short$ok)
  # Ten batters exceeds the maximum, so it earns a notice (but still does not block).
  big_lu <- do.call(
    lineup_of,
    lapply(1:10, function(i) pl(as.character(i), LETTERS[i], "M"))
  )
  big <- validate_lineup(cfg, big_lu, "Away")
  expect_equal(codes(big), "batting_size")
  expect_equal(msgs(big), "10 batters entered; this ruleset allows at most 9.")
  expect_equal(big$items[[1]]$severity, "notice")
  expect_true(big$ok)
})

test_that("duplicate jersey numbers are flagged", {
  lu <- lineup_of(pl("1", "A", "M", 7L), pl("2", "B", "F", 7L))
  r <- validate_lineup(preset_ruleset("anything_goes"), lu, "Away")
  expect_equal(codes(r), "duplicate_jersey")
  expect_equal(msgs(r), "Jersey number 7 is used more than once.")
})

test_that("blank jerseys are not treated as duplicates", {
  lu <- lineup_of(pl("1", "A", "M"), pl("2", "B", "F"))
  r <- validate_lineup(preset_ruleset("anything_goes"), lu, "Away")
  expect_false("duplicate_jersey" %in% codes(r))
})

test_that("duplicate names are flagged, reported in the first entry's casing", {
  lu <- lineup_of(pl("1", "Sam", "M"), pl("2", "sam", "F"))
  r <- validate_lineup(preset_ruleset("anything_goes"), lu, "Away")
  expect_equal(codes(r), "duplicate_name")
  expect_equal(msgs(r), "More than one player is named \"Sam\".")
})

test_that("a batting-order gender violation names the offending slot", {
  cfg <- preset_ruleset("gameon_summer") # max 2 males in a row
  lu <- lineup_of(
    pl("1", "A", "M"),
    pl("2", "B", "M"),
    pl("3", "C", "M"),
    pl("4", "D", "F")
  )
  r <- validate_lineup(cfg, lu, "Away")
  expect_false(r$ok)
  expect_equal(codes(r), "batting_gender_order")
  expect_equal(msgs(r), "Batting order: C (slot 3) breaks the gender rule.")
})

test_that("a forward gender break is NOT also reported as a wrap-around break", {
  # Regression: `seen2` accumulated the whole doubled sequence, so the FORWARD
  # break recurred at an index past length(g) and was re-reported as a wrap
  # break. On M M M F the user got two violations, the second claiming
  # "slot 4 back to slot 1" -- but slot 4 -> slot 1 is F -> M, perfectly legal.
  cfg <- preset_ruleset("gameon_summer")
  lu <- lineup_of(
    pl("1", "A", "M"),
    pl("2", "B", "M"),
    pl("3", "C", "M"),
    pl("4", "D", "F")
  )
  r <- validate_lineup(cfg, lu, "Away")
  expect_length(
    Filter(function(i) identical(i$severity, "violation"), r$items),
    1L
  )
  expect_false(any(grepl("wrap", msgs(r), ignore.case = TRUE)))
})

test_that("the batting order wraps when checking the gender rule", {
  cfg <- preset_ruleset("gameon_summer")
  # M F M M reads fine forwards, but wrapping gives ... M M | M F -> three males.
  lu <- lineup_of(
    pl("1", "A", "M"),
    pl("2", "B", "F"),
    pl("3", "C", "M"),
    pl("4", "D", "M")
  )
  r <- validate_lineup(cfg, lu, "Away")
  expect_equal(codes(r), "batting_gender_wrap")
  expect_match(msgs(r), "wraps around", fixed = TRUE)
  expect_match(msgs(r), "A (slot 1)", fixed = TRUE) # A is the batter who breaks it
})

test_that("a break that exists only around the turn is still caught", {
  # M M F M M reads fine forwards; only the wrap (slot 4, slot 5 then slot 1)
  # gives three males. Seeding the wrap window from tail(g, n) must not lose this.
  cfg <- preset_ruleset("gameon_summer")
  lu <- lineup_of(
    pl("1", "A", "M"),
    pl("2", "B", "M"),
    pl("3", "C", "F"),
    pl("4", "D", "M"),
    pl("5", "E", "M")
  )
  r <- validate_lineup(cfg, lu, "Away")
  expect_equal(codes(r), "batting_gender_wrap")
  expect_match(msgs(r), "A (slot 1)", fixed = TRUE)
})

test_that("an all-male order breaks both forwards and around the turn", {
  # Not a false positive: M M M M really is broken at the wrap too.
  cfg <- preset_ruleset("gameon_summer")
  lu <- lineup_of(
    pl("1", "A", "M"),
    pl("2", "B", "M"),
    pl("3", "C", "M"),
    pl("4", "D", "M")
  )
  r <- validate_lineup(cfg, lu, "Away")
  expect_equal(codes(r), c("batting_gender_order", "batting_gender_wrap"))
})

test_that("fielding violations from the engine are included, codes intact", {
  cfg <- preset_ruleset("gameon_summer") # min 4 females
  lu <- lineup_of(
    pl("1", "A", "M", 1L, 1L, "P"),
    pl("2", "B", "M", 2L, 2L, "C"),
    pl("3", "C", "M", 3L, 3L, "SS"),
    pl("4", "D", "M", 4L, 4L, "LF")
  )
  r <- validate_lineup(cfg, lu, "Away")
  expect_false(r$ok)
  expect_true("min_females" %in% codes(r))
  hit <- Filter(function(i) identical(i$code, "min_females"), r$items)
  expect_equal(
    hit[[1]]$message,
    "Need at least 4 females in the field (have 0)."
  )
})

test_that("fielder_count is a maximum: short is fine, oversized warns", {
  cfg <- preset_ruleset("standard_baseball") # fielder_count 9
  short <- validate_lineup(
    cfg,
    lineup_of(
      pl("1", "A", "M", 1L, 1L, "P"),
      pl("2", "B", "M", 2L, 2L, "C")
    ),
    "Away"
  )
  expect_false("fielder_count" %in% codes(short))

  pos <- c("P", "C", "1B", "2B", "3B", "SS", "LF", "CF", "RF", "OF") # 10 fielding positions
  big_lu <- do.call(
    lineup_of,
    lapply(1:10, function(i) {
      pl(as.character(i), LETTERS[i], "M", i, i, pos[i])
    })
  )
  big <- validate_lineup(cfg, big_lu, "Away")
  hit <- Filter(function(i) identical(i$code, "fielder_count"), big$items)
  expect_length(hit, 1L)
  expect_equal(hit[[1]]$severity, "notice")
  expect_equal(
    hit[[1]]$message,
    "10 fielders assigned; this ruleset allows at most 9."
  )
})
