library(testthat)
for (f in c("app_config.R","rules_engine.R","rule_presets.R","game_events.R",
            "lineup_validation.R"))
  source(file.path("R", f))

pl <- function(id, name, gender, jersey = NA_integer_, slot = NA_integer_, pos = NA_character_)
  make_player(id, name, gender, jersey, slot, pos)

lineup_of <- function(...) {
  ps <- list(...)
  lapply(seq_along(ps), function(i) { p <- ps[[i]]; p$order_slot <- i; p })
}

test_that("an empty lineup is legal and reported as run-only", {
  r <- validate_lineup(preset_ruleset("anything_goes"), list(), "Away")
  expect_true(r$ok)
  expect_match(paste(vapply(r$items, function(i) i$message, character(1))), "runs")
})

test_that("a batting-size mismatch is a notice, not a failure", {
  cfg <- preset_ruleset("standard_baseball")   # batting_size 9
  lu <- lineup_of(pl("1","A","M"), pl("2","B","M"))
  r <- validate_lineup(cfg, lu, "Away")
  msgs <- vapply(r$items, function(i) i$message, character(1))
  expect_true(any(grepl("9", msgs)))
  expect_true(all(vapply(r$items, function(i) i$severity != "violation", logical(1))))
  # A notice-severity item alone must not flip ok to FALSE -- only violations do.
  expect_true(r$ok)
})

test_that("duplicate jersey numbers are flagged", {
  lu <- lineup_of(pl("1","A","M", 7L), pl("2","B","F", 7L))
  msgs <- vapply(validate_lineup(preset_ruleset("anything_goes"), lu, "Away")$items,
                 function(i) i$message, character(1))
  expect_true(any(grepl("7", msgs)))
})

test_that("blank jerseys are not treated as duplicates", {
  lu <- lineup_of(pl("1","A","M"), pl("2","B","F"))
  msgs <- vapply(validate_lineup(preset_ruleset("anything_goes"), lu, "Away")$items,
                 function(i) i$message, character(1))
  expect_false(any(grepl("jersey", msgs, ignore.case = TRUE)))
})

test_that("duplicate names are flagged", {
  lu <- lineup_of(pl("1","Sam","M"), pl("2","Sam","F"))
  msgs <- vapply(validate_lineup(preset_ruleset("anything_goes"), lu, "Away")$items,
                 function(i) i$message, character(1))
  expect_true(any(grepl("Sam", msgs)))
})

test_that("a batting-order gender violation is reported as a violation", {
  cfg <- preset_ruleset("gameon_summer")   # max 2 males in a row
  lu <- lineup_of(pl("1","A","M"), pl("2","B","M"), pl("3","C","M"), pl("4","D","F"))
  r <- validate_lineup(cfg, lu, "Away")
  expect_false(r$ok)
  expect_true(any(vapply(r$items, function(i) identical(i$severity, "violation"), logical(1))))
})

test_that("the batting order wraps when checking the gender rule", {
  cfg <- preset_ruleset("gameon_summer")
  # M F M M reads fine forwards, but wrapping gives ... M M | M F -> three males.
  lu <- lineup_of(pl("1","A","M"), pl("2","B","F"), pl("3","C","M"), pl("4","D","M"))
  msgs <- vapply(validate_lineup(cfg, lu, "Away")$items, function(i) i$message, character(1))
  expect_true(any(grepl("wrap", msgs, ignore.case = TRUE)))
})

test_that("fielding violations from the engine are included", {
  cfg <- preset_ruleset("gameon_summer")   # min 4 females
  lu <- lineup_of(
    pl("1","A","M", 1L, 1L, "P"), pl("2","B","M", 2L, 2L, "C"),
    pl("3","C","M", 3L, 3L, "SS"), pl("4","D","M", 4L, 4L, "LF"))
  r <- validate_lineup(cfg, lu, "Away")
  expect_false(r$ok)
  msgs <- vapply(r$items, function(i) i$message, character(1))
  expect_true(any(grepl("female", msgs, ignore.case = TRUE)))
})

test_that("a fielder-count mismatch is a notice", {
  cfg <- preset_ruleset("standard_baseball")   # fielder_count 9
  lu <- lineup_of(pl("1","A","M", 1L, 1L, "P"), pl("2","B","M", 2L, 2L, "C"))
  items <- validate_lineup(cfg, lu, "Away")$items
  hit <- Filter(function(i) grepl("fielder", i$message, ignore.case = TRUE), items)
  expect_length(hit, 1L)
  expect_equal(hit[[1]]$severity, "notice")
})
