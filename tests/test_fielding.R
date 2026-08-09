library(testthat)
source(file.path("R", "app_config.R"))
source(file.path("R", "game_events.R"))
source(file.path("R", "rules_engine.R"))

fld <- function(cfg_fielding) { c <- default_ruleset_config(); c$fielding <- cfg_fielding; c }
pl <- function(g, pos) make_player(paste(g, pos), paste(g, pos), g, position = pos)
codes <- function(v) vapply(v, function(x) x$code, character(1))

test_that("no positions assigned => no fielding evaluation (no false alarms)", {
  cfg <- fld(STANDARD_COED_FIELDING)
  lineup <- list(make_player("a","A","M"), make_player("b","B","F"))  # no positions
  expect_equal(length(evaluate_fielding(cfg, lineup)), 0L)
})

test_that("standard coed: a legal 4-female defense passes", {
  cfg <- fld(STANDARD_COED_FIELDING)
  d <- list(
    pl("F","P"), pl("M","C"),                    # battery opposite (one F)
    pl("F","SS"), pl("M","1B"), pl("M","2B"), pl("M","3B"),  # infield has 1 F
    pl("F","LF"), pl("M","LCF"), pl("M","RCF"), pl("F","RF") # outfield has 2 F
  )  # F total = 4, M total = 6
  expect_equal(length(evaluate_fielding(cfg, d)), 0L)
})

test_that("standard coed: too many males and no outfield female both flag", {
  cfg <- fld(STANDARD_COED_FIELDING)
  d <- list(
    pl("M","P"), pl("M","C"),
    pl("F","SS"), pl("M","1B"), pl("M","2B"), pl("M","3B"),
    pl("M","LF"), pl("M","LCF"), pl("M","RCF"), pl("F","RF")
  )  # F=2 (< min 4), M=8 (> 6), outfield F=1 ok, infield F=1 ok, battery both M (tier "one" -> violation)
  cd <- codes(evaluate_fielding(cfg, d))
  expect_true("min_females" %in% cd)
  expect_true("max_males" %in% cd)
  expect_true("battery_opposite" %in% cd)
})

test_that("tier at 5 females requires 2 outfield + 2 infield", {
  cfg <- fld(STANDARD_COED_FIELDING)
  d <- list(
    pl("F","P"), pl("F","C"),                                # battery both F (also flags battery_opposite; unchecked)
    pl("F","SS"), pl("F","1B"), pl("M","2B"), pl("M","3B"),  # infield F=2
    pl("F","LF"), pl("M","LCF"), pl("M","RCF"), pl("M","RF") # outfield F=1  (needs 2 at 5F)
  )  # F total = 5 (P,C,SS,1B,LF)
  cd <- codes(evaluate_fielding(cfg, d))
  expect_true("outfield_min" %in% cd)
  expect_false("infield_min" %in% cd)
})

# --- Finding 3: an NA threshold (a cleared numericInput) must not crash the reducer ---

legal_10 <- function() list(
  pl("F","P"), pl("M","C"),
  pl("F","SS"), pl("M","1B"), pl("M","2B"), pl("M","3B"),
  pl("F","LF"), pl("M","LCF"), pl("M","RCF"), pl("F","RF"))

test_that("an NA min_females is treated as no minimum, not as an if(NA) crash", {
  # A cleared "Min females in field" box persists NA into the ruleset, and
  # `Ftot < NA` is NA -- so `if (Ftot < minf)` errored on EVERY event thereafter,
  # which (because loading re-folds the whole event list) bricked the game.
  cfg <- fld(list(fielder_count = NA_integer_, min_females = NA_integer_,
                  max_males = NA_integer_, tiers = list(),
                  position_requirements = list()))
  expect_equal(length(evaluate_fielding(cfg, legal_10())), 0L)
})

test_that("an NA tier outfield/infield minimum is skipped, not an if(NA) crash", {
  cfg <- fld(list(fielder_count = NA_integer_, min_females = 0L, max_males = NA_integer_,
                  tiers = list(list(females = 0L, outfield = NA_integer_,
                                    infield = NA_integer_, battery = "any")),
                  position_requirements = list()))
  expect_equal(length(evaluate_fielding(cfg, legal_10())), 0L)
})

test_that("6+ females relaxes battery (both may be female)", {
  cfg <- fld(STANDARD_COED_FIELDING)
  d <- list(
    pl("F","P"), pl("F","C"),                                # both female
    pl("F","SS"), pl("F","1B"), pl("M","2B"), pl("M","3B"),
    pl("F","LF"), pl("F","LCF"), pl("M","RCF"), pl("M","RF")
  )  # F total = 6 -> tier battery "any"
  expect_false("battery_opposite" %in% codes(evaluate_fielding(cfg, d)))
})
