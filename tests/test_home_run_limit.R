library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","rule_home_run.R"))
  source(file.path("R", f))

hr_cfg <- function(...) coerce_ruleset_config(list(home_run_rule = list(...)))

st_with <- function(outcomes, team = "away") {
  st <- list(batting_team = team, pa_log = lapply(outcomes, function(o)
    list(team = team, outcome = o, batter_id = "a1")))
  st
}
bat <- function(gender = "M") make_player("a1", "A1", gender, 1L, 1L, "SS")

test_that("no limit means the outcome is never rewritten", {
  cfg <- hr_cfg()
  r <- evaluate_home_run_limit(cfg, st_with(rep("HR", 9)), bat(), "HR")
  expect_equal(r$outcome, "HR")
  expect_null(r$warning)
})

test_that("under the limit passes through", {
  cfg <- hr_cfg(over_fence_limit = 3L)
  r <- evaluate_home_run_limit(cfg, st_with(c("HR", "HR")), bat(), "HR")
  expect_equal(r$outcome, "HR")
  expect_null(r$warning)
})

test_that("at the limit the next over-the-fence home run becomes an out", {
  cfg <- hr_cfg(over_fence_limit = 3L, over_limit_result = "out")
  r <- evaluate_home_run_limit(cfg, st_with(c("HR", "HR", "HR")), bat(), "HR")
  expect_equal(r$outcome, "GO")
  expect_equal(r$warning$code, "home_run_limit")
  expect_equal(r$warning$severity, "notice")
})

test_that("one below the limit still passes through (boundary: >= not >)", {
  # over_fence_limit = 3 with exactly 2 prior HRs: 2 < 3, so this one must pass.
  # A buggy `>` comparison (already <= limit triggers replacement) would wrongly
  # rewrite this one too.
  cfg <- hr_cfg(over_fence_limit = 3L, over_limit_result = "out")
  r <- evaluate_home_run_limit(cfg, st_with(c("HR", "HR")), bat(), "HR")
  expect_equal(r$outcome, "HR")
  expect_null(r$warning)
})

test_that("over_limit_result can be a double or a single", {
  d <- hr_cfg(over_fence_limit = 1L, over_limit_result = "ground_rule_double")
  expect_equal(evaluate_home_run_limit(d, st_with("HR"), bat(), "HR")$outcome, "2B")
  s <- hr_cfg(over_fence_limit = 1L, over_limit_result = "single")
  expect_equal(evaluate_home_run_limit(s, st_with("HR"), bat(), "HR")$outcome, "1B")
})

test_that("inside-the-park home runs are exempt by default", {
  cfg <- hr_cfg(over_fence_limit = 1L)
  # An existing ITPHR does not count toward the total...
  r <- evaluate_home_run_limit(cfg, st_with(c("ITPHR", "ITPHR")), bat(), "HR")
  expect_equal(r$outcome, "HR")
  # ...and a new ITPHR is never rewritten, even at the limit.
  r2 <- evaluate_home_run_limit(cfg, st_with("HR"), bat(), "ITPHR")
  expect_equal(r2$outcome, "ITPHR")
  expect_null(r2$warning)
})

test_that("inside_park_counts makes ITPHR count toward the limit", {
  cfg <- hr_cfg(over_fence_limit = 2L, inside_park_counts = TRUE)
  r <- evaluate_home_run_limit(cfg, st_with(c("HR", "ITPHR")), bat(), "HR")
  expect_equal(r$outcome, "GO")
})

test_that("inside_park_counts = FALSE still never rewrites a new ITPHR at the limit", {
  # Guards against an inverted inside_park_counts flag that starts rewriting
  # ITPHR outcomes themselves instead of merely toggling whether past ones count.
  cfg <- hr_cfg(over_fence_limit = 1L, inside_park_counts = FALSE)
  r <- evaluate_home_run_limit(cfg, st_with("HR"), bat(), "ITPHR")
  expect_equal(r$outcome, "ITPHR")
  expect_null(r$warning)
})

test_that("a per-gender limit overrides the overall limit", {
  cfg <- hr_cfg(over_fence_limit = 5L, limit_by_gender = list(M = 1L))
  expect_equal(evaluate_home_run_limit(cfg, st_with("HR"), bat("M"), "HR")$outcome, "GO")
  expect_equal(evaluate_home_run_limit(cfg, st_with("HR"), bat("F"), "HR")$outcome, "HR")
})

test_that("a gender with no override falls back to the overall limit, not unlimited", {
  # Guards against an implementation that treats "no entry for this gender in
  # limit_by_gender" as "no limit at all" instead of falling back to over_fence_limit.
  cfg <- hr_cfg(over_fence_limit = 1L, limit_by_gender = list(M = 5L))
  r <- evaluate_home_run_limit(cfg, st_with("HR"), bat("F"), "HR")
  expect_equal(r$outcome, "GO")
})

test_that("only the batting team's home runs are counted", {
  cfg <- hr_cfg(over_fence_limit = 1L)
  st <- list(batting_team = "away",
             pa_log = list(list(team = "home", outcome = "HR", batter_id = "h1")))
  r <- evaluate_home_run_limit(cfg, st, bat(), "HR")
  expect_equal(r$outcome, "HR")
})

test_that("count_over_fence_home_runs counts only the given team, not both", {
  # Direct check on the counting helper: a mixed log must yield the count for
  # the requested team only, catching an implementation that sums every row.
  cfg <- hr_cfg()
  st <- list(batting_team = "away", pa_log = list(
    list(team = "away", outcome = "HR", batter_id = "a1"),
    list(team = "home", outcome = "HR", batter_id = "h1"),
    list(team = "home", outcome = "HR", batter_id = "h2")))
  expect_equal(count_over_fence_home_runs(cfg, st, "away"), 1L)
  expect_equal(count_over_fence_home_runs(cfg, st, "home"), 2L)
})

test_that("non-home-run outcomes pass straight through", {
  cfg <- hr_cfg(over_fence_limit = 0L)
  expect_equal(evaluate_home_run_limit(cfg, st_with(character()), bat(), "1B")$outcome, "1B")
})
