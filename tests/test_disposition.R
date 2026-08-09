library(testthat)
for (f in c(
  "app_config.R",
  "rules_engine.R",
  "rule_presets.R",
  "rule_home_run.R",
  "game_events.R",
  "game_reducer.R",
  "disposition.R"
)) {
  source(file.path("R", f))
}

mk <- function(prefix, n = 4L) {
  lapply(seq_len(n), function(i) {
    make_player(
      paste0(prefix, i),
      paste(prefix, i),
      c("M", "F")[(i %% 2) + 1L],
      i,
      i,
      i
    )
  })
}

st0 <- function(cfg = default_ruleset_config()) {
  s <- initial_game_state(cfg)
  s$lineups$away <- mk("a")
  s$lineups$home <- mk("h")
  s$batting_team <- "away"
  s$current_batter <- s$lineups$away[[1]]
  s
}

test_that("rows are lead-runner-first with the batter last", {
  s <- st0()
  s$bases <- list(first = "a4", second = NA_character_, third = "a3")
  rows <- disposition_rows(s)
  expect_equal(
    vapply(rows, function(r) r$runner_id, character(1)),
    c("a3", "a4", "a1")
  )
  expect_equal(vapply(rows, function(r) r$from, integer(1)), c(3L, 1L, 0L))
})

test_that("with the bases empty only the batter appears", {
  rows <- disposition_rows(st0())
  expect_length(rows, 1L)
  expect_equal(rows[[1]]$from, 0L)
})

test_that("prefill on a single matches suggest_advances", {
  s <- st0()
  s$bases$first <- "a4"
  pf <- disposition_prefill(s, "1B")
  expect_equal(unname(pf[["a4"]]), "2") # forced to second
  expect_equal(unname(pf[["a1"]]), "1") # batter to first
})

test_that("prefill on a home run scores everyone", {
  s <- st0()
  s$bases <- list(first = "a4", second = "a3", third = "a2")
  pf <- disposition_prefill(s, "HR")
  expect_true(all(pf == "H"))
})

test_that("prefill on a strikeout holds the runners and outs the batter", {
  s <- st0()
  s$bases$second <- "a3"
  pf <- disposition_prefill(s, "K")
  expect_equal(unname(pf[["a3"]]), "2") # holds
  expect_equal(unname(pf[["a1"]]), "OUT")
})

test_that("prefill treats a sacrifice as an out for the batter", {
  s <- st0()
  s$bases$first <- "a4"
  expect_equal(unname(disposition_prefill(s, "SF")[["a1"]]), "OUT")
  expect_equal(unname(disposition_prefill(s, "SAC")[["a1"]]), "OUT")
})

test_that("a walk only forces the runners who must move", {
  s <- st0()
  s$bases <- list(first = "a4", second = NA_character_, third = "a2")
  pf <- disposition_prefill(s, "BB")
  expect_equal(unname(pf[["a4"]]), "2") # forced
  expect_equal(unname(pf[["a2"]]), "3") # not forced, holds third
})

test_that("validation rejects two runners on the same base", {
  s <- st0()
  s$bases$first <- "a4"
  rows <- disposition_rows(s)
  r <- validate_disposition(rows, c(a4 = "2", a1 = "2"))
  expect_false(r$ok)
  expect_match(paste(r$errors, collapse = " "), "same base|second")
})

test_that("two runners may both score and both be out", {
  s <- st0()
  s$bases$first <- "a4"
  rows <- disposition_rows(s)
  expect_true(validate_disposition(rows, c(a4 = "H", a1 = "H"))$ok)
  expect_true(validate_disposition(rows, c(a4 = "OUT", a1 = "OUT"))$ok)
})

test_that("validation rejects a runner moving backwards", {
  s <- st0()
  s$bases$third <- "a2"
  rows <- disposition_rows(s)
  r <- validate_disposition(rows, c(a2 = "1", a1 = "1"))
  expect_false(r$ok)
  expect_match(paste(r$errors, collapse = " "), "back")
})

test_that("a runner holding their base is fine", {
  s <- st0()
  s$bases$third <- "a2"
  rows <- disposition_rows(s)
  expect_true(validate_disposition(rows, c(a2 = "3", a1 = "1"))$ok)
})

test_that("validation requires a choice for every runner", {
  s <- st0()
  s$bases$first <- "a4"
  rows <- disposition_rows(s)
  r <- validate_disposition(rows, c(a1 = "1"))
  expect_false(r$ok)
  expect_match(paste(r$errors, collapse = " "), "a4|every runner|a 4")
})

test_that("validation rejects an unknown level", {
  rows <- disposition_rows(st0())
  expect_false(validate_disposition(rows, c(a1 = "5"))$ok)
})

test_that("payload derives outs, runs, and the RBI default", {
  s <- st0()
  s$bases <- list(first = "a4", second = NA_character_, third = "a2")
  p <- disposition_payload(s, "1B", c(a2 = "H", a4 = "OUT", a1 = "1"))
  expect_equal(p$outs_on_play, 1L)
  expect_equal(p$rbi, 1L)
  expect_equal(p$reached, 1L)
  expect_equal(p$batter_id, "a1")
  expect_equal(p$team, "away")
  expect_length(p$advances, 3L)
  scored <- Filter(function(a) isTRUE(a$scored), p$advances)
  expect_equal(scored[[1]]$runner_id, "a2")
})

test_that("an explicit RBI overrides the derived default", {
  s <- st0()
  s$bases$third <- "a2"
  p <- disposition_payload(s, "E", c(a2 = "H", a1 = "1"), rbi = 0L)
  expect_equal(p$rbi, 0L)
})

test_that("an out batter has reached NA and is not placed on a base", {
  s <- st0()
  p <- disposition_payload(s, "K", c(a1 = "OUT"))
  expect_true(is.na(p$reached))
  expect_equal(p$outs_on_play, 1L)
  s2 <- apply_plate_appearance(s, list(payload = p))
  expect_true(is.na(s2$bases$first))
})

test_that("the payload round-trips through the reducer", {
  s <- st0()
  s$bases$first <- "a4"
  p <- disposition_payload(s, "2B", c(a4 = "H", a1 = "2"))
  s2 <- apply_plate_appearance(s, list(payload = p))
  expect_equal(s2$bases$second, "a1")
  expect_true(is.na(s2$bases$first))
  expect_equal(s2$score$away, 1L)
})

test_that("resolve_outcome rewrites a home run past the limit", {
  cfg <- coerce_ruleset_config(list(
    home_run_rule = list(over_fence_limit = 1L)
  ))
  s <- st0(cfg)
  s$pa_log <- list(list(team = "away", outcome = "HR", batter_id = "a2"))
  r <- resolve_outcome(cfg, s, "HR")
  expect_equal(r$outcome, "GO")
  expect_equal(r$warning$code, "home_run_limit")
})

test_that("resolve_outcome leaves a normal outcome alone", {
  cfg <- default_ruleset_config()
  r <- resolve_outcome(cfg, st0(cfg), "1B")
  expect_equal(r$outcome, "1B")
  expect_null(r$warning)
})
