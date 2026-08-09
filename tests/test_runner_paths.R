library(testthat)
for (f in c(
  "app_config.R",
  "rules_engine.R",
  "rule_presets.R",
  "game_events.R",
  "game_reducer.R",
  "runner_paths.R"
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
start_evt <- function() {
  new_event(
    "game_start",
    list(
      ruleset = default_ruleset_config(),
      first_bat = "away",
      home = list(team_id = "H", name = "Home", lineup = mk("h")),
      away = list(team_id = "A", name = "Away", lineup = mk("a"))
    ),
    seq = 1L
  )
}
pa <- function(
  batter,
  outcome,
  advances,
  outs = 0L,
  rbi = 0L,
  reached = NA_integer_,
  seq = 2L
) {
  new_event(
    "plate_appearance",
    list(
      team = "away",
      batter_id = batter,
      outcome = outcome,
      reached = reached,
      rbi = rbi,
      outs_on_play = outs,
      advances = advances
    ),
    seq = seq
  )
}
A <- function(id, from, to, ...) make_advance(id, from, to, ...)

test_that("a batter who singles has reached one base and has not scored", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", list(A("a1", 0L, 1L)), reached = 1L)
  ))
  p <- runner_paths(s)
  expect_length(p, 1L)
  expect_equal(p[[1]]$bases_reached, 1L)
  expect_false(p[[1]]$scored)
  expect_true(is.na(p[[1]]$out_number))
})

test_that("a runner's later advances are credited to their own cell", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", list(A("a1", 0L, 1L)), reached = 1L, seq = 2L),
    pa(
      "a2",
      "BB",
      list(A("a1", 1L, 2L), A("a2", 0L, 1L)),
      reached = 1L,
      seq = 3L
    ),
    pa(
      "a3",
      "GO",
      list(A("a1", 2L, 3L), A("a2", 1L, 2L), A("a3", 0L, 0L, out = TRUE)),
      outs = 1L,
      seq = 4L
    ),
    pa(
      "a4",
      "SF",
      list(
        A("a1", 3L, 4L, scored = TRUE),
        A("a2", 2L, 2L),
        A("a4", 0L, 0L, out = TRUE)
      ),
      outs = 1L,
      rbi = 1L,
      seq = 5L
    )
  ))
  p <- runner_paths(s)
  expect_equal(p[[1]]$bases_reached, 4L)
  expect_true(p[[1]]$scored)
  expect_equal(p[[2]]$bases_reached, 2L)
  expect_false(p[[2]]$scored)
  expect_equal(p[[3]]$bases_reached, 0L)
  expect_equal(p[[3]]$out_number, 1L)
  expect_equal(p[[4]]$out_number, 2L)
})

test_that("out numbers count up within a half and restart in the next", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "K", list(A("a1", 0L, 0L, out = TRUE)), outs = 1L, seq = 2L),
    pa("a2", "K", list(A("a2", 0L, 0L, out = TRUE)), outs = 1L, seq = 3L),
    pa("a3", "K", list(A("a3", 0L, 0L, out = TRUE)), outs = 1L, seq = 4L)
  ))
  p <- runner_paths(s)
  expect_equal(vapply(p, function(r) r$out_number, integer(1)), c(1L, 2L, 3L))
})

test_that("two outs on one play are numbered in advance order", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", list(A("a1", 0L, 1L)), reached = 1L, seq = 2L),
    pa(
      "a2",
      "GO",
      list(A("a1", 1L, 1L, out = TRUE), A("a2", 0L, 0L, out = TRUE)),
      outs = 2L,
      seq = 3L
    )
  ))
  p <- runner_paths(s)
  expect_equal(p[[1]]$out_number, 1L)
  expect_equal(p[[2]]$out_number, 2L)
})

test_that("a runner put out on the basepaths keeps the bases they reached", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "2B", list(A("a1", 0L, 2L)), reached = 2L, seq = 2L),
    pa(
      "a2",
      "GO",
      list(A("a1", 2L, 2L, out = TRUE), A("a2", 0L, 1L)),
      outs = 1L,
      reached = 1L,
      seq = 3L
    )
  ))
  p <- runner_paths(s)
  expect_equal(p[[1]]$bases_reached, 2L)
  expect_equal(p[[1]]$out_at, 2L)
  expect_equal(p[[1]]$out_number, 1L)
})

test_that("a pinch runner's run is credited to the original batter's cell", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", list(A("a1", 0L, 1L)), reached = 1L, seq = 2L),
    new_event(
      "substitution",
      list(
        team = "away",
        kind = "courtesy_runner",
        out_player_id = "a1",
        in_player = make_player(
          "a9",
          "Sub",
          "M",
          9L,
          NA_integer_,
          NA_character_
        )
      ),
      seq = 3L
    ),
    pa(
      "a2",
      "HR",
      list(A("a9", 1L, 4L, scored = TRUE), A("a2", 0L, 4L, scored = TRUE)),
      reached = 4L,
      rbi = 2L,
      seq = 4L
    )
  ))
  p <- runner_paths(s)
  expect_true(p[[1]]$scored)
  expect_equal(p[[1]]$bases_reached, 4L)
})

test_that("a legacy event with no advances falls back to reached", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "2B", list(), reached = 2L, seq = 2L)
  ))
  p <- runner_paths(s)
  expect_equal(p[[1]]$bases_reached, 2L)
  expect_false(p[[1]]$scored)
})

test_that("a legacy home run is marked as scored", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "HR", list(), reached = 4L, rbi = 1L)
  ))
  p <- runner_paths(s)
  expect_true(p[[1]]$scored)
  expect_equal(p[[1]]$bases_reached, 4L)
})

test_that("an advance with no origin_index is ignored rather than erroring", {
  s <- fold_events(list(start_evt()))
  s$pa_log <- list(list(
    team = "away",
    inning = 1L,
    half = "top",
    batter_id = "a1",
    outcome = "1B",
    fielding = NA_character_,
    rbi = 0L,
    reached = 1L,
    outs_before = 0L,
    pa_index_in_half = 1L,
    advances = list(list(
      runner_id = "ghost",
      from = 2L,
      to = 3L,
      scored = FALSE,
      out = FALSE,
      origin_index = NA_integer_
    ))
  ))
  expect_silent(p <- runner_paths(s))
  expect_length(p, 1L)
})

test_that("scorebook_layout splits only the innings where someone batted twice", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", list(A("a1", 0L, 1L)), reached = 1L, seq = 2L),
    pa(
      "a2",
      "1B",
      list(A("a1", 1L, 2L), A("a2", 0L, 1L)),
      reached = 1L,
      seq = 3L
    ),
    pa(
      "a3",
      "1B",
      list(A("a1", 2L, 3L), A("a2", 1L, 2L), A("a3", 0L, 1L)),
      reached = 1L,
      seq = 4L
    ),
    pa(
      "a4",
      "1B",
      list(
        A("a1", 3L, 4L, scored = TRUE),
        A("a2", 2L, 3L),
        A("a3", 1L, 2L),
        A("a4", 0L, 1L)
      ),
      reached = 1L,
      rbi = 1L,
      seq = 5L
    ),
    pa("a1", "K", list(A("a1", 0L, 0L, out = TRUE)), outs = 1L, seq = 6L)
  ))
  lay <- scorebook_layout(runner_paths(s), "away")
  expect_equal(unname(lay$sub_counts[["1"]]), 2L)
  a1_cells <- Filter(function(c) identical(c$batter_id, "a1"), lay$cells)
  expect_equal(vapply(a1_cells, function(c) c$sub_index, integer(1)), c(1L, 2L))
})

test_that("an inning with no repeat batter has one sub-column", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "K", list(A("a1", 0L, 0L, out = TRUE)), outs = 1L, seq = 2L)
  ))
  lay <- scorebook_layout(runner_paths(s), "away")
  expect_equal(unname(lay$sub_counts[["1"]]), 1L)
  expect_equal(lay$cells[[1]]$col, 0L)
})

test_that("layout for a team with no plate appearances is empty but valid", {
  lay <- scorebook_layout(list(), "home")
  expect_length(lay$cells, 0L)
  expect_equal(lay$innings, 0L)
})
