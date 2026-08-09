library(testthat)
for (f in c(
  "app_config.R",
  "rules_engine.R",
  "game_events.R",
  "game_reducer.R"
)) {
  source(file.path("R", f))
}
mk_lineup <- function(prefix, genders = c("M", "F", "M", "F")) {
  lapply(seq_along(genders), function(i) {
    make_player(paste0(prefix, i), paste(prefix, i), genders[i], i, i, i)
  })
}
start_evt <- function(ruleset = default_ruleset_config()) {
  new_event(
    "game_start",
    list(
      ruleset = ruleset,
      first_bat = "away",
      home = list(team_id = "H", name = "Home", lineup = mk_lineup("h")),
      away = list(team_id = "A", name = "Away", lineup = mk_lineup("a"))
    ),
    seq = 1L
  )
}
pa <- function(
  batter,
  outcome,
  reached,
  rbi = 0L,
  outs = 0L,
  advances = list(),
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

test_that("a single puts the batter on first", {
  s <- fold_events(list(start_evt(), pa("a1", "1B", 1L)))
  expect_equal(s$bases$first, "a1")
  expect_equal(s$outs, 0L)
})

test_that("home run with a runner on first scores two and adds RBIs", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", 1L, seq = 2L),
    pa(
      "a2",
      "HR",
      4L,
      rbi = 2L,
      seq = 3L,
      advances = list(
        make_advance("a1", 1L, 4L, scored = TRUE),
        make_advance("a2", 0L, 4L, scored = TRUE)
      )
    )
  ))
  expect_equal(s$score$away, 2L)
  expect_equal(s$runs_this_half, 2L)
  expect_true(is.na(s$bases$first))
})

test_that("pa_log grows and records bases_after", {
  s <- fold_events(list(start_evt(), pa("a1", "1B", 1L)))
  expect_equal(length(s$pa_log), 1L)
  expect_equal(s$pa_log[[1]]$bases_after$first, "a1")
})

test_that("suggest_advances forces the batter to first on a walk", {
  s <- fold_events(list(start_evt()))
  adv <- suggest_advances(s, "BB")
  expect_true(any(vapply(adv, function(a) a$to == 1L, logical(1))))
})

test_that("suggest_advances scores everyone on an inside-the-park home run, same as HR", {
  # ITPHR must bump runners 4 bases like HR. A reducer that doesn't recognize
  # the code falls through the switch's default of 0L and returns no advances
  # at all (as if nothing happened), which this distinguishes from the fix.
  s <- fold_events(list(start_evt(), pa("a1", "1B", 1L)))
  adv <- suggest_advances(s, "ITPHR")
  runner <- Filter(function(a) a$runner_id == "a1", adv)
  expect_equal(length(runner), 1L)
  expect_equal(runner[[1]]$to, 4L)
  expect_true(runner[[1]]$scored)
})

test_that("at the shipping default, a cap-crossing play ends the half mid-inning (cap_ends_half)", {
  rs <- coerce_ruleset_config(list(run_cap = list(per_inning = 3L)))
  expect_true(rs$run_cap$cap_ends_half) # shipping default
  # A grand slam (4 runs) against a cap of 3, with the bases loaded from the
  # start (no prior plays needed -- advances alone carry the runners home).
  s <- fold_events(list(
    start_evt(rs),
    pa(
      "a4",
      "HR",
      4L,
      rbi = 4L,
      advances = list(
        make_advance("a1", 1L, 4L, scored = TRUE),
        make_advance("a2", 2L, 4L, scored = TRUE),
        make_advance("a3", 3L, 4L, scored = TRUE)
      )
    )
  ))
  expect_equal(s$score$away, 4L) # the play in progress counted in full (grand slam)
  expect_equal(s$half, "bottom") # ...and the half ended even though outs = 0
  expect_equal(s$outs, 0L)
})

test_that("at the shipping default, once the cap is reached the next play in the half scores zero", {
  rs <- coerce_ruleset_config(list(
    run_cap = list(per_inning = 3L, cap_ends_half = FALSE)
  ))
  s <- fold_events(list(
    start_evt(rs),
    pa(
      "a4",
      "HR",
      4L,
      rbi = 4L,
      seq = 2L,
      advances = list(
        make_advance("a1", 1L, 4L, scored = TRUE),
        make_advance("a2", 2L, 4L, scored = TRUE),
        make_advance("a3", 3L, 4L, scored = TRUE)
      )
    ),
    pa("a2", "HR", 4L, seq = 3L)
  )) # a second, unrelated solo shot
  expect_equal(s$score$away, 4L) # first play (grand slam) counted in full;
  # the second play's run is stopped by the cap
  expect_equal(s$half, "top") # cap_ends_half = FALSE: half kept going
  expect_true(any(vapply(
    s$warnings,
    function(w) identical(w$code, "run_cap"),
    logical(1)
  )))
})

test_that("pa_log entries carry advances, pa_index_in_half, and outs_before", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", 1L, advances = list(make_advance("a1", 0L, 1L)), seq = 2L),
    pa(
      "a2",
      "K",
      NA_integer_,
      outs = 1L,
      advances = list(make_advance("a2", 0L, 0L, out = TRUE)),
      seq = 3L
    )
  ))
  expect_equal(
    vapply(s$pa_log, function(r) r$pa_index_in_half, integer(1)),
    c(1L, 2L)
  )
  expect_equal(
    vapply(s$pa_log, function(r) r$outs_before, integer(1)),
    c(0L, 0L)
  )
  expect_length(s$pa_log[[1]]$advances, 1L)
})

test_that("pa_index_in_half restarts each half", {
  evs <- list(
    start_evt(),
    pa("a1", "K", NA_integer_, outs = 1L, seq = 2L),
    pa("a2", "K", NA_integer_, outs = 1L, seq = 3L),
    pa("a3", "K", NA_integer_, outs = 1L, seq = 4L)
  )
  s <- fold_events(evs)
  expect_equal(s$half, "bottom")
  expect_equal(
    vapply(s$pa_log, function(r) r$pa_index_in_half, integer(1)),
    c(1L, 2L, 3L)
  )
})

test_that("the batter's own advance records the index of its own plate appearance", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", 1L, advances = list(make_advance("a1", 0L, 1L)), seq = 2L)
  ))
  own <- Filter(function(a) identical(a$from, 0L), s$pa_log[[1]]$advances)
  expect_equal(own[[1]]$origin_index, 1L)
  expect_equal(s$runner_origin[["a1"]], 1L)
})

test_that("a later advance is attributed to the runner's originating plate appearance", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", 1L, advances = list(make_advance("a1", 0L, 1L)), seq = 2L),
    pa(
      "a2",
      "1B",
      1L,
      advances = list(
        make_advance("a1", 1L, 2L),
        make_advance("a2", 0L, 1L)
      ),
      seq = 3L
    )
  ))
  a1_move <- Filter(
    function(a) identical(a$runner_id, "a1"),
    s$pa_log[[2]]$advances
  )
  expect_equal(a1_move[[1]]$origin_index, 1L)
  expect_equal(s$runner_origin[["a1"]], 1L)
  expect_equal(s$runner_origin[["a2"]], 2L)
})

test_that("a runner who scores or is out leaves runner_origin", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", 1L, advances = list(make_advance("a1", 0L, 1L)), seq = 2L),
    pa(
      "a2",
      "HR",
      4L,
      rbi = 2L,
      advances = list(
        make_advance("a1", 1L, 4L, scored = TRUE),
        make_advance("a2", 0L, 4L, scored = TRUE)
      ),
      seq = 3L
    )
  ))
  expect_null(s$runner_origin[["a1"]])
  expect_null(s$runner_origin[["a2"]])
})

test_that("advance_half clears runner_origin", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", 1L, advances = list(make_advance("a1", 0L, 1L)), seq = 2L),
    pa("a2", "K", NA_integer_, outs = 1L, seq = 3L),
    pa("a3", "K", NA_integer_, outs = 1L, seq = 4L),
    pa("a4", "K", NA_integer_, outs = 1L, seq = 5L)
  ))
  expect_equal(s$half, "bottom")
  expect_length(s$runner_origin, 0L)
})

test_that("a pinch runner inherits the original batter's origin", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", 1L, advances = list(make_advance("a1", 0L, 1L)), seq = 2L),
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
    )
  ))
  expect_equal(s$bases$first, "a9")
  expect_equal(s$runner_origin[["a9"]], 1L)
  expect_null(s$runner_origin[["a1"]])
})

test_that("a runner with no recorded origin gets NA rather than erroring", {
  s0 <- fold_events(list(start_evt()))
  s0$bases$second <- "ghost"
  s1 <- apply_plate_appearance(
    s0,
    list(
      payload = list(
        team = "away",
        batter_id = "a1",
        outcome = "1B",
        reached = 1L,
        rbi = 0L,
        outs_on_play = 0L,
        advances = list(
          make_advance("ghost", 2L, 3L),
          make_advance("a1", 0L, 1L)
        )
      )
    )
  )
  ghost <- Filter(
    function(a) identical(a$runner_id, "ghost"),
    s1$pa_log[[1]]$advances
  )
  expect_true(is.na(ghost[[1]]$origin_index))
})
