# Bookworm Slice 2.3 — Scorebook and Presentation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the scorebook readable — show where every runner is right now, show every run scored rather than only home runs, stop drawing plate appearances on top of each other, and put the game situation in one card.

**Architecture:** A new pure module `R/runner_paths.R` reconstructs each batter's full trip around the bases from the `origin_index` bookkeeping slice 2.2 added, and computes the scorebook's column layout. `R/scorebook_render.R` becomes rendering only: it takes path records and emits SVG. The two are separately testable, which matters because the current renderer's bug — reading a frozen `reached` value — is a *data* bug wearing a rendering costume.

**Tech Stack:** R 4.5.3, Shiny 1.13.0, bslib 0.10.0, htmltools 0.5.9, testthat 3.3.2.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-08-08-bookworm-slice-two-design.md`, section "Slice 2.3".
- **Depends on slices 2.0, 2.1, and 2.2 being merged.** Requires `origin_index` on advances, `pa_index_in_half` and `outs_before` on `pa_log` entries, `BRAND_COLORS$primary_light` and `$rule_line`, and `APP_CONFIG$outcome_meta`.
- Run every command from the **project root**. Rscript: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe"`.
- `Rscript run_tests.R` must exit 0 at the end of **every task**.
- `R/runner_paths.R` must not reference Shiny, `BRAND_COLORS`, or SVG. It is data only.
- Legacy events (slice 1.1, no `origin_index`) must still render: fall back to the `reached` field and produce a cell with no later advances rather than erroring.
- Cells are 64px. Legibility at that size is the acceptance bar for Task 2.

---

### Task 1: Runner paths and scorebook layout

**Files:**
- Create: `R/runner_paths.R`
- Test: `tests/test_runner_paths.R` (create)

**Interfaces:**
- Produces:
  - `runner_paths(state)` → list of records, one per `pa_log` entry, in order:
    `list(pa_index=, team=, inning=, half=, pa_index_in_half=, batter_id=, outcome=, fielding=, rbi=, bases_reached=<0..4>, scored=<lgl>, out_at=<0..3>, out_number=<int|NA>)`.
    `out_at = 0L` means the trip ended in an out without reaching a base; `NA` means no out.
  - `scorebook_layout(paths, team)` → `list(innings = <int>, sub_counts = <named int>, cells = <list>)` where each cell gains `sub_index` (which time through the order in that inning) and `col` (0-based column offset).

- [ ] **Step 1: Write the failing test**

Create `tests/test_runner_paths.R`:

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","rule_presets.R","game_events.R",
            "game_reducer.R","runner_paths.R"))
  source(file.path("R", f))

mk <- function(prefix, n = 4L) lapply(seq_len(n), function(i)
  make_player(paste0(prefix, i), paste(prefix, i), c("M","F")[(i %% 2) + 1L], i, i, i))
start_evt <- function() new_event("game_start", list(
  ruleset = default_ruleset_config(), first_bat = "away",
  home = list(team_id="H", name="Home", lineup = mk("h")),
  away = list(team_id="A", name="Away", lineup = mk("a"))), seq = 1L)
pa <- function(batter, outcome, advances, outs = 0L, rbi = 0L, reached = NA_integer_, seq = 2L)
  new_event("plate_appearance", list(team = "away", batter_id = batter, outcome = outcome,
    reached = reached, rbi = rbi, outs_on_play = outs, advances = advances), seq = seq)
A <- function(id, from, to, ...) make_advance(id, from, to, ...)

test_that("a batter who singles has reached one base and has not scored", {
  s <- fold_events(list(start_evt(), pa("a1", "1B", list(A("a1", 0L, 1L)), reached = 1L)))
  p <- runner_paths(s)
  expect_length(p, 1L)
  expect_equal(p[[1]]$bases_reached, 1L)
  expect_false(p[[1]]$scored)
  expect_true(is.na(p[[1]]$out_number))
})

test_that("a runner's later advances are credited to their own cell", {
  # a1 singles, goes to 2nd on a2's walk, to 3rd on a3's ground out, scores on a4's fly.
  s <- fold_events(list(start_evt(),
    pa("a1", "1B", list(A("a1", 0L, 1L)), reached = 1L, seq = 2L),
    pa("a2", "BB", list(A("a1", 1L, 2L), A("a2", 0L, 1L)), reached = 1L, seq = 3L),
    pa("a3", "GO", list(A("a1", 2L, 3L), A("a2", 1L, 2L),
                        A("a3", 0L, 0L, out = TRUE)), outs = 1L, seq = 4L),
    pa("a4", "SF", list(A("a1", 3L, 4L, scored = TRUE),
                        A("a2", 2L, 2L),
                        A("a4", 0L, 0L, out = TRUE)), outs = 1L, rbi = 1L, seq = 5L)))
  p <- runner_paths(s)
  expect_equal(p[[1]]$bases_reached, 4L)   # a1's own cell shows the full trip
  expect_true(p[[1]]$scored)
  expect_equal(p[[2]]$bases_reached, 2L)   # a2 is standing on second
  expect_false(p[[2]]$scored)
  expect_equal(p[[3]]$bases_reached, 0L)   # a3 made an out at the plate
  expect_equal(p[[3]]$out_number, 1L)
  expect_equal(p[[4]]$out_number, 2L)
})

test_that("out numbers count up within a half and restart in the next", {
  s <- fold_events(list(start_evt(),
    pa("a1", "K", list(A("a1", 0L, 0L, out = TRUE)), outs = 1L, seq = 2L),
    pa("a2", "K", list(A("a2", 0L, 0L, out = TRUE)), outs = 1L, seq = 3L),
    pa("a3", "K", list(A("a3", 0L, 0L, out = TRUE)), outs = 1L, seq = 4L)))
  p <- runner_paths(s)
  expect_equal(vapply(p, function(r) r$out_number, integer(1)), c(1L, 2L, 3L))
})

test_that("two outs on one play are numbered in advance order", {
  s <- fold_events(list(start_evt(),
    pa("a1", "1B", list(A("a1", 0L, 1L)), reached = 1L, seq = 2L),
    pa("a2", "GO", list(A("a1", 1L, 1L, out = TRUE),
                        A("a2", 0L, 0L, out = TRUE)), outs = 2L, seq = 3L)))
  p <- runner_paths(s)
  expect_equal(p[[1]]$out_number, 1L)   # lead runner listed first
  expect_equal(p[[2]]$out_number, 2L)   # batter last
})

test_that("a runner put out on the basepaths keeps the bases they reached", {
  s <- fold_events(list(start_evt(),
    pa("a1", "2B", list(A("a1", 0L, 2L)), reached = 2L, seq = 2L),
    pa("a2", "GO", list(A("a1", 2L, 2L, out = TRUE),
                        A("a2", 0L, 1L)), outs = 1L, reached = 1L, seq = 3L)))
  p <- runner_paths(s)
  expect_equal(p[[1]]$bases_reached, 2L)
  expect_equal(p[[1]]$out_at, 2L)
  expect_equal(p[[1]]$out_number, 1L)
})

test_that("a pinch runner's run is credited to the original batter's cell", {
  s <- fold_events(list(start_evt(),
    pa("a1", "1B", list(A("a1", 0L, 1L)), reached = 1L, seq = 2L),
    new_event("substitution", list(team = "away", kind = "courtesy_runner",
      out_player_id = "a1",
      in_player = make_player("a9", "Sub", "M", 9L, NA_integer_, NA_character_)), seq = 3L),
    pa("a2", "HR", list(A("a9", 1L, 4L, scored = TRUE),
                        A("a2", 0L, 4L, scored = TRUE)), reached = 4L, rbi = 2L, seq = 4L)))
  p <- runner_paths(s)
  expect_true(p[[1]]$scored)             # a1's cell, run scored by the pinch runner
  expect_equal(p[[1]]$bases_reached, 4L)
})

test_that("a legacy event with no advances falls back to reached", {
  s <- fold_events(list(start_evt(),
    pa("a1", "2B", list(), reached = 2L, seq = 2L)))
  p <- runner_paths(s)
  expect_equal(p[[1]]$bases_reached, 2L)
  expect_false(p[[1]]$scored)
})

test_that("a legacy home run is marked as scored", {
  s <- fold_events(list(start_evt(), pa("a1", "HR", list(), reached = 4L, rbi = 1L)))
  p <- runner_paths(s)
  expect_true(p[[1]]$scored)
  expect_equal(p[[1]]$bases_reached, 4L)
})

test_that("an advance with no origin_index is ignored rather than erroring", {
  s <- fold_events(list(start_evt()))
  s$pa_log <- list(list(team = "away", inning = 1L, half = "top", batter_id = "a1",
    outcome = "1B", fielding = NA_character_, rbi = 0L, reached = 1L, outs_before = 0L,
    pa_index_in_half = 1L,
    advances = list(list(runner_id = "ghost", from = 2L, to = 3L, scored = FALSE,
                         out = FALSE, origin_index = NA_integer_))))
  expect_silent(p <- runner_paths(s))
  expect_length(p, 1L)
})

test_that("scorebook_layout splits only the innings where someone batted twice", {
  s <- fold_events(list(start_evt(),
    # inning 1: a1, a2, a3 out, a4, then a1 again -> a1 has two PAs
    pa("a1", "1B", list(A("a1", 0L, 1L)), reached = 1L, seq = 2L),
    pa("a2", "1B", list(A("a1", 1L, 2L), A("a2", 0L, 1L)), reached = 1L, seq = 3L),
    pa("a3", "1B", list(A("a1", 2L, 3L), A("a2", 1L, 2L), A("a3", 0L, 1L)),
       reached = 1L, seq = 4L),
    pa("a4", "1B", list(A("a1", 3L, 4L, scored = TRUE), A("a2", 2L, 3L),
                        A("a3", 1L, 2L), A("a4", 0L, 1L)), reached = 1L, rbi = 1L, seq = 5L),
    pa("a1", "K", list(A("a1", 0L, 0L, out = TRUE)), outs = 1L, seq = 6L)))
  lay <- scorebook_layout(runner_paths(s), "away")
  expect_equal(unname(lay$sub_counts[["1"]]), 2L)
  a1_cells <- Filter(function(c) identical(c$batter_id, "a1"), lay$cells)
  expect_equal(vapply(a1_cells, function(c) c$sub_index, integer(1)), c(1L, 2L))
})

test_that("an inning with no repeat batter has one sub-column", {
  s <- fold_events(list(start_evt(),
    pa("a1", "K", list(A("a1", 0L, 0L, out = TRUE)), outs = 1L, seq = 2L)))
  lay <- scorebook_layout(runner_paths(s), "away")
  expect_equal(unname(lay$sub_counts[["1"]]), 1L)
  expect_equal(lay$cells[[1]]$col, 0L)
})

test_that("layout for a team with no plate appearances is empty but valid", {
  lay <- scorebook_layout(list(), "home")
  expect_length(lay$cells, 0L)
  expect_equal(lay$innings, 0L)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_runner_paths.R")'`
Expected: FAIL — cannot open `R/runner_paths.R`.

- [ ] **Step 3: Create `R/runner_paths.R`**

```r
# Reconstructs each batter's full trip around the bases from the pa_log.
#
# The bug this fixes: the old renderer read pa_log[[i]]$reached, which is frozen at the
# moment of the plate appearance. A runner who singles and later comes around to score
# never updated their own cell, so only home runs ever showed a run. Every advance now
# carries origin_index — the pa_log entry that put that runner on base — so a later
# advance can be credited back to the cell it belongs to.
#
# Pure data. No Shiny, no colours, no SVG.

.own_advance <- function(rec)
  Filter(function(a) identical(as.integer(a$from %||% -1L), 0L), rec$advances %||% list())

.seed_from_own <- function(rec) {
  own <- .own_advance(rec)
  if (length(own)) {
    a <- own[[1]]
    if (isTRUE(a$scored)) return(4L)
    if (isTRUE(a$out))    return(0L)
    return(as.integer(a$to))
  }
  # Legacy path: slice 1.1 events have no batter advance, only `reached`.
  r <- rec$reached %||% NA_integer_
  if (is.null(r) || length(r) != 1 || is.na(r)) 0L else as.integer(r)
}

runner_paths <- function(state) {
  pal <- state$pa_log %||% list()
  n <- length(pal)
  if (n == 0L) return(list())

  recs <- vector("list", n)
  for (i in seq_len(n)) {
    rec <- pal[[i]]
    seed <- .seed_from_own(rec)
    recs[[i]] <- list(
      pa_index = i, team = rec$team, inning = as.integer(rec$inning), half = rec$half,
      pa_index_in_half = as.integer(rec$pa_index_in_half %||% NA_integer_),
      batter_id = rec$batter_id, outcome = rec$outcome,
      fielding = rec$fielding %||% NA_character_,
      rbi = as.integer(rec$rbi %||% 0L),
      bases_reached = seed, scored = seed >= 4L,
      out_at = NA_integer_, out_number = NA_integer_)
  }

  for (i in seq_len(n)) {
    rec <- pal[[i]]
    # outs_before carries the half's running total, so out numbering is local to the play.
    k <- as.integer(rec$outs_before %||% 0L)
    advances <- rec$advances %||% list()

    for (a in advances) {
      oi <- a$origin_index %||% NA_integer_
      if (is.null(oi) || length(oi) != 1 || is.na(oi) || oi < 1L || oi > n) next
      oi <- as.integer(oi)
      if (isTRUE(a$scored)) {
        recs[[oi]]$bases_reached <- 4L
        recs[[oi]]$scored <- TRUE
      } else if (isTRUE(a$out)) {
        k <- k + 1L
        recs[[oi]]$out_at <- as.integer(a$from)
        recs[[oi]]$out_number <- k
        recs[[oi]]$bases_reached <- max(recs[[oi]]$bases_reached, as.integer(a$from))
      } else {
        recs[[oi]]$bases_reached <- max(recs[[oi]]$bases_reached, as.integer(a$to))
      }
    }

    # Legacy events record outs_on_play without an out advance for the batter.
    if (!length(Filter(function(a) isTRUE(a$out), advances)) &&
        as.integer(rec$outs_on_play %||% 0L) > 0L && is.na(recs[[i]]$out_number)) {
      recs[[i]]$out_number <- k + 1L
      recs[[i]]$out_at <- 0L
    }
  }
  recs
}

# Column layout for one team's scorebook. An inning gets one sub-column per time
# through the order, so batting around no longer draws cells on top of each other.
scorebook_layout <- function(paths, team) {
  cells <- Filter(function(p) identical(p$team, team), paths)
  if (!length(cells)) return(list(innings = 0L, sub_counts = integer(0), cells = list()))

  seen <- list()   # "<inning>|<batter_id>" -> count so far
  for (i in seq_along(cells)) {
    key <- paste0(cells[[i]]$inning, "|", cells[[i]]$batter_id)
    seen[[key]] <- (seen[[key]] %||% 0L) + 1L
    cells[[i]]$sub_index <- seen[[key]]
  }

  innings <- max(vapply(cells, function(c) c$inning, integer(1)))
  sub_counts <- stats::setNames(rep(1L, innings), as.character(seq_len(innings)))
  for (c in cells) {
    k <- as.character(c$inning)
    sub_counts[[k]] <- max(sub_counts[[k]], c$sub_index)
  }
  # Column offset: every preceding inning's sub-columns, plus this cell's own index.
  offsets <- c(0L, cumsum(unname(sub_counts)))
  for (i in seq_along(cells))
    cells[[i]]$col <- offsets[[cells[[i]]$inning]] + cells[[i]]$sub_index - 1L

  list(innings = as.integer(innings), sub_counts = sub_counts, cells = cells)
}
```

- [ ] **Step 4: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_runner_paths.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0.

- [ ] **Step 5: Commit**

```bash
git add R/runner_paths.R tests/test_runner_paths.R
git commit -m "feat: reconstruct full runner trips and scorebook column layout"
```

---

### Task 2: Scorebook cell

**Files:**
- Modify: `R/scorebook_render.R` (`scorebook_cell_svg`)
- Test: `tests/test_scorebook_render.R` (rewrite the cell tests)

**Interfaces:**
- Produces: `scorebook_cell_svg(path, x, y, cell)` where `path` is a record from
  `runner_paths()`. Returns an SVG fragment string. **Signature change** — the old form
  took a raw `pa_log` record.

- [ ] **Step 1: Write the failing test**

Replace the first `test_that` block in `tests/test_scorebook_render.R` (the one building a
`rec` list) with:

```r
# Add runner_paths.R to the source list at the top of this file.
mk_path <- function(bases_reached, scored = FALSE, out_at = NA_integer_,
                    out_number = NA_integer_, rbi = 0L, outcome = "1B",
                    fielding = NA_character_)
  list(pa_index = 1L, team = "away", inning = 1L, half = "top", pa_index_in_half = 1L,
       batter_id = "a1", outcome = outcome, fielding = fielding, rbi = rbi,
       bases_reached = bases_reached, scored = scored,
       out_at = out_at, out_number = out_number)

n_heavy <- function(frag) lengths(regmatches(frag, gregexpr('stroke-width="3"', frag)))

test_that("the cell draws one heavy leg per base reached", {
  expect_equal(n_heavy(scorebook_cell_svg(mk_path(0L), 0, 0, 64)), 0L)
  expect_equal(n_heavy(scorebook_cell_svg(mk_path(1L), 0, 0, 64)), 1L)
  expect_equal(n_heavy(scorebook_cell_svg(mk_path(2L), 0, 0, 64)), 2L)
  expect_equal(n_heavy(scorebook_cell_svg(mk_path(3L), 0, 0, 64)), 3L)
})

test_that("a scored trip draws all four legs and fills the diamond", {
  frag <- scorebook_cell_svg(mk_path(4L, scored = TRUE), 0, 0, 64)
  expect_equal(n_heavy(frag), 4L)
  expect_true(grepl("polygon", frag))
  expect_true(grepl(BRAND_COLORS$primary_light, frag, fixed = TRUE))
})

test_that("an unscored cell is not filled", {
  frag <- scorebook_cell_svg(mk_path(3L), 0, 0, 64)
  expect_false(grepl(BRAND_COLORS$primary_light, frag, fixed = TRUE))
})

test_that("the outcome and fielding notation are rendered", {
  frag <- scorebook_cell_svg(mk_path(0L, outcome = "GO", fielding = "6-3"), 0, 0, 64)
  expect_true(grepl("GO", frag, fixed = TRUE))
  expect_true(grepl("6-3", frag, fixed = TRUE))
})

test_that("the out number renders as a circled numeral", {
  frag <- scorebook_cell_svg(mk_path(0L, out_number = 2L, outcome = "K"), 0, 0, 64)
  expect_true(grepl("circle", frag))
  expect_true(grepl(">2<", frag, fixed = TRUE))
})

test_that("RBI dots are drawn, one per RBI, capped at four", {
  n_dots <- function(f) lengths(regmatches(f, gregexpr("bw-rbi", f)))
  expect_equal(n_dots(scorebook_cell_svg(mk_path(4L, scored = TRUE, rbi = 2L), 0, 0, 64)), 2L)
  expect_equal(n_dots(scorebook_cell_svg(mk_path(4L, scored = TRUE, rbi = 9L), 0, 0, 64)), 4L)
  expect_equal(n_dots(scorebook_cell_svg(mk_path(1L, rbi = 0L), 0, 0, 64)), 0L)
})

test_that("an out on the basepaths marks the base", {
  frag <- scorebook_cell_svg(mk_path(2L, out_at = 2L, out_number = 1L), 0, 0, 64)
  expect_true(grepl("bw-out-mark", frag, fixed = TRUE))
})

test_that("a batter out at the plate has no base mark", {
  frag <- scorebook_cell_svg(mk_path(0L, out_at = 0L, out_number = 1L, outcome = "K"),
                             0, 0, 64)
  expect_false(grepl("bw-out-mark", frag, fixed = TRUE))
})

test_that("the cell is positioned at the requested offset", {
  frag <- scorebook_cell_svg(mk_path(1L), x = 120, y = 40, cell = 64)
  # Every coordinate must fall inside the cell's box.
  nums <- as.numeric(regmatches(frag, gregexpr("[0-9]+\\.?[0-9]*", frag))[[1]])
  expect_true(all(nums[nums > 100 & nums < 400] >= 40))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_scorebook_render.R")'`
Expected: FAIL — the current cell renderer emits a single `polygon` with no per-leg strokes.

- [ ] **Step 3: Rewrite `scorebook_cell_svg()`**

Replace the top of `R/scorebook_render.R`:

```r
# One scorebook cell: a diamond whose edges darken as the runner advances, filled when
# they score. Takes a record from runner_paths(), not a raw pa_log entry — the whole
# point is that a cell reflects the runner's *cumulative* trip, updated by later plays.

# Vertices, clockwise from home. Index 1..4 == first, second, third, home.
.diamond_vertices <- function(cx, cy, r)
  list(home = c(cx, cy + r), first = c(cx + r, cy),
       second = c(cx, cy - r), third = c(cx - r, cy))

.leg_endpoints <- function(v) list(
  list(v$home,   v$first),    # leg 1: home to first
  list(v$first,  v$second),   # leg 2
  list(v$second, v$third),    # leg 3
  list(v$third,  v$home))     # leg 4: third to home

.base_point <- function(v, base) switch(as.character(base),
  "1" = v$first, "2" = v$second, "3" = v$third, "4" = v$home, NULL)

scorebook_cell_svg <- function(path, x, y, cell) {
  cx <- x + cell / 2
  cy <- y + cell * 0.42          # leaves room for the outcome text beneath
  r  <- cell * 0.28
  v  <- .diamond_vertices(cx, cy, r)
  reached <- as.integer(path$bases_reached %||% 0L)
  parts <- character()

  # Filled interior means the runner scored. Drawn first so the legs sit on top.
  if (isTRUE(path$scored)) {
    pts <- paste(vapply(list(v$home, v$first, v$second, v$third),
      function(p) sprintf("%.1f,%.1f", p[1], p[2]), character(1)), collapse = " ")
    parts <- c(parts, sprintf('<polygon points="%s" fill="%s" stroke="none"/>',
                              pts, BRAND_COLORS$primary_light))
  }

  # One line per leg: heavy for legs the runner completed, light for the rest.
  legs <- .leg_endpoints(v)
  for (j in seq_along(legs)) {
    done <- reached >= j
    parts <- c(parts, sprintf(
      '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="%s" stroke-linecap="round" %s/>',
      legs[[j]][[1]][1], legs[[j]][[1]][2], legs[[j]][[2]][1], legs[[j]][[2]][2],
      if (done) BRAND_COLORS$dark else BRAND_COLORS$secondary,
      if (done) "3" else "1",
      if (done) "" else 'opacity="0.35"'))
  }

  # Out number, circled, top right.
  if (!is.na(path$out_number %||% NA)) {
    ox <- x + cell - 10; oy <- y + 11
    parts <- c(parts,
      sprintf('<circle cx="%.1f" cy="%.1f" r="7" fill="none" stroke="%s" stroke-width="1"/>',
              ox, oy, BRAND_COLORS$dark),
      sprintf('<text x="%.1f" y="%.1f" font-size="9" text-anchor="middle" fill="%s">%d</text>',
              ox, oy + 3.2, BRAND_COLORS$dark, as.integer(path$out_number)))
  }

  # RBI dots, top left, capped at four glyphs.
  rbi <- min(4L, as.integer(path$rbi %||% 0L))
  for (i in seq_len(rbi))
    parts <- c(parts, sprintf(
      '<circle class="bw-rbi" cx="%.1f" cy="%.1f" r="2.2" fill="%s"/>',
      x + 6 + (i - 1) * 5.5, y + 8, BRAND_COLORS$danger))

  # A cross at the base where the trip ended, when it ended on the basepaths.
  out_at <- path$out_at %||% NA_integer_
  if (!is.na(out_at) && out_at >= 1L) {
    p <- .base_point(v, out_at)
    parts <- c(parts, sprintf(
      '<text class="bw-out-mark" x="%.1f" y="%.1f" font-size="10" text-anchor="middle" fill="%s">&#215;</text>',
      p[1], p[2] + 3.5, BRAND_COLORS$danger))
  }

  # Outcome plus fielding notation, centred beneath the diamond.
  label <- paste0(path$outcome,
    if (!is.na(path$fielding %||% NA)) paste0(" ", path$fielding) else "")
  parts <- c(parts, sprintf(
    '<text x="%.1f" y="%.1f" font-size="%.1f" text-anchor="middle" fill="%s">%s</text>',
    cx, y + cell - 5, cell * 0.17, BRAND_COLORS$dark, htmltools::htmlEscape(label)))

  paste(parts, collapse = "")
}
```

- [ ] **Step 4: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_scorebook_render.R")'`
Expected: the cell tests PASS; the grid test still fails until Task 3.

- [ ] **Step 5: Commit**

```bash
git add R/scorebook_render.R tests/test_scorebook_render.R
git commit -m "feat: scorebook cell traces the runner's path base by base"
```

---

### Task 3: Scorebook grid

**Files:**
- Modify: `R/scorebook_render.R` (`render_scorebook_svg`)
- Modify: `R/tracking_module.R` (team toggle)
- Modify: `www/css/app.css`
- Test: `tests/test_scorebook_render.R` (extend)

**Interfaces:**
- Produces: `render_scorebook_svg(state, team, current_batter_id = NULL)` → an
  `htmltools::HTML` block. **Signature change** — gains `current_batter_id`.

- [ ] **Step 1: Write the failing test**

Append to `tests/test_scorebook_render.R`:

```r
mk_lineup <- function(prefix, n = 3L) lapply(seq_len(n), function(i)
  make_player(paste0(prefix, i), paste("A", i), "M", i, i, i))

state_with <- function(events) {
  start <- new_event("game_start", list(ruleset = default_ruleset_config(),
    first_bat = "away",
    home = list(team_id = "H", name = "Home", lineup = mk_lineup("h")),
    away = list(team_id = "A", name = "Away", lineup = mk_lineup("a"))), seq = 1L)
  fold_events(c(list(start), events))
}
PA <- function(batter, outcome, advances, outs = 0L, reached = NA_integer_, seq = 2L)
  new_event("plate_appearance", list(team = "away", batter_id = batter, outcome = outcome,
    reached = reached, rbi = 0L, outs_on_play = outs, advances = advances), seq = seq)
Adv <- function(id, from, to, ...) make_advance(id, from, to, ...)

test_that("the grid renders an svg with a row label per batter", {
  html <- as.character(render_scorebook_svg(state_with(list()), "away"))
  expect_true(grepl("<svg", html))
  for (i in 1:3) expect_true(grepl(paste("A", i), html, fixed = TRUE))
})

test_that("a jersey-less player has no stray leading hash or space", {
  st <- state_with(list())
  st$lineups$away[[1]]$jersey_number <- NA_integer_
  html <- as.character(render_scorebook_svg(st, "away"))
  expect_false(grepl(">#NA", html, fixed = TRUE))
  expect_false(grepl('">A 1', html, fixed = TRUE) && grepl('"> A 1', html, fixed = TRUE))
})

test_that("the current batter's row label is bold", {
  st <- state_with(list())
  html <- as.character(render_scorebook_svg(st, "away", current_batter_id = "a2"))
  expect_true(grepl("font-weight=\"700\"", html, fixed = TRUE) ||
              grepl("bw-current", html, fixed = TRUE))
})

test_that("no row is bold when no current batter is passed", {
  html <- as.character(render_scorebook_svg(state_with(list()), "away"))
  expect_false(grepl("bw-current", html, fixed = TRUE))
})

test_that("batting around widens the inning into sub-columns", {
  st <- state_with(list(
    PA("a1", "1B", list(Adv("a1", 0L, 1L)), reached = 1L, seq = 2L),
    PA("a2", "1B", list(Adv("a1", 1L, 2L), Adv("a2", 0L, 1L)), reached = 1L, seq = 3L),
    PA("a3", "1B", list(Adv("a1", 2L, 3L), Adv("a2", 1L, 2L), Adv("a3", 0L, 1L)),
       reached = 1L, seq = 4L),
    PA("a1", "K", list(Adv("a1", 0L, 0L, out = TRUE)), outs = 1L, seq = 5L)))
  html <- as.character(render_scorebook_svg(st, "away"))
  # Two sub-columns in inning 1 means a wider svg than a single-column inning.
  narrow <- as.character(render_scorebook_svg(state_with(list(
    PA("a1", "K", list(Adv("a1", 0L, 0L, out = TRUE)), outs = 1L, seq = 2L))), "away"))
  w <- function(h) as.numeric(sub('.*<svg width="([0-9]+)".*', "\\1", h))
  expect_gt(w(html), w(narrow))
})

test_that("a team with no plate appearances still renders its lineup", {
  html <- as.character(render_scorebook_svg(state_with(list()), "home"))
  expect_true(grepl("<svg", html))
})

test_that("the grid shows every inning that has been played", {
  st <- state_with(list(
    PA("a1", "K", list(Adv("a1", 0L, 0L, out = TRUE)), outs = 1L, seq = 2L),
    PA("a2", "K", list(Adv("a2", 0L, 0L, out = TRUE)), outs = 1L, seq = 3L),
    PA("a3", "K", list(Adv("a3", 0L, 0L, out = TRUE)), outs = 1L, seq = 4L)))
  expect_equal(st$half, "bottom")
  html <- as.character(render_scorebook_svg(st, "away"))
  expect_true(grepl(">1<", html, fixed = TRUE))   # inning header
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_scorebook_render.R")'`
Expected: FAIL — `render_scorebook_svg` has no `current_batter_id` argument, and inning
columns do not split.

- [ ] **Step 3: Rewrite `render_scorebook_svg()`**

```r
render_scorebook_svg <- function(state, team, current_batter_id = NULL) {
  lineup  <- state$lineups[[team]]
  batters <- Filter(function(p) !is.na(p$order_slot), lineup)
  batters <- batters[order(vapply(batters, function(p) p$order_slot, integer(1)))]

  lay <- scorebook_layout(runner_paths(state), team)
  # Show every inning reached, even one with no plate appearances yet.
  innings <- max(1L, lay$innings, as.integer(state$inning %||% 1L))
  sub_counts <- stats::setNames(rep(1L, innings), as.character(seq_len(innings)))
  for (k in names(lay$sub_counts)) sub_counts[[k]] <- lay$sub_counts[[k]]
  total_cols <- sum(sub_counts)

  cell <- 64; label_w <- 130; header_h <- 26
  w <- label_w + total_cols * cell
  h <- header_h + max(1L, length(batters)) * cell
  offsets <- c(0L, cumsum(unname(sub_counts)))

  parts <- character()

  # Faint rules so the grid reads as a scorebook page.
  for (i in seq_len(innings)) {
    gx <- label_w + offsets[[i]] * cell
    parts <- c(parts, sprintf(
      '<line x1="%d" y1="0" x2="%d" y2="%d" stroke="%s" stroke-width="1"/>',
      gx, gx, h, BRAND_COLORS$rule_line))
    # Inning header, centred over its sub-columns.
    parts <- c(parts, sprintf(
      '<text x="%.1f" y="17" font-size="11" text-anchor="middle" fill="%s">%d</text>',
      gx + sub_counts[[i]] * cell / 2, BRAND_COLORS$secondary, i))
  }
  for (bi in seq_len(max(1L, length(batters)))) {
    ry <- header_h + bi * cell
    parts <- c(parts, sprintf(
      '<line x1="0" y1="%d" x2="%d" y2="%d" stroke="%s" stroke-width="1"/>',
      ry, w, ry, BRAND_COLORS$rule_line))
  }
  parts <- c(parts, sprintf(
    '<line x1="0" y1="%d" x2="%d" y2="%d" stroke="%s" stroke-width="1"/>',
    header_h, w, header_h, BRAND_COLORS$rule_line))

  for (bi in seq_along(batters)) {
    p <- batters[[bi]]
    y <- header_h + (bi - 1) * cell
    is_current <- !is.null(current_batter_id) &&
      identical(p$player_id, current_batter_id)
    label <- paste0(
      if (!is.na(p$jersey_number)) paste0("#", p$jersey_number, " ") else "",
      p$name)
    if (is_current)
      parts <- c(parts, sprintf(
        '<rect x="0" y="%.1f" width="%d" height="%d" fill="%s" opacity="0.5"/>',
        y, w, cell, BRAND_COLORS$primary_light))
    parts <- c(parts, sprintf(
      '<text %s x="6" y="%.1f" font-size="12" fill="%s" font-weight="%s">%s</text>',
      if (is_current) 'class="bw-current"' else "",
      y + cell / 2, BRAND_COLORS$dark, if (is_current) "700" else "400",
      htmltools::htmlEscape(label)))

    for (c in lay$cells) {
      if (!identical(c$batter_id, p$player_id)) next
      parts <- c(parts, scorebook_cell_svg(c, label_w + c$col * cell, y, cell))
    }
  }

  htmltools::HTML(sprintf(
    '<div class="bw-scorebook"><svg width="%d" height="%d" viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg">%s</svg></div>',
    w, h, w, h, paste(parts, collapse = "")))
}
```

- [ ] **Step 4: Render both teams behind a toggle**

In `R/tracking_module.R`, replace the Scorebook nav panel and its output:

```r
      nav_panel("Scorebook",
        div(class = "p-2",
          radioButtons(ns("sb_team"), NULL, c("Away" = "away", "Home" = "home"),
                       selected = "away", inline = TRUE),
          uiOutput(ns("scorebook")))),
```

```r
    output$scorebook <- renderUI({
      s <- state()
      team <- input$sb_team %||% "away"
      cb <- if (identical(team, s$batting_team)) s$current_batter$player_id else NULL
      render_scorebook_svg(s, team, current_batter_id = cb)
    })
```

The toggle labels should show the real team names; update them when the game starts:

```r
    observeEvent(state()$teams, {
      t <- state()$teams
      updateRadioButtons(session, "sb_team",
        choiceNames = c(t$away$name %||% "Away", t$home$name %||% "Home"),
        choiceValues = c("away", "home"),
        selected = isolate(input$sb_team) %||% "away", inline = TRUE)
    }, ignoreInit = TRUE, once = TRUE)
```

- [ ] **Step 5: Pin the name column when the grid scrolls**

Append to `www/css/app.css`:

```css
/* Scorebook: horizontal scroll on narrow screens; the svg carries its own grid. */
.bw-scorebook { overflow-x: auto; -webkit-overflow-scrolling: touch; }
.bw-scorebook svg { display: block; }
```

The name column is inside the SVG, so pinning it needs the label band drawn as a second,
sticky SVG. That is more machinery than it is worth at this size — the horizontal scroll
plus a 130px label column is sufficient. Note it in the README's known limitations rather
than half-building it.

- [ ] **Step 6: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_scorebook_render.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0.

- [ ] **Step 7: Manual check — this is the acceptance bar for the slice**

Run the app and score a half-inning: single, walk, ground out, sacrifice fly. Confirm:
- the first batter's cell shows one heavy leg after the single, two after the walk, three
  after the ground out, and a filled diamond after the sacrifice fly
- the runner's current base is obvious at a glance mid-inning
- the out numbers read 1 and 2
- batting around produces side-by-side cells, not overlapping ones
- the whole thing is legible at 64px cells on a phone-width window

- [ ] **Step 8: Commit**

```bash
git add R/scorebook_render.R R/tracking_module.R www/css/app.css tests/test_scorebook_render.R
git commit -m "feat: scorebook grid with per-PA sub-columns, rules, and a bold current batter"
```

---

### Task 4: Situation summary card

**Files:**
- Modify: `R/boxscore.R` (extract a shared accumulator, add `batting_line_for`)
- Create: `R/situation_card.R`
- Modify: `R/tracking_module.R` (`output$situation`)
- Modify: `www/css/app.css`
- Test: `tests/test_boxscore.R` (extend), `tests/test_situation_card.R` (create)

**Interfaces:**
- Produces:
  - `batting_line_for(state, team, player_id)` → `list(AB=, R=, H=, RBI=, BB=, K=)` or `NULL`.
  - `batter_line_text(line)` → `<chr>`, e.g. `"1-for-2, RBI"` or `"0-for-1"`.
  - `situation_card_ui(state)` → a `bslib::card`.

- [ ] **Step 1: Write the failing test**

Create `tests/test_situation_card.R`:

```r
library(testthat)
suppressMessages({ library(shiny); library(bslib); library(htmltools) })
for (f in c("app_config.R","brand_colors.R","rules_engine.R","rule_presets.R",
            "game_events.R","game_reducer.R","boxscore.R","situation_card.R"))
  source(file.path("R", f))

mk <- function(prefix, n = 3L) lapply(seq_len(n), function(i)
  make_player(paste0(prefix, i), paste0(toupper(prefix), i), "M", i, i, i))
st <- function() {
  s <- initial_game_state()
  s$lineups$away <- mk("a"); s$lineups$home <- mk("h")
  s$teams <- list(away = list(team_id = "A", name = "Otters"),
                  home = list(team_id = "H", name = "Badgers"))
  s$current_batter <- s$lineups$away[[1]]
  s$score <- list(home = 2L, away = 5L)
  s$inning <- 4L; s$half <- "top"; s$outs <- 2L
  s$count <- list(balls = 1L, strikes = 2L)
  s
}

test_that("the card shows both team names and the score", {
  html <- as.character(situation_card_ui(st()))
  expect_true(grepl("Otters", html, fixed = TRUE))
  expect_true(grepl("Badgers", html, fixed = TRUE))
  expect_true(grepl(">5<", html, fixed = TRUE))
  expect_true(grepl(">2<", html, fixed = TRUE))
})

test_that("the half-inning is an arrow, not the words top and bottom", {
  html <- as.character(situation_card_ui(st()))
  expect_true(grepl("&#9650;", html, fixed = TRUE) || grepl("▲", html, fixed = TRUE))
  expect_false(grepl(">top<", html, fixed = TRUE))
})

test_that("the count and outs are shown", {
  html <- as.character(situation_card_ui(st()))
  expect_true(grepl("1-2", html, fixed = TRUE))
  expect_true(grepl("2 out", html, fixed = TRUE))
})

test_that("the current batter and their line are shown", {
  s <- st()
  s$pa_log <- list(
    list(team = "away", batter_id = "a1", outcome = "1B", rbi = 1L, reached = 1L),
    list(team = "away", batter_id = "a1", outcome = "K",  rbi = 0L, reached = NA_integer_))
  html <- as.character(situation_card_ui(s))
  expect_true(grepl("A1", html, fixed = TRUE))
  expect_true(grepl("1-for-2", html, fixed = TRUE))
})

test_that("a final game says FINAL", {
  s <- st(); s$status <- "final"
  expect_true(grepl("FINAL", as.character(situation_card_ui(s)), fixed = TRUE))
})

test_that("the card renders with no current batter", {
  s <- st(); s$current_batter <- NULL
  expect_silent(html <- as.character(situation_card_ui(s)))
  expect_true(grepl("Otters", html, fixed = TRUE))
})

test_that("batter_line_text summarises hits, RBI, and walks", {
  expect_equal(batter_line_text(list(AB = 2L, H = 1L, RBI = 1L, BB = 0L, K = 0L, R = 0L)),
               "1-for-2, RBI")
  expect_equal(batter_line_text(list(AB = 1L, H = 0L, RBI = 0L, BB = 0L, K = 1L, R = 0L)),
               "0-for-1, K")
  expect_equal(batter_line_text(list(AB = 0L, H = 0L, RBI = 0L, BB = 1L, K = 0L, R = 0L)),
               "0-for-0, BB")
  expect_equal(batter_line_text(list(AB = 0L, H = 0L, RBI = 0L, BB = 0L, K = 0L, R = 0L)),
               "first at-bat")
})
```

Append to `tests/test_boxscore.R`:

```r
test_that("batting_line_for returns one player's accumulated line", {
  st <- initial_game_state()
  st$lineups$away <- list(make_player("a1", "Ann", "F", 7L, 1L, "SS"))
  st$pa_log <- list(
    list(team = "away", batter_id = "a1", outcome = "2B", rbi = 2L, reached = 2L),
    list(team = "away", batter_id = "a1", outcome = "BB", rbi = 0L, reached = 1L))
  l <- batting_line_for(st, "away", "a1")
  expect_equal(l$AB, 1L)     # the walk is not an at-bat
  expect_equal(l$H, 1L)
  expect_equal(l$RBI, 2L)
  expect_equal(l$BB, 1L)
})

test_that("batting_line_for returns NULL for an unknown player", {
  st <- initial_game_state()
  expect_null(batting_line_for(st, "away", "nobody"))
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_situation_card.R")'`
Expected: FAIL — cannot open `R/situation_card.R`.

- [ ] **Step 3: Extract the accumulator in `R/boxscore.R`**

Pull the loop out of `batting_lines()` so the card and the box score share one definition
of what an at-bat is:

```r
# Per-player counting stats for one team, keyed by player_id.
.accumulate_batting <- function(state, team) {
  ids <- vapply(state$lineups[[team]], function(p) p$player_id, character(1))
  init <- function() setNames(as.list(rep(0L, length(ids))), ids)
  acc <- list(AB = init(), R = init(), H = init(), RBI = init(), BB = init(), K = init())

  for (rec in state$pa_log) {
    if (!identical(rec$team, team)) next
    id <- rec$batter_id
    if (is.null(acc$AB[[id]])) next
    o <- rec$outcome
    if (!o %in% .AB_EXCLUDE)      acc$AB[[id]]  <- acc$AB[[id]] + 1L
    if (o %in% .HIT)              acc$H[[id]]   <- acc$H[[id]] + 1L
    if (o %in% c("K", "KL"))      acc$K[[id]]   <- acc$K[[id]] + 1L
    if (o %in% c("BB", "IBB"))    acc$BB[[id]]  <- acc$BB[[id]] + 1L
    acc$RBI[[id]] <- acc$RBI[[id]] + as.integer(rec$rbi %||% 0L)
    r <- rec$reached %||% NA_integer_
    if (!is.na(r) && r == 4L)     acc$R[[id]]   <- acc$R[[id]] + 1L
  }
  list(ids = ids, acc = acc)
}

batting_line_for <- function(state, team, player_id) {
  a <- .accumulate_batting(state, team)
  if (!player_id %in% a$ids) return(NULL)
  stats::setNames(lapply(names(a$acc), function(k) a$acc[[k]][[player_id]]), names(a$acc))
}
```

Then rewrite `batting_lines()` to call `.accumulate_batting()` and build its data frame
from the result, keeping the `Order`/`Player` columns from slice 2.0 Task 4.

`.HIT` must gain `"ITPHR"`: an inside-the-park home run is a hit and a run. Change the
constant to `c("1B","2B","3B","HR","ITPHR")`. Add a test for it in `tests/test_boxscore.R`.

- [ ] **Step 4: Create `R/situation_card.R`**

```r
# The one-glance game situation: score, inning, outs, count, batter, batter's line.

batter_line_text <- function(line) {
  if (is.null(line)) return("")
  parts <- sprintf("%d-for-%d", line$H, line$AB)
  extras <- character()
  if (line$RBI > 0L) extras <- c(extras, if (line$RBI == 1L) "RBI" else sprintf("%d RBI", line$RBI))
  if (line$BB  > 0L) extras <- c(extras, if (line$BB  == 1L) "BB"  else sprintf("%d BB",  line$BB))
  if (line$K   > 0L) extras <- c(extras, if (line$K   == 1L) "K"   else sprintf("%d K",   line$K))
  if (line$AB == 0L && !length(extras)) return("first at-bat")
  paste(c(parts, extras), collapse = ", ")
}

.score_block <- function(name, runs, batting) tags$div(
  class = paste("bw-score-side", if (batting) "bw-batting" else ""),
  tags$div(class = "bw-team", name),
  tags$div(class = "bw-runs", runs))

situation_card_ui <- function(state) {
  final <- identical(state$status, "final")
  away_name <- state$teams$away$name %||% "Away"
  home_name <- state$teams$home$name %||% "Home"
  arrow <- if (identical(state$half, "top")) HTML("&#9650;") else HTML("&#9660;")

  b <- state$current_batter
  line <- if (is.null(b)) NULL else batting_line_for(state, state$batting_team, b$player_id)
  slot <- if (is.null(b) || is.na(b$order_slot)) NULL else sprintf("#%d in the order", b$order_slot)

  card(class = "bw-situation", full_screen = FALSE,
    card_body(class = "p-2",
      tags$div(class = "bw-sit-top d-flex align-items-center justify-content-between",
        .score_block(away_name, state$score$away,
                     !final && identical(state$batting_team, "away")),
        tags$div(class = "bw-sit-mid text-center",
          if (final) tags$div(class = "bw-final", "FINAL")
          else tagList(
            tags$div(class = "bw-inning", arrow, " ", state$inning),
            tags$div(class = "bw-count", sprintf("%d-%d", state$count$balls,
                                                 state$count$strikes)),
            tags$div(class = "bw-outs text-muted",
                     sprintf("%d out", state$outs)))),
        .score_block(home_name, state$score$home,
                     !final && identical(state$batting_team, "home"))),
      if (!is.null(b)) tags$div(class = "bw-sit-batter d-flex gap-2 align-items-baseline mt-1",
        tags$span(class = "bw-batter-name",
          paste0(if (!is.na(b$jersey_number)) paste0("#", b$jersey_number, " ") else "",
                 b$name)),
        if (!is.null(slot)) tags$span(class = "text-muted small", slot),
        tags$span(class = "text-muted small ms-auto", batter_line_text(line)))))
}
```

- [ ] **Step 5: Swap it into `tracking_module.R` and style it**

Replace `output$situation`'s body with `renderUI(situation_card_ui(state()))`.

Append to `www/css/app.css`:

```css
/* Situation card: score and count carry the most weight — they are what a scorer
   looks at between every pitch. */
.bw-situation { border-color: var(--bs-border-color); }
.bw-score-side { text-align: center; min-width: 5.5rem; }
.bw-score-side .bw-team { font-size: .75rem; text-transform: uppercase;
  letter-spacing: .04em; color: var(--bs-secondary); }
.bw-score-side .bw-runs { font-size: 2rem; line-height: 1; font-weight: 700;
  font-variant-numeric: tabular-nums; }
.bw-score-side.bw-batting .bw-team { color: var(--bs-primary); font-weight: 700; }
.bw-sit-mid .bw-inning { font-size: .95rem; font-weight: 600; }
.bw-sit-mid .bw-count { font-size: 1.6rem; font-weight: 700; line-height: 1.1;
  font-variant-numeric: tabular-nums; }
.bw-sit-mid .bw-outs { font-size: .8rem; }
.bw-sit-mid .bw-final { font-size: 1.3rem; font-weight: 700;
  letter-spacing: .08em; color: var(--bs-danger); }
.bw-sit-batter { border-top: 1px solid var(--bs-border-color); padding-top: .35rem; }
.bw-batter-name { font-weight: 700; }
@media (max-width: 375px) {
  .bw-score-side .bw-runs { font-size: 1.6rem; }
  .bw-sit-mid .bw-count { font-size: 1.3rem; }
}
```

- [ ] **Step 6: Run the tests and the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_situation_card.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_boxscore.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0.

- [ ] **Step 7: Commit**

```bash
git add R/situation_card.R R/boxscore.R R/tracking_module.R www/css/app.css tests/test_situation_card.R tests/test_boxscore.R
git commit -m "feat: one-glance situation card; share the batting accumulator"
```

---

### Task 5: Tracking layout

**Files:**
- Modify: `R/tracking_module.R` (`tracking_ui`)
- Modify: `README.md`

**Interfaces:** none new.

- [ ] **Step 1: Reorder the tracking view**

`tracking_ui()` currently puts Undo and Substitution above the tabs and gives them equal
weight with the outcome grid. Restructure so the primary action — recording a play — is
unmistakably primary:

```r
tracking_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("situation")),
    uiOutput(ns("action_panel")),
    div(class = "d-flex gap-2 mt-2 bw-secondary-actions",
        actionButton(ns("undo"), "Undo", class = "btn-sm btn-outline-warning"),
        actionButton(ns("sub"), "Substitution", class = "btn-sm btn-outline-secondary")),
    navset_tab(
      nav_panel("Scorebook",
        div(class = "p-2",
          radioButtons(ns("sb_team"), NULL, c("Away" = "away", "Home" = "home"),
                       selected = "away", inline = TRUE),
          uiOutput(ns("scorebook")))),
      nav_panel("Box score",
        div(class = "p-2",
          uiOutput(ns("box_away_hdr")), DT::DTOutput(ns("box_away")),
          uiOutput(ns("box_home_hdr")), DT::DTOutput(ns("box_home")))),
      nav_panel("Help", outcome_help_ui())))
}
```

Append to `www/css/app.css`:

```css
.bw-secondary-actions { justify-content: flex-end; }
```

- [ ] **Step 2: Update the README**

In `## Known limitations`, replace the scorebook bullet set with what is now true:

```markdown
- The scorebook scrolls horizontally on narrow screens; the name column is inside the SVG
  and does not stay pinned.
- Undo reverts in-session; persisted event rows are pruned in a later phase.
```

In `## Rules supported`, add the slice 2.1 additions:

```markdown
Presets: Anything Goes (default), Standard Baseball, Standard Slowpitch Softball,
Standard Fastpitch Softball, GameOn Summer, GameOn Spring. Over-the-fence home-run limits
(overall or per gender) with a configurable result past the limit; inside-the-park home
runs are tracked separately and exempt by default. Pinch/courtesy runners with per-inning,
per-game, and per-player limits plus eligibility rules. Mercy as a schedule of tiers.
Run caps that either count same-play runs or truncate, and that end the half-inning
when reached.
```

- [ ] **Step 3: Run the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: exit 0.

- [ ] **Step 4: Full manual pass**

Run the app end to end: pick GameOn Summer, enter both lineups, score two innings including
a batting-around inning, a pinch runner, a substitution, and a home run past the 3-HR limit.
Confirm every panel is correct and readable at 375px.

- [ ] **Step 5: Commit**

```bash
git add R/tracking_module.R www/css/app.css README.md
git commit -m "feat: tracking layout with Help tab and secondary action placement"
```

---

## Definition of done for slice 2.3

- `Rscript run_tests.R` exits 0.
- A runner standing on third shows three heavy legs; scoring fills the diamond. Runs by anyone, not just home runs, appear.
- Mid-inning, the scorebook shows where every runner currently is.
- Batting around draws side-by-side cells, never overlapping ones.
- The current batter's row is bold, and both teams' scorebooks are reachable.
- The situation card shows score, inning, outs, count, batter, and the batter's line in one glance at 375px.
- The Help tab lists all 18 outcome codes.
