# Bookworm Slice 2.2 — Play Entry and Substitutions Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Stop inferring where runners went. Ask the scorer, with the inference as a pre-fill. Make the Substitution button work, and record the runner-origin bookkeeping the scorebook needs.

**Architecture:** The disposition modal is built from pure functions in a new `R/disposition.R` — `disposition_rows()`, `disposition_prefill()`, `validate_disposition()`, `disposition_payload()`. The Shiny layer only renders those and collects inputs, so the whole play-entry rule set is unit-testable without a browser. `suggest_advances()` survives unchanged as the pre-fill source; it stops being authoritative. The reducer gains origin tracking so slice 2.3 can reconstruct each runner's full trip.

**Tech Stack:** R 4.5.3, Shiny 1.13.0, bslib 0.10.0, testthat 3.3.2.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-08-08-bookworm-slice-two-design.md`, section "Slice 2.2".
- **Depends on slice 2.1 being merged.** `evaluate_home_run_limit()`, `evaluate_pinch_runner()`, `state$pinch_runner_log`, and the nested ruleset schema must exist. Depends on slice 2.0 for `APP_CONFIG$outcome_meta`.
- **`outcome_button()` does not exist.** An earlier draft of this plan depended on it. Slice 2.0's final review found `bslib::popover()` fires on the same tap that records a play, and the owner ruled the per-button popovers out; the function was deleted. Outcome buttons are plain `actionButton(ns(paste0("o_", code)), code, class = "btn-outline-primary bw-outcome-btn")`. Do not reintroduce a popover on them.
- **All five rule evaluators return the same item shape.** Slice 2.1's final review unified them: `evaluate_fielding`, `evaluate_home_run_limit`, `evaluate_pinch_runner`, and `validate_lineup` all yield `list(severity =, code =, message =)` items, and the two that also report success keep an `ok` field. `evaluate_pinch_runner` returns `list(ok =, items =)` — **not** `errors`. The `code` field is what lets these messages use the existing toast dedupe from commit `7efcf48`; prefer surfacing them through that machinery rather than hand-rolling strings.
- **Mercy now evaluates only at a half-inning boundary,** with `after_inning` meaning *completed* innings, and `state$status` is **derived** rather than latched — an Undo can reopen a game that had gone final. Do not reintroduce a latch when wiring Undo in this slice.
- Run every command from the **project root**. Rscript: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe"`.
- `Rscript run_tests.R` must exit 0 at the end of **every task**.
- Events written by slice 1.1 must still fold. `apply_plate_appearance()`'s `reached` fallback stays as a compatibility path.
- Nothing in `R/disposition.R` may reference Shiny.

---

### Task 1: Disposition pure functions

**Files:**
- Create: `R/disposition.R`
- Test: `tests/test_disposition.R` (create)

**Interfaces:**
- Produces:
  - `disposition_rows(state)` → ordered list of `list(runner_id=, from=, name=, jersey=)`. Lead runner first (third, second, first), batter last, `from = 0L` for the batter.
  - `disposition_prefill(state, outcome)` → named character vector, `runner_id` → one of `"1" "2" "3" "H" "OUT"`.
  - `validate_disposition(rows, choices)` → `list(ok=, errors=<chr>)`.
  - `disposition_payload(state, outcome, choices, rbi = NULL)` → a `plate_appearance` payload list.
  - `resolve_outcome(cfg, state, outcome)` → `list(outcome=, warning=)`, delegating to `evaluate_home_run_limit()`.
  - `DISPOSITION_LEVELS` → `c("1","2","3","H","OUT")`.

- [ ] **Step 1: Write the failing test**

Create `tests/test_disposition.R`:

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","rule_presets.R","rule_home_run.R",
            "game_events.R","game_reducer.R","disposition.R"))
  source(file.path("R", f))

mk <- function(prefix, n = 4L) lapply(seq_len(n), function(i)
  make_player(paste0(prefix, i), paste(prefix, i), c("M","F")[(i %% 2) + 1L], i, i, i))

st0 <- function(cfg = default_ruleset_config()) {
  s <- initial_game_state(cfg)
  s$lineups$away <- mk("a"); s$lineups$home <- mk("h")
  s$batting_team <- "away"
  s$current_batter <- s$lineups$away[[1]]
  s
}

test_that("rows are lead-runner-first with the batter last", {
  s <- st0(); s$bases <- list(first = "a4", second = NA_character_, third = "a3")
  rows <- disposition_rows(s)
  expect_equal(vapply(rows, function(r) r$runner_id, character(1)), c("a3","a4","a1"))
  expect_equal(vapply(rows, function(r) r$from, integer(1)), c(3L, 1L, 0L))
})

test_that("with the bases empty only the batter appears", {
  rows <- disposition_rows(st0())
  expect_length(rows, 1L)
  expect_equal(rows[[1]]$from, 0L)
})

test_that("prefill on a single matches suggest_advances", {
  s <- st0(); s$bases$first <- "a4"
  pf <- disposition_prefill(s, "1B")
  expect_equal(unname(pf[["a4"]]), "2")   # forced to second
  expect_equal(unname(pf[["a1"]]), "1")   # batter to first
})

test_that("prefill on a home run scores everyone", {
  s <- st0(); s$bases <- list(first = "a4", second = "a3", third = "a2")
  pf <- disposition_prefill(s, "HR")
  expect_true(all(pf == "H"))
})

test_that("prefill on a strikeout holds the runners and outs the batter", {
  s <- st0(); s$bases$second <- "a3"
  pf <- disposition_prefill(s, "K")
  expect_equal(unname(pf[["a3"]]), "2")     # holds
  expect_equal(unname(pf[["a1"]]), "OUT")
})

test_that("prefill treats a sacrifice as an out for the batter", {
  s <- st0(); s$bases$first <- "a4"
  expect_equal(unname(disposition_prefill(s, "SF")[["a1"]]), "OUT")
  expect_equal(unname(disposition_prefill(s, "SAC")[["a1"]]), "OUT")
})

test_that("a walk only forces the runners who must move", {
  s <- st0(); s$bases <- list(first = "a4", second = NA_character_, third = "a2")
  pf <- disposition_prefill(s, "BB")
  expect_equal(unname(pf[["a4"]]), "2")   # forced
  expect_equal(unname(pf[["a2"]]), "3")   # not forced, holds third
})

test_that("validation rejects two runners on the same base", {
  s <- st0(); s$bases$first <- "a4"
  rows <- disposition_rows(s)
  r <- validate_disposition(rows, c(a4 = "2", a1 = "2"))
  expect_false(r$ok)
  expect_match(paste(r$errors, collapse = " "), "same base|second")
})

test_that("two runners may both score and both be out", {
  s <- st0(); s$bases$first <- "a4"
  rows <- disposition_rows(s)
  expect_true(validate_disposition(rows, c(a4 = "H", a1 = "H"))$ok)
  expect_true(validate_disposition(rows, c(a4 = "OUT", a1 = "OUT"))$ok)
})

test_that("validation rejects a runner moving backwards", {
  s <- st0(); s$bases$third <- "a2"
  rows <- disposition_rows(s)
  r <- validate_disposition(rows, c(a2 = "1", a1 = "1"))
  expect_false(r$ok)
  expect_match(paste(r$errors, collapse = " "), "back")
})

test_that("a runner holding their base is fine", {
  s <- st0(); s$bases$third <- "a2"
  rows <- disposition_rows(s)
  expect_true(validate_disposition(rows, c(a2 = "3", a1 = "1"))$ok)
})

test_that("validation requires a choice for every runner", {
  s <- st0(); s$bases$first <- "a4"
  rows <- disposition_rows(s)
  r <- validate_disposition(rows, c(a1 = "1"))
  expect_false(r$ok)
  expect_match(paste(r$errors, collapse = " "), "a4|every runner")
})

test_that("validation rejects an unknown level", {
  rows <- disposition_rows(st0())
  expect_false(validate_disposition(rows, c(a1 = "5"))$ok)
})

test_that("payload derives outs, runs, and the RBI default", {
  s <- st0(); s$bases <- list(first = "a4", second = NA_character_, third = "a2")
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
  s <- st0(); s$bases$third <- "a2"
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
  s <- st0(); s$bases$first <- "a4"
  p <- disposition_payload(s, "2B", c(a4 = "H", a1 = "2"))
  s2 <- apply_plate_appearance(s, list(payload = p))
  expect_equal(s2$bases$second, "a1")
  expect_true(is.na(s2$bases$first))
  expect_equal(s2$score$away, 1L)
})

test_that("resolve_outcome rewrites a home run past the limit", {
  cfg <- coerce_ruleset_config(list(home_run_rule = list(over_fence_limit = 1L)))
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_disposition.R")'`
Expected: FAIL — cannot open `R/disposition.R`.

- [ ] **Step 3: Create `R/disposition.R`**

```r
# Runner disposition: what happened to every runner on the play.
# Pure — no Shiny. The tracking module renders these rows and collects the choices;
# every rule about what a legal disposition is lives here.

DISPOSITION_LEVELS <- c("1", "2", "3", "H", "OUT")

# Outcomes where the batter is out unless the scorer says otherwise. SF and SAC are
# outs for the batter even though they are not in .OUT_OUTCOMES (they are not at-bats).
.BATTER_OUT_OUTCOMES <- c("K", "KL", "GO", "FO", "LO", "PO", "SF", "SAC")

.BASE_FROM <- c(first = 1L, second = 2L, third = 3L)

.lookup_player <- function(state, team, player_id) {
  hit <- Filter(function(p) identical(p$player_id, player_id),
                state$lineups[[team]] %||% list())
  if (length(hit)) hit[[1]] else NULL
}

# Lead runner first (third, second, first), batter last with from = 0.
# Lead-first matters: it is the order a scorer reads the field in.
disposition_rows <- function(state) {
  team <- state$batting_team
  rows <- list()
  for (base in c("third", "second", "first")) {
    id <- state$bases[[base]]
    if (is.null(id) || is.na(id)) next
    p <- .lookup_player(state, team, id)
    rows[[length(rows) + 1L]] <- list(
      runner_id = id, from = unname(.BASE_FROM[[base]]),
      name = p$name %||% id, jersey = p$jersey_number %||% NA_integer_)
  }
  b <- state$current_batter
  if (!is.null(b))
    rows[[length(rows) + 1L]] <- list(
      runner_id = b$player_id, from = 0L, name = b$name,
      jersey = b$jersey_number %||% NA_integer_)
  rows
}

# Pre-fill from suggest_advances(). That function is no longer authoritative — it only
# saves the scorer taps on the common case.
disposition_prefill <- function(state, outcome) {
  by_id <- list()
  for (a in suggest_advances(state, outcome))
    by_id[[a$runner_id]] <- if (isTRUE(a$scored)) "H" else as.character(a$to)

  rows <- disposition_rows(state)
  out <- character(0)
  for (r in rows) {
    lvl <- by_id[[r$runner_id]]
    if (is.null(lvl)) {
      lvl <- if (r$from == 0L) {
        if (outcome %in% .BATTER_OUT_OUTCOMES) "OUT" else "1"
      } else as.character(r$from)   # runners hold
    }
    out[[r$runner_id]] <- lvl
  }
  out
}

validate_disposition <- function(rows, choices) {
  errors <- character()
  add <- function(m) errors <<- c(errors, m)

  for (r in rows) {
    lvl <- choices[[r$runner_id]]
    if (is.null(lvl) || is.na(lvl) || !nzchar(lvl)) {
      add(sprintf("Say what happened to %s.", r$name)); next
    }
    if (!lvl %in% DISPOSITION_LEVELS) {
      add(sprintf("%s: \"%s\" is not a valid destination.", r$name, lvl)); next
    }
    if (lvl %in% c("1", "2", "3") && as.integer(lvl) < r$from)
      add(sprintf("%s cannot go back from %s to %s.", r$name,
                  c("home", "first", "second", "third")[r$from + 1L],
                  c("first", "second", "third")[as.integer(lvl)]))
  }
  if (length(errors)) return(list(ok = FALSE, errors = errors))

  # Two runners may both score and both be out, but only one may occupy a base.
  placed <- vapply(rows, function(r) choices[[r$runner_id]], character(1))
  on_base <- placed[placed %in% c("1", "2", "3")]
  for (b in unique(on_base[duplicated(on_base)]))
    add(sprintf("Two runners cannot end on the same base (%s).",
                c("first", "second", "third")[as.integer(b)]))

  list(ok = length(errors) == 0, errors = errors)
}

# Builds the plate_appearance payload. `rbi` defaults to the number of runners who
# scored; the scorer can override it for errors and sacrifices.
disposition_payload <- function(state, outcome, choices, rbi = NULL) {
  rows <- disposition_rows(state)
  advances <- lapply(rows, function(r) {
    lvl <- choices[[r$runner_id]]
    if (identical(lvl, "OUT"))
      make_advance(r$runner_id, r$from, r$from, out = TRUE)
    else if (identical(lvl, "H"))
      make_advance(r$runner_id, r$from, 4L, scored = TRUE)
    else
      make_advance(r$runner_id, r$from, as.integer(lvl))
  })
  outs <- sum(vapply(advances, function(a) isTRUE(a$out), logical(1)))
  runs <- sum(vapply(advances, function(a) isTRUE(a$scored), logical(1)))

  batter <- Filter(function(a) identical(a$from, 0L), advances)
  reached <- if (!length(batter)) NA_integer_
             else if (isTRUE(batter[[1]]$out)) NA_integer_
             else as.integer(batter[[1]]$to)

  list(team = state$batting_team,
       batter_id = state$current_batter$player_id,
       outcome = outcome, reached = reached,
       rbi = as.integer(rbi %||% runs),
       outs_on_play = as.integer(outs),
       advances = advances)
}

# Applies rule rewrites that depend on game state before the outcome is committed.
# Today that is only the home-run limit.
resolve_outcome <- function(cfg, state, outcome) {
  batter <- state$current_batter
  if (is.null(batter)) return(list(outcome = outcome, warning = NULL))
  evaluate_home_run_limit(cfg, state, batter, outcome)
}
```

- [ ] **Step 4: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_disposition.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0.

- [ ] **Step 5: Commit**

```bash
git add R/disposition.R tests/test_disposition.R
git commit -m "feat: pure runner-disposition rules with suggest_advances as pre-fill"
```

---

### Task 2: Runner origin tracking in the reducer

**Files:**
- Modify: `R/game_reducer.R` (`initial_game_state`, `advance_half`, `apply_plate_appearance`, `apply_substitution`)
- Test: `tests/test_reducer_pa.R` (extend)

**Interfaces:**
- Produces:
  - `state$runner_origin` — named list, `runner_id` → 1-based `pa_log` index of the plate appearance that put them on base.
  - Each advance stored in a `pa_log` entry carries `origin_index` (`<int>` or `NA_integer_`).
  - Each `pa_log` entry gains `pa_index_in_half` (`<int>`, 1-based per team per half), `outs_before` (`<int>`), and `advances`.
- Consumed by slice 2.3's `runner_paths()`.

- [ ] **Step 1: Write the failing test**

Append to `tests/test_reducer_pa.R`:

```r
test_that("pa_log entries carry advances, pa_index_in_half, and outs_before", {
  s <- fold_events(list(start_evt(),
    pa("a1", "1B", 1L, advances = list(make_advance("a1", 0L, 1L)), seq = 2L),
    pa("a2", "K", NA_integer_, outs = 1L,
       advances = list(make_advance("a2", 0L, 0L, out = TRUE)), seq = 3L)))
  expect_equal(vapply(s$pa_log, function(r) r$pa_index_in_half, integer(1)), c(1L, 2L))
  expect_equal(vapply(s$pa_log, function(r) r$outs_before, integer(1)), c(0L, 0L))
  expect_length(s$pa_log[[1]]$advances, 1L)
})

test_that("pa_index_in_half restarts each half", {
  evs <- list(start_evt(),
    pa("a1", "K", NA_integer_, outs = 1L, seq = 2L),
    pa("a2", "K", NA_integer_, outs = 1L, seq = 3L),
    pa("a3", "K", NA_integer_, outs = 1L, seq = 4L))
  s <- fold_events(evs)
  expect_equal(s$half, "bottom")
  expect_equal(vapply(s$pa_log, function(r) r$pa_index_in_half, integer(1)), c(1L, 2L, 3L))
})

test_that("the batter's own advance records the index of its own plate appearance", {
  s <- fold_events(list(start_evt(),
    pa("a1", "1B", 1L, advances = list(make_advance("a1", 0L, 1L)), seq = 2L)))
  own <- Filter(function(a) identical(a$from, 0L), s$pa_log[[1]]$advances)
  expect_equal(own[[1]]$origin_index, 1L)
  expect_equal(s$runner_origin[["a1"]], 1L)
})

test_that("a later advance is attributed to the runner's originating plate appearance", {
  s <- fold_events(list(start_evt(),
    pa("a1", "1B", 1L, advances = list(make_advance("a1", 0L, 1L)), seq = 2L),
    pa("a2", "1B", 1L, advances = list(
      make_advance("a1", 1L, 2L), make_advance("a2", 0L, 1L)), seq = 3L)))
  a1_move <- Filter(function(a) identical(a$runner_id, "a1"), s$pa_log[[2]]$advances)
  expect_equal(a1_move[[1]]$origin_index, 1L)   # belongs to a1's own PA, not a2's
  expect_equal(s$runner_origin[["a1"]], 1L)
  expect_equal(s$runner_origin[["a2"]], 2L)
})

test_that("a runner who scores or is out leaves runner_origin", {
  s <- fold_events(list(start_evt(),
    pa("a1", "1B", 1L, advances = list(make_advance("a1", 0L, 1L)), seq = 2L),
    pa("a2", "HR", 4L, rbi = 2L, advances = list(
      make_advance("a1", 1L, 4L, scored = TRUE),
      make_advance("a2", 0L, 4L, scored = TRUE)), seq = 3L)))
  expect_null(s$runner_origin[["a1"]])
  expect_null(s$runner_origin[["a2"]])
})

test_that("advance_half clears runner_origin", {
  s <- fold_events(list(start_evt(),
    pa("a1", "1B", 1L, advances = list(make_advance("a1", 0L, 1L)), seq = 2L),
    pa("a2", "K", NA_integer_, outs = 1L, seq = 3L),
    pa("a3", "K", NA_integer_, outs = 1L, seq = 4L),
    pa("a4", "K", NA_integer_, outs = 1L, seq = 5L)))
  expect_equal(s$half, "bottom")
  expect_length(s$runner_origin, 0L)
})

test_that("a pinch runner inherits the original batter's origin", {
  s <- fold_events(list(start_evt(),
    pa("a1", "1B", 1L, advances = list(make_advance("a1", 0L, 1L)), seq = 2L),
    new_event("substitution", list(team = "away", kind = "courtesy_runner",
      out_player_id = "a1",
      in_player = make_player("a9", "Sub", "M", 9L, NA_integer_, NA_character_)), seq = 3L)))
  expect_equal(s$bases$first, "a9")
  expect_equal(s$runner_origin[["a9"]], 1L)
  expect_null(s$runner_origin[["a1"]])
})

test_that("a runner with no recorded origin gets NA rather than erroring", {
  s0 <- fold_events(list(start_evt()))
  s0$bases$second <- "ghost"          # placed without a plate appearance
  s1 <- apply_plate_appearance(s0, list(payload = list(
    team = "away", batter_id = "a1", outcome = "1B", reached = 1L, rbi = 0L,
    outs_on_play = 0L,
    advances = list(make_advance("ghost", 2L, 3L), make_advance("a1", 0L, 1L)))))
  ghost <- Filter(function(a) identical(a$runner_id, "ghost"), s1$pa_log[[1]]$advances)
  expect_true(is.na(ghost[[1]]$origin_index))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_reducer_pa.R")'`
Expected: FAIL — `pa_index_in_half` is NULL.

- [ ] **Step 3: Seed the new state fields**

In `initial_game_state()`, add to the list:

```r
    runner_origin = list(), pa_counter = list(home = 0L, away = 0L),
```

The state field is `pa_counter` — a per-team running counter — while the value it writes
onto each `pa_log` entry is `pa_index_in_half`. Two names for two things: do not collapse
them, or the per-entry value will be read as the live counter.

In `advance_half()`, clear both — a new half starts with empty bases, so no origins carry
over. Add before `state <- reset_count(state)`:

```r
  state$runner_origin <- list()
  state$pa_counter[[state$batting_team]] <- 0L
```

Careful with ordering: `advance_half()` flips `state$batting_team` before this point, so
resetting the counter for the *new* batting team is what you want. The outgoing team's
counter is reset when its next half begins.

- [ ] **Step 4: Track origins in `apply_plate_appearance()`**

Rewrite the top of the function so origins are resolved before the advances are stored.
`own_index` is the slot this entry is about to occupy.

```r
apply_plate_appearance <- function(state, evt) {
  p <- evt$payload
  team <- state$batting_team
  bases <- state$bases
  runs <- 0L
  own_index <- length(state$pa_log) + 1L
  origin <- state$runner_origin %||% list()

  advances <- p$advances %||% list()
  # Resolve each advance to the plate appearance that put that runner on base. The
  # batter's own advance belongs to this entry; everyone else looks theirs up.
  advances <- lapply(advances, function(a) {
    a$origin_index <- if (identical(as.integer(a$from), 0L)) own_index
                      else (origin[[a$runner_id]] %||% NA_integer_)
    a
  })

  for (a in advances) {
    bases <- .clear_base_of(bases, a$runner_id)
    if (isTRUE(a$scored)) {
      runs <- runs + 1L
      origin[[a$runner_id]] <- NULL
    } else if (isTRUE(a$out)) {
      origin[[a$runner_id]] <- NULL
    } else if (a$to %in% c(1L, 2L, 3L)) {
      bases[[.base_slot(a$to)]] <- a$runner_id
      origin[[a$runner_id]] <- a$origin_index
    }
  }
```

Keep the legacy `reached` fallback block that follows unchanged, but register the batter's
origin in it too, since a slice 1.1 event has no batter advance:

```r
  reached <- p$reached %||% NA_integer_
  if (!is.na(reached) && reached %in% c(1L, 2L, 3L)) {
    already <- any(vapply(advances, function(a) identical(a$runner_id, p$batter_id), logical(1)))
    if (!already) {
      bases[[.base_slot(reached)]] <- p$batter_id
      origin[[p$batter_id]] <- own_index
    }
  }
```

Then set the state fields and extend the `pa_log` entry:

```r
  state$bases <- bases
  state$runner_origin <- origin
  state$pa_counter[[team]] <- (state$pa_counter[[team]] %||% 0L) + 1L
  outs_before <- state$outs
```

and in the `pa_log` append, add the three new fields:

```r
    advances = advances,
    pa_index_in_half = state$pa_counter[[team]],
    outs_before = as.integer(outs_before),
```

`outs_before` must be captured **before** `state$outs <- state$outs + ...`. Place the
`outs_before <- state$outs` line above that increment.

- [ ] **Step 5: Transfer the origin on a pinch runner**

In `apply_substitution()`'s `courtesy_runner` branch, after the base swap and before the
`pinch_runner_log` append (added by slice 2.1 Task 6):

```r
    origin <- state$runner_origin %||% list()
    if (!is.null(origin[[p$out_player_id]])) {
      origin[[p$in_player$player_id]] <- origin[[p$out_player_id]]
      origin[[p$out_player_id]] <- NULL
      state$runner_origin <- origin
    }
```

- [ ] **Step 6: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_reducer_pa.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0.

- [ ] **Step 7: Commit**

```bash
git add R/game_reducer.R tests/test_reducer_pa.R
git commit -m "feat: track runner origins, per-half PA index, and outs before each play"
```

---

### Task 3: Disposition modal

**Files:**
- Modify: `R/tracking_module.R` (`record`, `output$action_panel`, new observers)
- Create: `R/disposition_ui.R`
- Modify: `www/css/app.css`
- Test: `tests/test_disposition_ui.R` (create)

**Interfaces:**
- Produces:
  - `disposition_modal_ui(ns, rows, prefill, outcome)` → a `modalDialog`.
  - `read_disposition_choices(input, rows)` → named character vector for `validate_disposition()`.
- Consumes: everything from Task 1.

- [ ] **Step 1: Write the failing test**

Create `tests/test_disposition_ui.R`:

```r
library(testthat)
suppressMessages({ library(shiny); library(bslib); library(htmltools) })
for (f in c("app_config.R","rules_engine.R","rule_presets.R","rule_home_run.R",
            "game_events.R","game_reducer.R","disposition.R","disposition_ui.R"))
  source(file.path("R", f))

mk <- function(prefix, n = 4L) lapply(seq_len(n), function(i)
  make_player(paste0(prefix, i), paste(prefix, i), c("M","F")[(i %% 2) + 1L], i, i, i))
st <- function() {
  s <- initial_game_state(); s$lineups$away <- mk("a"); s$batting_team <- "away"
  s$current_batter <- s$lineups$away[[1]]; s$bases$first <- "a4"; s
}

test_that("the modal renders one control group per runner", {
  s <- st(); rows <- disposition_rows(s)
  html <- as.character(disposition_modal_ui(NS("track"), rows,
                                            disposition_prefill(s, "1B"), "1B"))
  expect_true(grepl("track-disp_a4", html, fixed = TRUE))
  expect_true(grepl("track-disp_a1", html, fixed = TRUE))
  expect_true(grepl("track-disp_commit", html, fixed = TRUE))
  expect_true(grepl("track-disp_rbi", html, fixed = TRUE))
})

test_that("the modal names each runner and their current base", {
  s <- st(); rows <- disposition_rows(s)
  html <- as.character(disposition_modal_ui(NS("track"), rows,
                                            disposition_prefill(s, "1B"), "1B"))
  expect_true(grepl("a 4", html, fixed = TRUE) || grepl("a4", html, fixed = TRUE))
  expect_true(grepl("first", html, ignore.case = TRUE))
  expect_true(grepl("batter", html, ignore.case = TRUE))
})

test_that("every level is offered for every runner", {
  s <- st(); rows <- disposition_rows(s)
  html <- as.character(disposition_modal_ui(NS("track"), rows,
                                            disposition_prefill(s, "1B"), "1B"))
  for (lvl in DISPOSITION_LEVELS)
    expect_true(grepl(paste0('value="', lvl, '"'), html, fixed = TRUE), info = lvl)
})

test_that("read_disposition_choices pulls one value per row", {
  s <- st(); rows <- disposition_rows(s)
  input <- list(disp_a4 = "2", disp_a1 = "1")
  ch <- read_disposition_choices(input, rows)
  expect_equal(unname(ch[["a4"]]), "2")
  expect_equal(unname(ch[["a1"]]), "1")
  expect_length(ch, 2L)
})

test_that("a missing input becomes an empty string so validation catches it", {
  s <- st(); rows <- disposition_rows(s)
  ch <- read_disposition_choices(list(disp_a1 = "1"), rows)
  expect_equal(unname(ch[["a4"]]), "")
  expect_false(validate_disposition(rows, ch)$ok)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_disposition_ui.R")'`
Expected: FAIL — cannot open `R/disposition_ui.R`.

- [ ] **Step 3: Create `R/disposition_ui.R`**

```r
# The runner-disposition modal. Rendering only — every rule lives in R/disposition.R.

.BASE_LABEL <- c("batter", "first", "second", "third")

.disposition_row_ui <- function(ns, row, selected) {
  label <- paste0(
    if (!is.na(row$jersey)) paste0("#", row$jersey, " ") else "",
    row$name, "  (", .BASE_LABEL[[row$from + 1L]], ")")
  tags$div(class = "bw-disp-row d-flex align-items-center justify-content-between gap-2",
    tags$span(class = "bw-disp-name", label),
    radioButtons(ns(paste0("disp_", row$runner_id)), NULL,
      choices = stats::setNames(DISPOSITION_LEVELS, DISPOSITION_LEVELS),
      selected = selected, inline = TRUE))
}

disposition_modal_ui <- function(ns, rows, prefill, outcome, errors = character()) {
  lbl <- APP_CONFIG$outcome_meta[[outcome]]$label %||% outcome
  runs_default <- sum(prefill == "H")
  modalDialog(
    title = sprintf("%s — where did everyone end up?", lbl),
    tags$p(class = "text-muted small",
      "1, 2, 3 = the base they finished on. H = scored. OUT = out on the play."),
    tags$div(class = "bw-disp-grid",
      !!!lapply(rows, function(r) .disposition_row_ui(ns, r, prefill[[r$runner_id]]))),
    if (length(errors))
      tags$div(class = "alert alert-danger py-2 small mt-2",
        tags$ul(class = "mb-0", !!!lapply(errors, tags$li))),
    tags$hr(),
    numericInput(ns("disp_rbi"), "RBI", value = runs_default, min = 0, max = 4,
                 width = "7rem"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("disp_commit"), "Commit play", class = "btn-primary")),
    easyClose = FALSE, size = "m")
}

read_disposition_choices <- function(input, rows) {
  out <- character(0)
  for (r in rows) {
    v <- input[[paste0("disp_", r$runner_id)]]
    out[[r$runner_id]] <- if (is.null(v) || length(v) != 1 || is.na(v)) "" else as.character(v)
  }
  out
}
```

- [ ] **Step 4: Wire the modal into `tracking_server()`**

Replace `record()` and add the commit observer. `pending` holds the play between the
outcome tap and the commit.

```r
    pending <- reactiveVal(NULL)   # list(outcome =, rows =, prefill =)

    commit_payload <- function(payload) {
      evt <- new_event("plate_appearance", payload)
      appended <- storage$append_event(game_id, evt)
      events(c(isolate(events()), list(appended)))
      storage$save_snapshot(game_id, isolate(state()))
    }

    record <- function(outcome) {
      s <- isolate(state())
      if (identical(s$status, "final")) return(invisible())
      res <- resolve_outcome(s$ruleset, s, outcome)
      if (!is.null(res$warning)) showNotification(res$warning$message, type = "warning",
                                                  duration = 6)
      outcome <- res$outcome
      rows <- disposition_rows(s)
      # Bases empty: only the batter is involved, so there is nothing to ask.
      if (length(rows) <= 1L) {
        prefill <- disposition_prefill(s, outcome)
        commit_payload(disposition_payload(s, outcome, prefill))
        return(invisible())
      }
      prefill <- disposition_prefill(s, outcome)
      pending(list(outcome = outcome, rows = rows, prefill = prefill))
      showModal(disposition_modal_ui(session$ns, rows, prefill, outcome))
    }

    observeEvent(input$disp_commit, {
      pd <- isolate(pending()); req(pd)
      s <- isolate(state())
      choices <- read_disposition_choices(input, pd$rows)
      v <- validate_disposition(pd$rows, choices)
      if (!v$ok) {
        showModal(disposition_modal_ui(session$ns, pd$rows, choices, pd$outcome, v$errors))
        return(invisible())
      }
      removeModal()
      pending(NULL)
      commit_payload(disposition_payload(s, pd$outcome, choices, rbi = input$disp_rbi))
    }, ignoreInit = TRUE)
```

Re-showing the modal with `choices` as the prefill preserves what the scorer selected
rather than resetting to the inference.

- [ ] **Step 5: Style the modal**

Append to `www/css/app.css`:

```css
/* Runner disposition modal: one row per runner, radio group on the right. */
.bw-disp-row { padding: .35rem 0; border-bottom: 1px solid var(--bs-border-color); }
.bw-disp-row:last-child { border-bottom: 0; }
.bw-disp-name { font-weight: 600; }
.bw-disp-row .shiny-input-container { margin-bottom: 0; }
.bw-disp-row .radio-inline, .bw-disp-row .form-check-inline { margin-right: .5rem; }
@media (max-width: 576px) {
  .bw-disp-row { flex-direction: column; align-items: flex-start !important; gap: .1rem; }
}
```

- [ ] **Step 6: Run the tests and the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_disposition_ui.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0. `tests/test_tracking_module.R` tests `record_outcome_event()`,
which is now unused by `record()`. Keep the function and its tests — slice 2.3's tests
still reference it as the inference baseline — or delete both together. Do not leave a
tested-but-dead function without a note.

- [ ] **Step 7: Manual check**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'shiny::runApp(".", port = 4321, launch.browser = FALSE)'`

Confirm: a single with the bases empty commits on one tap; a single with a runner on first
opens the modal pre-filled with the runner going to second; setting both to second is
rejected with an inline message; Commit records the play.

- [ ] **Step 8: Commit**

```bash
git add R/disposition_ui.R R/tracking_module.R www/css/app.css tests/test_disposition_ui.R
git commit -m "feat: ask where every runner ended up instead of inferring it"
```

---

### Task 4: Substitution modal

**Files:**
- Create: `R/substitution_ui.R`
- Modify: `R/tracking_module.R` (`input$sub` observer)
- Modify: `R/game_reducer.R` (`apply_substitution` builds players with `make_player()`)
- Test: `tests/test_reducer_subs.R` (extend), `tests/test_substitution_ui.R` (create)

**Interfaces:**
- Produces:
  - `substitution_modal_ui(ns, state, kind, errors = character())` → a `modalDialog`.
  - `build_substitution_event(input, state, kind)` → a `substitution` event, or
    `list(errors = <chr>)` when the inputs are incomplete.

- [ ] **Step 1: Write the failing tests**

Append to `tests/test_reducer_subs.R`:

```r
test_that("a batting substitution keeps every player field", {
  st <- initial_game_state()
  st$lineups$away <- list(make_player("a1", "A1", "M", 1L, 1L, "SS"))
  evt <- new_event("substitution", list(team = "away", kind = "batting", order_slot = 1L,
    in_player = make_player("a9", "Sub", "F", 22L, NA_integer_, "2B")))
  s <- apply_substitution(st, evt)
  p <- s$lineups$away[[1]]
  expect_equal(p$player_id, "a9")
  expect_equal(p$name, "Sub")
  expect_equal(p$gender, "F")
  expect_equal(p$jersey_number, 22L)
  expect_equal(p$order_slot, 1L)      # takes the slot it replaced
  expect_equal(p$position, "2B")      # keeps its own position
})

test_that("a defensive substitution keeps the batting order slot", {
  st <- initial_game_state()
  st$lineups$home <- list(make_player("h1", "H1", "M", 1L, 3L, "P"))
  evt <- new_event("substitution", list(team = "home", kind = "defensive",
    out_player_id = "h1", position = "LF",
    in_player = make_player("h9", "Sub", "M", 44L, NA_integer_, NA_character_)))
  s <- apply_substitution(st, evt)
  p <- s$lineups$home[[1]]
  expect_equal(p$player_id, "h9")
  expect_equal(p$position, "LF")
  expect_equal(p$order_slot, 3L)      # the outgoing player's slot is inherited
})
```

Create `tests/test_substitution_ui.R`:

```r
library(testthat)
suppressMessages({ library(shiny); library(bslib); library(htmltools) })
for (f in c("app_config.R","rules_engine.R","rule_presets.R","rule_pinch_runner.R",
            "game_events.R","game_reducer.R","substitution_ui.R"))
  source(file.path("R", f))

mk <- function(prefix, n = 3L) lapply(seq_len(n), function(i)
  make_player(paste0(prefix, i), paste(prefix, i), c("M","F")[(i %% 2) + 1L], i, i,
              c("P","C","SS")[i]))
st <- function(cfg = default_ruleset_config()) {
  s <- initial_game_state(cfg)
  s$lineups$away <- mk("a"); s$lineups$home <- mk("h")
  s$batting_team <- "away"; s$current_batter <- s$lineups$away[[1]]
  s$bases$second <- "a3"
  s
}

test_that("the batting modal lists both teams' order slots", {
  html <- as.character(substitution_modal_ui(NS("track"), st(), "batting"))
  expect_true(grepl("track-sub_slot", html, fixed = TRUE))
  expect_true(grepl("track-sub_name", html, fixed = TRUE))
  expect_true(grepl("track-sub_commit", html, fixed = TRUE))
})

test_that("the pinch-runner modal offers only occupied bases", {
  html <- as.character(substitution_modal_ui(NS("track"), st(), "courtesy_runner"))
  expect_true(grepl("second", html, ignore.case = TRUE))
  expect_false(grepl(">first<", html, fixed = TRUE))
})

test_that("the defensive modal offers the fielding team's players", {
  html <- as.character(substitution_modal_ui(NS("track"), st(), "defensive"))
  expect_true(grepl("h 1", html, fixed = TRUE) || grepl("h1", html, fixed = TRUE))
  expect_true(grepl("track-sub_pos", html, fixed = TRUE))
})

test_that("build_substitution_event refuses a blank incoming name", {
  r <- build_substitution_event(list(sub_slot = "1", sub_name = "  "), st(), "batting")
  expect_true(length(r$errors) > 0)
})

test_that("build_substitution_event produces a valid batting event", {
  evt <- build_substitution_event(
    list(sub_slot = "1", sub_name = "Sub", sub_gender = "F", sub_jersey = "22",
         sub_pos = "2B"), st(), "batting")
  expect_equal(evt$type, "substitution")
  expect_equal(evt$payload$kind, "batting")
  expect_equal(evt$payload$order_slot, 1L)
  expect_equal(evt$payload$in_player$name, "Sub")
  expect_equal(evt$payload$in_player$jersey_number, 22L)
  expect_true(validate_event(evt)$ok)
})

test_that("a pinch runner past the per-inning limit is rejected", {
  cfg <- coerce_ruleset_config(list(pinch_runner = list(max_per_inning = 1L)))
  s <- st(cfg)
  s$pinch_runner_log <- list(list(inning = s$inning, half = s$half, team = "away",
                                  out_player_id = "x", in_player_id = "y"))
  r <- build_substitution_event(
    list(sub_base = "second", sub_name = "Runner", sub_gender = "M", sub_jersey = "5"),
    s, "courtesy_runner")
  expect_true(length(r$errors) > 0)
  expect_match(paste(r$errors, collapse = " "), "per inning")
})

test_that("an eligible pinch runner produces a valid event", {
  evt <- build_substitution_event(
    list(sub_base = "second", sub_name = "Runner", sub_gender = "M", sub_jersey = "5"),
    st(), "courtesy_runner")
  expect_equal(evt$payload$kind, "courtesy_runner")
  expect_equal(evt$payload$out_player_id, "a3")
  expect_true(validate_event(evt)$ok)
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_substitution_ui.R")'`
Expected: FAIL — cannot open `R/substitution_ui.R`.

- [ ] **Step 3: Fix `apply_substitution()` to build complete players**

The `batting` branch currently takes `p$in_player` as-is and patches only `order_slot`; the
`defensive` branch does the same with `position`. Rebuild through `make_player()` so every
field is present and correctly typed, and so a defensive sub inherits the outgoing player's
batting slot:

```r
apply_substitution <- function(state, evt) {
  p <- evt$payload
  team <- p$team
  inp <- p$in_player
  if (p$kind == "batting") {
    lineup <- state$lineups[[team]]
    for (i in seq_along(lineup)) {
      if (identical(lineup[[i]]$order_slot, as.integer(p$order_slot))) {
        lineup[[i]] <- make_player(inp$player_id, inp$name, inp$gender,
          jersey_number = inp$jersey_number, order_slot = as.integer(p$order_slot),
          position = inp$position)
      }
    }
    state$lineups[[team]] <- lineup
    state <- .set_current_batter(state)
  } else if (p$kind == "defensive") {
    lineup <- state$lineups[[team]]
    for (i in seq_along(lineup)) {
      if (identical(lineup[[i]]$player_id, p$out_player_id)) {
        lineup[[i]] <- make_player(inp$player_id, inp$name, inp$gender,
          jersey_number = inp$jersey_number,
          order_slot = lineup[[i]]$order_slot,   # the slot stays with the lineup spot
          position = p$position)
      }
    }
    state$lineups[[team]] <- lineup
  } else if (p$kind == "courtesy_runner") {
    ...   # unchanged; keeps the origin transfer and pinch_runner_log append
  }
  state
}
```

- [ ] **Step 4: Create `R/substitution_ui.R`**

```r
# Substitution modal: batting, defensive, and pinch/courtesy runner.
# Validation for pinch runners delegates to evaluate_pinch_runner().

.SUB_KINDS <- c("Batting substitution" = "batting",
                "Defensive substitution" = "defensive",
                "Pinch / courtesy runner" = "courtesy_runner")

.fielding_team <- function(state)
  if (identical(state$batting_team, "away")) "home" else "away"

.occupied_bases <- function(state) {
  occ <- c(first = state$bases$first, second = state$bases$second, third = state$bases$third)
  occ[!is.na(occ)]
}

.player_choices <- function(lineup) {
  if (!length(lineup)) return(character(0))
  stats::setNames(vapply(lineup, function(p) p$player_id, character(1)),
                  vapply(lineup, function(p) paste0(
                    if (!is.na(p$jersey_number)) paste0("#", p$jersey_number, " ") else "",
                    p$name), character(1)))
}

.incoming_fields <- function(ns, show_position) {
  tagList(
    tags$h6("Incoming player"),
    layout_columns(col_widths = c(6, 3, 3),
      textInput(ns("sub_name"), "Name"),
      selectInput(ns("sub_gender"), "Gender", c("M" = "M", "F" = "F")),
      textInput(ns("sub_jersey"), "Jersey")),
    if (show_position)
      selectInput(ns("sub_pos"), "Position",
        c("(no position)" = "", stats::setNames(APP_CONFIG$positions, APP_CONFIG$positions))))
}

substitution_modal_ui <- function(ns, state, kind, errors = character()) {
  body <- if (identical(kind, "batting")) {
    slots <- sort(unique(unlist(lapply(c("away", "home"), function(t)
      vapply(Filter(function(p) !is.na(p$order_slot), state$lineups[[t]]),
             function(p) p$order_slot, integer(1))))))
    tagList(
      selectInput(ns("sub_team"), "Team",
        stats::setNames(c("away", "home"),
          c(state$teams$away$name %||% "Away", state$teams$home$name %||% "Home"))),
      selectInput(ns("sub_slot"), "Batting order slot",
        stats::setNames(as.character(slots), paste("Slot", slots))),
      .incoming_fields(ns, show_position = TRUE))
  } else if (identical(kind, "defensive")) {
    ft <- .fielding_team(state)
    tagList(
      tags$p(class = "text-muted small",
        sprintf("Fielding team: %s", state$teams[[ft]]$name %||% ft)),
      selectInput(ns("sub_out"), "Player coming out", .player_choices(state$lineups[[ft]])),
      .incoming_fields(ns, show_position = TRUE))
  } else {
    occ <- .occupied_bases(state)
    if (!length(occ))
      tags$p(class = "text-muted", "No runners on base.")
    else tagList(
      selectInput(ns("sub_base"), "Runner",
        stats::setNames(names(occ), paste0(names(occ), " — ",
          vapply(occ, function(id) {
            p <- Filter(function(q) identical(q$player_id, id),
                        state$lineups[[state$batting_team]])
            if (length(p)) p[[1]]$name else id
          }, character(1))))),
      .incoming_fields(ns, show_position = FALSE))
  }

  modalDialog(
    title = names(.SUB_KINDS)[.SUB_KINDS == kind],
    selectInput(ns("sub_kind"), "Type", .SUB_KINDS, selected = kind),
    tags$hr(), body,
    if (length(errors))
      tags$div(class = "alert alert-danger py-2 small mt-2",
        tags$ul(class = "mb-0", !!!lapply(errors, tags$li))),
    footer = tagList(modalButton("Cancel"),
      actionButton(ns("sub_commit"), "Make substitution", class = "btn-primary")),
    easyClose = FALSE)
}

build_substitution_event <- function(input, state, kind) {
  nm <- trimws(input$sub_name %||% "")
  if (!nzchar(nm)) return(list(errors = "Enter the incoming player's name."))
  incoming <- make_player(uuid::UUIDgenerate(), nm, input$sub_gender %||% "M",
    jersey_number = .parse_jersey(input$sub_jersey),
    order_slot = NA_integer_,
    position = { p <- input$sub_pos %||% ""; if (nzchar(p)) p else NA_character_ })

  if (identical(kind, "batting")) {
    team <- input$sub_team %||% state$batting_team
    slot <- suppressWarnings(as.integer(input$sub_slot %||% NA))
    if (is.na(slot)) return(list(errors = "Choose a batting order slot."))
    return(new_event("substitution", list(team = team, kind = "batting",
      order_slot = slot, in_player = incoming)))
  }

  if (identical(kind, "defensive")) {
    ft <- .fielding_team(state)
    out_id <- input$sub_out %||% ""
    if (!nzchar(out_id)) return(list(errors = "Choose the player coming out."))
    return(new_event("substitution", list(team = ft, kind = "defensive",
      out_player_id = out_id,
      position = { p <- input$sub_pos %||% ""; if (nzchar(p)) p else NA_character_ },
      in_player = incoming)))
  }

  base <- input$sub_base %||% ""
  out_id <- state$bases[[base]] %||% NA_character_
  if (!nzchar(base) || is.na(out_id))
    return(list(errors = "Choose a runner to replace."))
  out_player <- Filter(function(p) identical(p$player_id, out_id),
                       state$lineups[[state$batting_team]])
  out_player <- if (length(out_player)) out_player[[1]] else
    make_player(out_id, out_id, "M", NA_integer_, NA_integer_, NA_character_)

  v <- evaluate_pinch_runner(state$ruleset, state, out_player, incoming)
  # evaluate_pinch_runner returns list(ok =, items = list(list(severity=, code=, message=))).
  # Slice 2.1's final review unified all five rule evaluators on that item shape so their
  # messages can use the code-based toast dedupe from commit 7efcf48. An earlier draft of
  # this plan read `v$errors`, which no longer exists.
  if (!v$ok) return(list(errors = vapply(v$items, function(i) i$message, character(1))))

  new_event("substitution", list(team = state$batting_team, kind = "courtesy_runner",
    out_player_id = out_id, in_player = incoming))
}
```

`.parse_jersey()` is defined in `R/setup_module.R` (slice 2.1 Task 9). `global.R` sources
every file in `R/`, so it is available at runtime; the test file above must source
`setup_module.R` too — add it to the `for (f in c(...))` vector.

- [ ] **Step 5: Wire the button in `tracking_server()`**

```r
    observeEvent(input$sub, {
      showModal(substitution_modal_ui(session$ns, isolate(state()), "batting"))
    }, ignoreInit = TRUE)

    # Switching the type re-renders the modal with the right fields.
    observeEvent(input$sub_kind, {
      showModal(substitution_modal_ui(session$ns, isolate(state()), input$sub_kind))
    }, ignoreInit = TRUE)

    observeEvent(input$sub_commit, {
      s <- isolate(state())
      res <- build_substitution_event(input, s, input$sub_kind %||% "batting")
      if (!is.null(res$errors)) {
        showModal(substitution_modal_ui(session$ns, s, input$sub_kind %||% "batting",
                                        res$errors))
        return(invisible())
      }
      removeModal()
      appended <- storage$append_event(game_id, res)
      events(c(isolate(events()), list(appended)))
      storage$save_snapshot(game_id, isolate(state()))
    }, ignoreInit = TRUE)
```

- [ ] **Step 6: Run the tests and the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_substitution_ui.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_reducer_subs.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0.

- [ ] **Step 7: Manual check**

Run the app. Confirm the Substitution button opens a modal, all three types render their
own fields, a pinch runner under GameOn Summer is rejected when the second one in an inning
is attempted, and a committed batting substitution changes the current batter.

- [ ] **Step 8: Commit**

```bash
git add R/substitution_ui.R R/tracking_module.R R/game_reducer.R tests/test_substitution_ui.R tests/test_reducer_subs.R
git commit -m "feat: working substitution modal with pinch-runner validation"
```

---

### Task 5: Mid-game lineup entry

Slice 2.1 added the `lineup_set` event and its reducer branch, but nothing emits it. This
task gives it a button, which is what makes "enter the lineup during the first inning" real.

**Files:**
- Modify: `R/setup_module.R` (`.player_row` gains an optional `values` argument)
- Create: `R/lineup_modal.R`
- Modify: `R/tracking_module.R` (button plus observers)
- Test: `tests/test_lineup_modal.R` (create)

**Interfaces:**
- Produces:
  - `.player_row(ns, prefix, id, order, show_gender, values = NULL)` — `values` is an
    optional `list(name=, gender=, jersey=, position=)` used to pre-populate the row.
    Backwards compatible: the existing three-argument calls in `setup_server()` are unaffected.
  - `lineup_modal_ui(ns, state, team, show_gender, n_rows)` → a `modalDialog` containing a
    fixed grid of lineup rows, pre-filled from the team's current lineup.
  - `build_lineup_set_event(input, team, row_ids, show_gender)` → a `lineup_set` event.

The modal renders a **fixed** number of rows rather than growing them with `insertUI`.
`insertUI` inside a modal is workable but `renderUI` is not — re-rendering a table of Shiny
inputs resets every value the user has typed. A fixed grid sidesteps the problem entirely
and matches how a paper scorebook works: blank slots you fill in. `collect_lineup()` already
skips rows with no name.

- [ ] **Step 1: Write the failing test**

Create `tests/test_lineup_modal.R`:

```r
library(testthat)
suppressMessages({ library(shiny); library(bslib); library(htmltools) })
for (f in c("app_config.R","rules_engine.R","rule_presets.R","game_events.R",
            "game_reducer.R","setup_module.R","lineup_modal.R"))
  source(file.path("R", f))

st <- function(away = list()) {
  s <- initial_game_state()
  s$lineups$away <- away
  s$teams <- list(away = list(team_id = "A", name = "Otters"),
                  home = list(team_id = "H", name = "Badgers"))
  s
}

test_that("a pre-filled row carries its existing values", {
  html <- as.character(.player_row(NS("track"), "lu_away", 1L, order = 1L,
    show_gender = TRUE,
    values = list(name = "Ann", gender = "F", jersey = 7L, position = "SS")))
  expect_true(grepl('value="Ann"', html, fixed = TRUE))
  expect_true(grepl('value="7"', html, fixed = TRUE))
})

test_that("a row with no values is blank and still has its ids", {
  html <- as.character(.player_row(NS("track"), "lu_away", 2L, order = 2L,
                                   show_gender = TRUE))
  expect_true(grepl('id="track-lu_away_name_2"', html, fixed = TRUE))
  expect_false(grepl('value="Ann"', html, fixed = TRUE))
})

test_that("the modal renders at least twelve rows for an empty lineup", {
  html <- as.character(lineup_modal_ui(NS("track"), st(), "away",
                                       show_gender = TRUE, n_rows = 12L))
  for (i in 1:12)
    expect_true(grepl(paste0("track-lu_away_name_", i), html, fixed = TRUE), info = i)
})

test_that("the modal pre-fills an existing lineup and adds spare rows", {
  lu <- list(make_player("a1", "Ann", "F", 7L, 1L, "SS"),
             make_player("a2", "Bo",  "M", 3L, 2L, "P"))
  html <- as.character(lineup_modal_ui(NS("track"), st(lu), "away",
                                       show_gender = TRUE, n_rows = 5L))
  expect_true(grepl('value="Ann"', html, fixed = TRUE))
  expect_true(grepl('value="Bo"', html, fixed = TRUE))
  expect_true(grepl("track-lu_away_name_5", html, fixed = TRUE))
})

test_that("the modal names the team and omits gender when genderless", {
  html <- as.character(lineup_modal_ui(NS("track"), st(), "away",
                                       show_gender = FALSE, n_rows = 3L))
  expect_true(grepl("Otters", html, fixed = TRUE))
  expect_false(grepl("lu_away_gender_1", html, fixed = TRUE))
})

test_that("build_lineup_set_event produces a valid event that skips blank rows", {
  input <- list(
    lu_away_name_1 = "Ann", lu_away_gender_1 = "F", lu_away_jersey_1 = "7",
    lu_away_pos_1 = "SS",
    lu_away_name_2 = "",    lu_away_gender_2 = "M", lu_away_jersey_2 = "",
    lu_away_pos_2 = "",
    lu_away_name_3 = "Bo",  lu_away_gender_3 = "M", lu_away_jersey_3 = "3",
    lu_away_pos_3 = "P")
  evt <- build_lineup_set_event(input, "away", 1:3, show_gender = TRUE)
  expect_equal(evt$type, "lineup_set")
  expect_equal(evt$payload$team, "away")
  expect_length(evt$payload$lineup, 2L)
  expect_equal(evt$payload$lineup[[1]]$name, "Ann")
  expect_equal(evt$payload$lineup[[2]]$order_slot, 2L)   # renumbered, not slot 3
  expect_true(validate_event(evt)$ok)
})

test_that("an all-blank grid produces a valid empty lineup", {
  evt <- build_lineup_set_event(list(), "home", 1:5, show_gender = TRUE)
  expect_length(evt$payload$lineup, 0L)
  expect_true(validate_event(evt)$ok)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_lineup_modal.R")'`
Expected: FAIL — `.player_row()` has no `values` argument.

- [ ] **Step 3: Add `values` to `.player_row()`**

In `R/setup_module.R`, give the function an optional `values` argument defaulting to `NULL`
and thread it into each input's `value`/`selected`:

```r
.player_row <- function(ns, prefix, id, order, show_gender, values = NULL) {
  v <- values %||% list()
  cell <- function(...) tags$td(class = "bw-cell", ...)
  jersey_val <- if (is.null(v$jersey) || is.na(v$jersey)) "" else as.character(v$jersey)
  pos_val <- if (is.null(v$position) || is.na(v$position)) "" else as.character(v$position)
  tags$tr(id = ns(paste0(prefix, "_row_", id)),
    tags$td(class = "bw-order", order),
    cell(textInput(ns(paste0(prefix, "_name_", id)), NULL,
                   value = v$name %||% "", placeholder = "Name")),
    if (show_gender)
      cell(selectInput(ns(paste0(prefix, "_gender_", id)), NULL,
                       c("M" = "M", "F" = "F"), selected = v$gender %||% "M",
                       width = "5rem")),
    cell(tagAppendAttributes(
      textInput(ns(paste0(prefix, "_jersey_", id)), NULL, value = jersey_val,
                placeholder = "#", width = "5rem"),
      inputmode = "numeric", .cssSelector = "input")),
    cell(selectInput(ns(paste0(prefix, "_pos_", id)), NULL, .POS_CHOICES(),
                     selected = pos_val, width = "7rem")),
    cell(actionButton(ns(paste0(prefix, "_del_", id)), "×",
                      class = "btn-sm btn-outline-danger")))
}
```

The existing calls in `setup_server()` pass no `values`, so they get blank rows exactly as
before.

- [ ] **Step 4: Create `R/lineup_modal.R`**

```r
# Enter or replace a lineup during a game. This is what makes a run-only team become a
# tracked team mid-inning, and what lets a lineup that was not ready at first pitch be
# entered later. The reducer's lineup_set branch re-evaluates the batting-order rule
# retroactively, so a rule broken before the lineup was known surfaces here.

lineup_modal_ui <- function(ns, state, team, show_gender, n_rows = 12L) {
  existing <- state$lineups[[team]] %||% list()
  existing <- existing[order(vapply(existing,
    function(p) p$order_slot %||% NA_integer_, integer(1)), na.last = TRUE)]
  n_rows <- max(as.integer(n_rows), length(existing) + 3L)
  prefix <- paste0("lu_", team)

  rows <- lapply(seq_len(n_rows), function(i) {
    p <- if (i <= length(existing)) existing[[i]] else NULL
    vals <- if (is.null(p)) NULL else list(
      name = p$name, gender = p$gender, jersey = p$jersey_number, position = p$position)
    .player_row(ns, prefix, i, order = i, show_gender = show_gender, values = vals)
  })

  modalDialog(
    title = sprintf("%s lineup", state$teams[[team]]$name %||% team),
    tags$p(class = "text-muted small",
      "Leave a row blank to skip it. Saving replaces this team's lineup; anything already recorded is re-checked against the rules."),
    tags$div(class = "bw-lineup-wrap",
      tags$table(class = "table table-sm bw-lineup",
        .lineup_table_head(show_gender),
        tags$tbody(!!!rows))),
    footer = tagList(modalButton("Cancel"),
      actionButton(ns("lu_commit"), "Save lineup", class = "btn-primary")),
    easyClose = FALSE, size = "l")
}

build_lineup_set_event <- function(input, team, row_ids, show_gender) {
  lineup <- collect_lineup(input, paste0("lu_", team), row_ids,
                           show_gender = show_gender)
  new_event("lineup_set", list(team = team, lineup = lineup))
}
```

`collect_lineup()` already renumbers `order_slot` by position among non-blank rows, so a
gap in the grid does not leave a hole in the batting order.

- [ ] **Step 5: Wire the button in `tracking_server()`**

Add an "Edit lineup" control next to Undo and Substitution in `tracking_ui()`:

```r
        actionButton(ns("edit_lineup"), "Edit lineup", class = "btn-outline-secondary"),
```

and in `tracking_server()`:

```r
    lineup_team <- reactiveVal("away")
    LINEUP_ROWS <- 12L

    observeEvent(input$edit_lineup, {
      s <- isolate(state())
      team <- s$batting_team
      lineup_team(team)
      showModal(lineup_modal_ui(session$ns, s, team,
        show_gender = !ruleset_is_genderless(s$ruleset), n_rows = LINEUP_ROWS))
    }, ignoreInit = TRUE)

    observeEvent(input$lu_commit, {
      s <- isolate(state())
      team <- isolate(lineup_team())
      n <- max(LINEUP_ROWS, length(s$lineups[[team]]) + 3L)
      evt <- build_lineup_set_event(input, team, seq_len(n),
        show_gender = !ruleset_is_genderless(s$ruleset))
      removeModal()
      appended <- storage$append_event(game_id, evt)
      events(c(isolate(events()), list(appended)))
      storage$save_snapshot(game_id, isolate(state()))
    }, ignoreInit = TRUE)
```

The row count in the commit handler must match the one the modal rendered, which is why
both compute `max(LINEUP_ROWS, length(existing) + 3L)`. Reading a row id that was never
rendered is harmless — `collect_lineup()` sees `NULL` and skips it — but reading too *few*
would silently drop players.

The modal always edits the **batting** team. That is the team whose lineup matters right
now; the fielding team's lineup can be edited on its own half.

- [ ] **Step 6: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_lineup_modal.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_setup_module.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0.

- [ ] **Step 7: Manual check**

Start a game with the away lineup empty and the home lineup filled. Confirm the away half
shows the runs-only panel, **Edit lineup** opens a blank grid, saving three players switches
the away half to outcome buttons with the first batter due up, and the scorebook now has
three rows for the away team.

Then, with GameOn Summer selected, record two male batters before entering the lineup and
confirm the retroactive `batting_gender_retro` violation modal appears on save.

- [ ] **Step 8: Commit**

```bash
git add R/lineup_modal.R R/setup_module.R R/tracking_module.R tests/test_lineup_modal.R
git commit -m "feat: enter or replace a lineup mid-game"
```

---

## Definition of done for slice 2.2

- `Rscript run_tests.R` exits 0.
- Tapping an outcome with runners on base always opens a pre-filled disposition panel; with the bases empty it commits in one tap.
- Two runners cannot be placed on the same base, and no runner can move backwards.
- RBI defaults to the number of scored runners and is editable.
- `state$runner_origin`, `origin_index`, `pa_index_in_half`, and `outs_before` are populated, including through a pinch runner.
- The Substitution button opens a working modal for all three kinds, and pinch runners are validated against the ruleset.
- A home run past the league limit is recorded as the configured replacement outcome with a notice.
- A run-only team can be given a lineup mid-game, and a rule broken before the lineup was known surfaces as a retroactive violation on save.
