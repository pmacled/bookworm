# Bookworm Slice 2.1 — Ruleset Model and Setup Flow Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the ruleset expressive enough for real league rules — home-run limits, pinch-runner allowances, mercy schedules, "no three males in a row" — ship named presets, and rebuild the setup screen as rules-first with a real lineup table.

**Architecture:** `R/rules_engine.R` stays the pure rules layer: it never touches Shiny and every function in it is testable with a plain list. New rule evaluators (`evaluate_home_run_limit`, `evaluate_pinch_runner`) follow the shape `evaluate_fielding` already established — they take a config plus state and return warning items, and never mutate. Presets move to their own file so the engine does not grow a data blob. The setup UI keeps its existing `insertUI`/`removeUI` row machinery and its `<prefix>_<field>_<id>` input naming; only the markup and the ordering change.

**Tech Stack:** R 4.5.3, Shiny 1.13.0, bslib 0.10.0, testthat 3.3.2.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-08-08-bookworm-slice-two-design.md`, section "Slice 2.1".
- Run every command from the **project root**.
- Rscript is not on PATH: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe"`.
- Full suite: `Rscript run_tests.R`. It must exit 0 at the end of **every task**.
- Test convention: `library(testthat)`, then `source(file.path("R", "<file>.R"))` per dependency, then `test_that()` blocks. Run from the project root.
- **Do not touch `R/tracking_module.R`, `R/boxscore.R`, `R/outcome_help.R`, `R/app_config.R`, `_brand.yml`, `R/brand_colors.R`, `R/auth_module.R`, `R/supabase_client.R`, `R/session_flow.R`, `R/app_main.R`, or `README.md`.** Slice 2.0 owns those and runs in parallel. This slice delivers *pure engine functions* for the home-run limit and pinch runners; slice 2.2 wires them into play recording.
- `ITPHR` is added to `APP_CONFIG$outcome_meta` by slice 2.0 Task 2. This slice only reads it. If slice 2.0 has not merged yet, write the code against `"ITPHR"` as a literal and let the shared test suite catch the ordering.
- Every rule evaluator is pure: no `state <-` assignment inside, no Shiny.
- Every preset must satisfy `validate_ruleset_config(preset_ruleset(id))$ok == TRUE`.

---

### Task 1: Ruleset schema and backward-compatible migration

The nesting of `run_cap` and `mercy_rule` and the renaming of the batting-gender types are a single atomic change: splitting them would leave the reducer reading keys that no longer exist.

**Files:**
- Modify: `R/rules_engine.R:1-64`
- Modify: `R/game_reducer.R:71-72`, `R/game_reducer.R:130-132` (read the nested run-cap keys)
- Modify: `R/setup_module.R:38-47` (`collect_ruleset` writes the nested keys)
- Test: `tests/test_rules_engine.R` (extend), `tests/test_rules_eval.R:11-15` (rewrite)

**Interfaces:**
- Produces:
  - `default_ruleset_config()` → the nested schema in the spec, section 2.1.1.
  - `coerce_ruleset_config(cfg)` → migrates legacy shapes, then merges over defaults. Idempotent.
  - `validate_ruleset_config(cfg)` → `list(ok = <lgl>, errors = <chr>)`.
  - `apply_run_cap(cfg, runs_before, runs_on_play, inning)` → `list(runs = <int>, cap_hit = <lgl>)`. **Signature change** — the old form took `(cfg, runs_this_half, inning)` and returned an integer.

- [ ] **Step 1: Write the failing migration tests**

Append to `tests/test_rules_engine.R`:

```r
test_that("the new default is Anything Goes: 0-0 count, unlimited fouls, no gender rule", {
  cfg <- default_ruleset_config()
  expect_equal(cfg$starting_count$balls, 0L)
  expect_equal(cfg$starting_count$strikes, 0L)
  expect_equal(cfg$foul_out_rule, "unlimited")
  expect_equal(cfg$batting_gender_rule$type, "none")
  expect_true(is.na(cfg$run_cap$per_inning))
  expect_length(cfg$mercy_rule$tiers, 0L)
  expect_true(is.na(cfg$home_run_rule$over_fence_limit))
  expect_true(is.na(cfg$pinch_runner$max_per_inning))
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("legacy scalar run-cap keys migrate into run_cap", {
  cfg <- coerce_ruleset_config(list(run_cap_per_inning = 5L, open_last_inning = FALSE))
  expect_equal(cfg$run_cap$per_inning, 5L)
  expect_false(cfg$run_cap$open_last_inning)
  expect_true(cfg$run_cap$same_play_runs_count)   # new field takes its default
  expect_null(cfg$run_cap_per_inning)             # old key is gone, not shadowing
})

test_that("legacy scalar mercy keys migrate into a single tier", {
  cfg <- coerce_ruleset_config(list(mercy_rule = list(differential = 10L, after_inning = 4L)))
  expect_length(cfg$mercy_rule$tiers, 1L)
  expect_equal(cfg$mercy_rule$tiers[[1]]$differential, 10L)
  expect_equal(cfg$mercy_rule$tiers[[1]]$after_inning, 4L)
  expect_null(cfg$mercy_rule$differential)
})

test_that("a legacy mercy differential with no after_inning defaults to inning 1", {
  cfg <- coerce_ruleset_config(list(mercy_rule = list(differential = 10L)))
  expect_equal(cfg$mercy_rule$tiers[[1]]$after_inning, 1L)
})

test_that("legacy batting-gender type names migrate", {
  a <- coerce_ruleset_config(list(batting_gender_rule = list(type = "no_two_males_consecutive")))
  expect_equal(a$batting_gender_rule$type, "max_consecutive_males")
  expect_equal(a$batting_gender_rule$n, 1L)

  b <- coerce_ruleset_config(list(batting_gender_rule = list(type = "every_other")))
  expect_equal(b$batting_gender_rule$type, "max_consecutive_same_gender")
  expect_equal(b$batting_gender_rule$n, 1L)

  c3 <- coerce_ruleset_config(list(batting_gender_rule = list(type = "every_n", n = 4L)))
  expect_equal(c3$batting_gender_rule$type, "min_females_per_n")
  expect_equal(c3$batting_gender_rule$n, 4L)
})

test_that("the legacy courtesy_runner boolean migrates", {
  on  <- coerce_ruleset_config(list(courtesy_runner = TRUE))
  expect_true(is.na(on$pinch_runner$max_per_game))     # unlimited
  off <- coerce_ruleset_config(list(courtesy_runner = FALSE))
  expect_equal(off$pinch_runner$max_per_game, 0L)
  expect_null(on$courtesy_runner)
})

test_that("migration is idempotent", {
  once  <- coerce_ruleset_config(list(run_cap_per_inning = 5L,
             mercy_rule = list(differential = 10L, after_inning = 4L),
             batting_gender_rule = list(type = "every_other")))
  twice <- coerce_ruleset_config(once)
  expect_identical(once, twice)
})

test_that("validation rejects the new enums", {
  bad <- default_ruleset_config()
  bad$home_run_rule$over_limit_result <- "explode"
  expect_false(validate_ruleset_config(bad)$ok)

  bad2 <- default_ruleset_config()
  bad2$pinch_runner$eligibility <- "whoever"
  expect_false(validate_ruleset_config(bad2)$ok)

  bad3 <- default_ruleset_config()
  bad3$batting_gender_rule$type <- "max_consecutive_males"   # requires n
  bad3$batting_gender_rule$n <- NA_integer_
  expect_false(validate_ruleset_config(bad3)$ok)

  bad4 <- default_ruleset_config()
  bad4$mercy_rule$tiers <- list(list(after_inning = 3L))     # missing differential
  expect_false(validate_ruleset_config(bad4)$ok)
})
```

Also **rewrite** the existing run-cap test in `tests/test_rules_eval.R` (lines 11–15) — the
old signature is gone:

```r
test_that("run cap limits non-open innings", {
  cfg <- coerce_ruleset_config(list(run_cap_per_inning = 5L, innings = 7L,
                                    open_last_inning = TRUE))
  cfg$run_cap$same_play_runs_count <- FALSE   # legacy clamping behaviour
  expect_equal(apply_run_cap(cfg, runs_before = 0L, runs_on_play = 8L, inning = 3L)$runs, 5L)
  expect_equal(apply_run_cap(cfg, runs_before = 0L, runs_on_play = 8L, inning = 7L)$runs, 8L)
})
```

And update the first assertion in `tests/test_rules_engine.R`'s "default config is valid"
test: `expect_equal(cfg$starting_count$balls, 1L)` becomes `0L`.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_rules_engine.R")'`
Expected: FAIL — `cfg$run_cap` is NULL.

- [ ] **Step 3: Replace `default_ruleset_config()`**

```r
default_ruleset_config <- function() {
  list(
    preset = "anything_goes",
    starting_count = list(balls = 0L, strikes = 0L),
    foul_out_rule = "unlimited",
    batting_gender_rule = list(type = "none", n = NA_integer_),
    male_walk_rule = "none",
    batting_size = NA_integer_,
    fielding = list(fielder_count = NA_integer_, min_females = 0L,
                    max_males = NA_integer_, tiers = list(),
                    position_requirements = list()),
    innings = 7L,
    run_cap = list(per_inning = NA_integer_, open_last_inning = TRUE,
                   same_play_runs_count = TRUE, cap_ends_half = TRUE),
    mercy_rule = list(tiers = list()),
    home_run_rule = list(over_fence_limit = NA_integer_, limit_by_gender = list(),
                         over_limit_result = "out", inside_park_counts = FALSE),
    pinch_runner = list(max_per_inning = NA_integer_, max_per_game = NA_integer_,
                        max_per_player_per_game = NA_integer_,
                        eligibility = "anyone", allowed_for = "anyone"),
    short_lineup_auto_out = FALSE
  )
}
```

- [ ] **Step 4: Add the migration, and run it before the merge**

Insert `.migrate_ruleset_config()` above `coerce_ruleset_config()` and call it first. Running
migration *before* `modifyList()` is what stops old scalar keys sitting alongside the new
nested defaults.

```r
.BATTING_GENDER_ALIASES <- list(
  no_two_males_consecutive = list(type = "max_consecutive_males",       n = 1L),
  every_other              = list(type = "max_consecutive_same_gender", n = 1L),
  every_n                  = list(type = "min_females_per_n",           n = NA_integer_)
)

# Rewrites pre-slice-2 ruleset shapes in place. Idempotent: a config that is already
# in the new shape passes through untouched. Games persisted before slice 2 embed their
# ruleset in the game_start event and re-coerce on every load, so this must never lose data.
.migrate_ruleset_config <- function(cfg) {
  # run_cap: scalar top-level keys -> nested block
  if (!is.null(cfg$run_cap_per_inning) || !is.null(cfg$open_last_inning)) {
    rc <- cfg$run_cap %||% list()
    if (is.null(rc$per_inning) && !is.null(cfg$run_cap_per_inning))
      rc$per_inning <- cfg$run_cap_per_inning
    if (is.null(rc$open_last_inning) && !is.null(cfg$open_last_inning))
      rc$open_last_inning <- cfg$open_last_inning
    cfg$run_cap <- rc
    cfg$run_cap_per_inning <- NULL
    cfg$open_last_inning <- NULL
  }

  # mercy: scalar differential/after_inning -> single-entry tiers list
  m <- cfg$mercy_rule
  if (!is.null(m) && !is.null(m$differential)) {
    d <- m$differential
    if (length(d) == 1 && !is.na(d)) {
      after <- m$after_inning
      after <- if (is.null(after) || length(after) != 1 || is.na(after)) 1L else as.integer(after)
      cfg$mercy_rule <- list(tiers = list(
        list(after_inning = after, differential = as.integer(d))))
    } else {
      cfg$mercy_rule <- list(tiers = m$tiers %||% list())
    }
  }

  # batting gender: renamed types
  bg <- cfg$batting_gender_rule
  if (!is.null(bg) && !is.null(bg$type) && bg$type %in% names(.BATTING_GENDER_ALIASES)) {
    alias <- .BATTING_GENDER_ALIASES[[bg$type]]
    n <- if (is.na(alias$n)) bg$n else alias$n
    cfg$batting_gender_rule <- list(type = alias$type, n = n)
  }

  # courtesy_runner boolean -> pinch_runner block
  if (!is.null(cfg$courtesy_runner)) {
    pr <- cfg$pinch_runner %||% list()
    if (is.null(pr$max_per_game) && identical(cfg$courtesy_runner, FALSE))
      pr$max_per_game <- 0L
    cfg$pinch_runner <- pr
    cfg$courtesy_runner <- NULL
  }
  cfg
}
```

Then in `coerce_ruleset_config()`, replace the body's opening lines and coercion tail:

```r
coerce_ruleset_config <- function(cfg) {
  d <- default_ruleset_config()
  cfg <- .migrate_ruleset_config(cfg %||% list())
  d <- utils::modifyList(d, cfg)

  d$starting_count$balls   <- as.integer(d$starting_count$balls)
  d$starting_count$strikes <- as.integer(d$starting_count$strikes)
  d$innings                <- as.integer(d$innings)
  d$batting_gender_rule$n  <- .as_int_or_na(d$batting_gender_rule$n)

  d$batting_size <- .as_int_or_na(d$batting_size)
  if (!is.na(d$batting_size) && d$batting_size < 1L) d$batting_size <- NA_integer_

  d$fielding$fielder_count <- .as_int_or_na(d$fielding$fielder_count)
  d$fielding$min_females   <- as.integer(d$fielding$min_females)
  d$fielding$max_males     <- .as_int_or_na(d$fielding$max_males)
  d$fielding$tiers         <- d$fielding$tiers %||% list()

  d$run_cap$per_inning           <- .as_int_or_na(d$run_cap$per_inning)
  d$run_cap$open_last_inning     <- isTRUE(d$run_cap$open_last_inning)
  d$run_cap$same_play_runs_count <- isTRUE(d$run_cap$same_play_runs_count)
  d$run_cap$cap_ends_half        <- isTRUE(d$run_cap$cap_ends_half)

  d$mercy_rule$tiers <- lapply(d$mercy_rule$tiers %||% list(), function(t)
    list(after_inning = .as_int_or_na(t$after_inning),
         differential = .as_int_or_na(t$differential)))

  d$home_run_rule$over_fence_limit <- .as_int_or_na(d$home_run_rule$over_fence_limit)
  d$home_run_rule$limit_by_gender <-
    lapply(d$home_run_rule$limit_by_gender %||% list(), .as_int_or_na)
  d$home_run_rule$inside_park_counts <- isTRUE(d$home_run_rule$inside_park_counts)

  for (k in c("max_per_inning", "max_per_game", "max_per_player_per_game"))
    d$pinch_runner[[k]] <- .as_int_or_na(d$pinch_runner[[k]])
  d
}
```

`isTRUE()` on the four logicals is what makes the function idempotent for those fields;
`.as_int_or_na()` already is.

- [ ] **Step 5: Extend `validate_ruleset_config()`**

Keep the existing checks (starting count bounds, `foul_out_rule`, `male_walk_rule`,
`innings`, `batting_size`) and replace the batting-gender block, then append the new ones:

```r
  bg <- cfg$batting_gender_rule$type
  valid_bg <- c("none", "max_consecutive_males", "max_consecutive_same_gender",
                "min_females_per_n")
  if (!bg %in% valid_bg) add("invalid batting_gender_rule type")
  if (!identical(bg, "none") && is.na(cfg$batting_gender_rule$n))
    add(sprintf("%s batting rule requires n", bg))
  if (!identical(bg, "none") && !is.na(cfg$batting_gender_rule$n) &&
      cfg$batting_gender_rule$n < 1L)
    add("batting_gender_rule n must be >= 1")

  for (t in cfg$mercy_rule$tiers) {
    if (is.na(t$after_inning) || is.na(t$differential))
      add("each mercy tier needs after_inning and differential")
    else if (t$after_inning < 1L || t$differential < 1L)
      add("mercy tier values must be >= 1")
  }

  if (!cfg$home_run_rule$over_limit_result %in%
      c("out", "ground_rule_double", "single")) add("invalid over_limit_result")
  if (!cfg$pinch_runner$eligibility %in%
      c("anyone", "same_gender", "last_out", "last_same_gender_out"))
    add("invalid pinch_runner eligibility")
  if (!cfg$pinch_runner$allowed_for %in% c("anyone", "pitcher_catcher"))
    add("invalid pinch_runner allowed_for")
```

- [ ] **Step 6: Update the four run-cap call sites**

Rewrite `apply_run_cap()` in `R/rules_engine.R`:

```r
# Returns how many of `runs_on_play` actually count, and whether the cap was reached.
# same_play_runs_count = TRUE: a play in progress completes fully; the cap stops the
# *next* batter. FALSE: runs are clamped mid-play at the cap.
apply_run_cap <- function(cfg, runs_before, runs_on_play, inning) {
  rc <- cfg$run_cap
  cap <- rc$per_inning
  runs_on_play <- as.integer(runs_on_play)
  if (is.na(cap)) return(list(runs = runs_on_play, cap_hit = FALSE))
  if (isTRUE(rc$open_last_inning) && inning >= cfg$innings)
    return(list(runs = runs_on_play, cap_hit = FALSE))
  total <- as.integer(runs_before) + runs_on_play
  runs <- if (isTRUE(rc$same_play_runs_count)) runs_on_play
          else max(0L, cap - as.integer(runs_before))
  list(runs = as.integer(runs), cap_hit = total >= cap)
}
```

In `R/game_reducer.R`, `.refresh_flags()` currently recomputes the run-cap notice from
`state$runs_this_half`, which `advance_half()` has already reset. Replace that whole block
(lines 71–76) with a read of a transient flag:

```r
  if (isTRUE(state$cap_hit_last_play)) {
    w <- c(w, list(list(severity = "notice", code = "run_cap",
      message = sprintf("Run cap of %d reached this inning.", cfg$run_cap$per_inning))))
  }
```

Add `cap_hit_last_play = FALSE` to `initial_game_state()`'s list, and clear it at the top of
`apply_event()` for every branch except `game_start` (which rebuilds state):

```r
apply_event <- function(state, evt) {
  type <- evt$type
  if (type != "game_start") state$cap_hit_last_play <- FALSE
  ...
```

Then in `apply_plate_appearance()`, replace the capping lines:

```r
  cr <- apply_run_cap(state$ruleset, state$runs_this_half, runs, state$inning)
  runs <- cr$runs
  state$cap_hit_last_play <- cr$cap_hit
  state$score[[team]] <- state$score[[team]] + runs
  state$runs_this_half <- state$runs_this_half + runs
```

and replace the half-ending line at the end of the function:

```r
  cap_ends <- isTRUE(cr$cap_hit) && isTRUE(state$ruleset$run_cap$cap_ends_half)
  if (state$outs >= 3L || cap_ends) state <- advance_half(state)
  else state <- .set_current_batter(state)
```

`advance_half()` must not clear `cap_hit_last_play` — check that it does not, and leave the
flag alone there.

Finally, the `half_runs` branch of `apply_event()` currently carries a hand-rolled
duplicate of the cap-notice logic (lines 126–139). Delete that comment and the
re-append block, and replace the branch body's capping with the same pattern:

```r
  if (type == "half_runs") {
    team <- state$batting_team
    runs <- as.integer(evt$payload$runs %||% 0L)
    cr <- apply_run_cap(state$ruleset, state$runs_this_half, runs, state$inning)
    state$score[[team]] <- state$score[[team]] + cr$runs
    state$runs_this_half <- state$runs_this_half + cr$runs
    state <- advance_half(state)
    state$cap_hit_last_play <- cr$cap_hit   # set AFTER advance_half so the notice survives
    return(.refresh_flags(state))
  }
```

While you are in `.refresh_flags()`, add the in-game fielder-count notice required by spec
section 2.1.3. Put it directly after the `batting_size` notice block:

```r
  fc <- cfg$fielding$fielder_count
  if (!is.na(fc)) {
    n_field <- length(Filter(function(p) !is.na(.position_category(p$position)),
                             state$lineups[[def_team]]))
    if (n_field > 0L && n_field != fc)
      w <- c(w, list(list(severity = "notice", code = "fielder_count",
        message = sprintf("%d fielders have positions; rule expects %d.", n_field, fc))))
  }
```

`def_team` is already computed at the top of `.refresh_flags()`. The `n_field > 0L` guard
matters: a team whose lineup has no positions assigned yet must not be nagged every pitch.
Add a test for it in `tests/test_reducer_warnings.R`:

```r
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
```

In `R/setup_module.R`'s `collect_ruleset()`, change the run-cap and mercy lines so it writes
the nested shape (Task 10 rewrites this function entirely; this keeps it working meanwhile):

```r
    run_cap = list(per_inning = if ((input$run_cap %||% 0) > 0) input$run_cap else NA_integer_),
    mercy_rule = list(tiers = if ((input$mercy_diff %||% 0) > 0)
      list(list(after_inning = 1L, differential = input$mercy_diff)) else list())
```

- [ ] **Step 7: Run the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: exit 0. Tests that construct a `default_ruleset_config()` and assume a 1‑1 count
will fail — update the assertions rather than restoring the old default. Check
`tests/test_reducer_core.R` and `tests/test_reducer_pa.R` for `count$balls` assertions.

- [ ] **Step 8: Commit**

```bash
git add R/rules_engine.R R/game_reducer.R R/setup_module.R tests/test_rules_engine.R tests/test_rules_eval.R tests/test_reducer_warnings.R
git commit -m "feat: nested ruleset schema with backward-compatible migration"
```

---

### Task 2: Batting gender rule types

**Files:**
- Modify: `R/rules_engine.R` (`next_batter_gender_ok`)
- Test: `tests/test_rules_eval.R` (extend)

**Interfaces:**
- Produces: `next_batter_gender_ok(cfg, prev_genders, next_gender)` → `<lgl>`. Signature unchanged; the four new `type` values are handled.

- [ ] **Step 1: Write the failing test**

Append to `tests/test_rules_eval.R`:

```r
gender_cfg <- function(type, n) coerce_ruleset_config(
  list(batting_gender_rule = list(type = type, n = n)))

test_that("max_consecutive_males n=1 is the old no-two-males rule", {
  cfg <- gender_cfg("max_consecutive_males", 1L)
  expect_false(next_batter_gender_ok(cfg, c("M"), "M"))
  expect_true(next_batter_gender_ok(cfg, c("M"), "F"))
  expect_true(next_batter_gender_ok(cfg, c("F"), "M"))
  expect_true(next_batter_gender_ok(cfg, character(), "M"))   # nothing to violate yet
})

test_that("max_consecutive_males n=2 allows two males but not three", {
  cfg <- gender_cfg("max_consecutive_males", 2L)
  expect_true(next_batter_gender_ok(cfg, c("M"), "M"))
  expect_false(next_batter_gender_ok(cfg, c("M", "M"), "M"))
  expect_true(next_batter_gender_ok(cfg, c("F", "M"), "M"))
  expect_true(next_batter_gender_ok(cfg, c("M", "M"), "F"))
})

test_that("max_consecutive_same_gender applies to both genders", {
  cfg <- gender_cfg("max_consecutive_same_gender", 1L)
  expect_false(next_batter_gender_ok(cfg, c("F"), "F"))
  expect_false(next_batter_gender_ok(cfg, c("M"), "M"))
  expect_true(next_batter_gender_ok(cfg, c("M"), "F"))
})

test_that("min_females_per_n needs one female per window", {
  cfg <- gender_cfg("min_females_per_n", 3L)
  expect_false(next_batter_gender_ok(cfg, c("M", "M"), "M"))
  expect_true(next_batter_gender_ok(cfg, c("M", "F"), "M"))
  # A window that is not yet full cannot be violated.
  expect_true(next_batter_gender_ok(cfg, c("M"), "M"))
})

test_that("type none never fails", {
  cfg <- gender_cfg("none", NA_integer_)
  expect_true(next_batter_gender_ok(cfg, c("M", "M", "M"), "M"))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_rules_eval.R")'`
Expected: FAIL — `max_consecutive_males` falls through to the final `TRUE`.

- [ ] **Step 3: Replace `next_batter_gender_ok()`**

```r
next_batter_gender_ok <- function(cfg, prev_genders, next_gender) {
  rule <- cfg$batting_gender_rule
  type <- rule$type
  if (is.null(type) || identical(type, "none")) return(TRUE)
  n <- rule$n
  if (is.null(n) || length(n) != 1 || is.na(n)) return(TRUE)
  n <- as.integer(n)
  seq_all <- c(prev_genders, next_gender)

  if (identical(type, "max_consecutive_males")) {
    window <- utils::tail(seq_all, n + 1L)
    return(!(length(window) == n + 1L && all(window == "M")))
  }
  if (identical(type, "max_consecutive_same_gender")) {
    window <- utils::tail(seq_all, n + 1L)
    return(!(length(window) == n + 1L && length(unique(window)) == 1L))
  }
  if (identical(type, "min_females_per_n")) {
    window <- utils::tail(seq_all, n)
    if (length(window) < n) return(TRUE)   # window not full yet
    return(any(window == "F"))
  }
  TRUE
}
```

- [ ] **Step 4: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_rules_eval.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0. `tests/test_reducer_warnings.R:13` builds a config with the legacy
`no_two_males_consecutive` name; Task 1's migration means it still works.

- [ ] **Step 5: Commit**

```bash
git add R/rules_engine.R tests/test_rules_eval.R
git commit -m "feat: generalized batting gender rules (max consecutive, min females per window)"
```

---

### Task 3: Run-cap semantics

**Files:**
- Test: `tests/test_reducer_runcap.R` (create)

`apply_run_cap()` and the reducer wiring were written in Task 1. This task proves the
behaviour end-to-end through `fold_events()`, including the grand-slam case that motivated
the change.

**Interfaces:** consumes `apply_run_cap`, `fold_events` from Task 1.

- [ ] **Step 1: Write the test**

Create `tests/test_reducer_runcap.R`:

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

mk <- function(prefix, n = 4L) lapply(seq_len(n), function(i)
  make_player(paste0(prefix, i), paste(prefix, i), c("M","F")[(i %% 2) + 1L], i, i, i))

start_with <- function(cfg) new_event("game_start", list(
  ruleset = cfg, first_bat = "away",
  home = list(team_id="H", name="Home", lineup = mk("h")),
  away = list(team_id="A", name="Away", lineup = mk("a"))), seq = 1L)

adv <- function(id, from, to, scored = FALSE) make_advance(id, from, to, scored = scored)

pa <- function(batter, outcome, reached, advances = list(), outs = 0L, seq = 2L)
  new_event("plate_appearance", list(team = "away", batter_id = batter, outcome = outcome,
    reached = reached, rbi = 0L, outs_on_play = outs, advances = advances), seq = seq)

cap_cfg <- function(...) coerce_ruleset_config(utils::modifyList(
  list(innings = 7L, run_cap = list(per_inning = 5L, open_last_inning = TRUE,
                                    same_play_runs_count = TRUE, cap_ends_half = TRUE)),
  list(...)))

test_that("apply_run_cap with same_play_runs_count lets a grand slam finish", {
  cfg <- cap_cfg()
  r <- apply_run_cap(cfg, runs_before = 4L, runs_on_play = 4L, inning = 1L)
  expect_equal(r$runs, 4L)      # all four count: 4 + 4 = 8
  expect_true(r$cap_hit)
})

test_that("apply_run_cap without same_play_runs_count truncates at the cap", {
  cfg <- cap_cfg(run_cap = list(per_inning = 5L, open_last_inning = TRUE,
                                same_play_runs_count = FALSE, cap_ends_half = TRUE))
  r <- apply_run_cap(cfg, runs_before = 4L, runs_on_play = 4L, inning = 1L)
  expect_equal(r$runs, 1L)      # clamped to reach exactly 5
  expect_true(r$cap_hit)
})

test_that("the cap does not apply in an open last inning", {
  cfg <- cap_cfg()
  r <- apply_run_cap(cfg, runs_before = 4L, runs_on_play = 4L, inning = 7L)
  expect_equal(r$runs, 4L)
  expect_false(r$cap_hit)
})

test_that("reaching the cap ends the half-inning", {
  cfg <- cap_cfg(run_cap = list(per_inning = 2L, open_last_inning = FALSE,
                                same_play_runs_count = TRUE, cap_ends_half = TRUE))
  # a1 homers with nobody on (1 run), then a2 homers (1 run) -> cap of 2 reached
  s <- fold_events(list(
    start_with(cfg),
    pa("a1", "HR", 4L, list(adv("a1", 0L, 4L, scored = TRUE)), seq = 2L),
    pa("a2", "HR", 4L, list(adv("a2", 0L, 4L, scored = TRUE)), seq = 3L)))
  expect_equal(s$score$away, 2L)
  expect_equal(s$half, "bottom")        # half ended on the cap, not on three outs
  expect_equal(s$outs, 0L)
  expect_equal(s$batting_team, "home")
})

test_that("cap_ends_half = FALSE keeps the half alive", {
  cfg <- cap_cfg(run_cap = list(per_inning = 2L, open_last_inning = FALSE,
                                same_play_runs_count = FALSE, cap_ends_half = FALSE))
  s <- fold_events(list(
    start_with(cfg),
    pa("a1", "HR", 4L, list(adv("a1", 0L, 4L, scored = TRUE)), seq = 2L),
    pa("a2", "HR", 4L, list(adv("a2", 0L, 4L, scored = TRUE)), seq = 3L),
    pa("a3", "HR", 4L, list(adv("a3", 0L, 4L, scored = TRUE)), seq = 4L)))
  expect_equal(s$score$away, 2L)        # third run discarded
  expect_equal(s$half, "top")           # still batting
})

test_that("the run-cap notice survives the half ending", {
  cfg <- cap_cfg(run_cap = list(per_inning = 1L, open_last_inning = FALSE,
                                same_play_runs_count = TRUE, cap_ends_half = TRUE))
  s <- fold_events(list(
    start_with(cfg),
    pa("a1", "HR", 4L, list(adv("a1", 0L, 4L, scored = TRUE)), seq = 2L)))
  codes <- vapply(s$warnings, function(w) w$code, character(1))
  expect_true("run_cap" %in% codes)
})

test_that("a run-only half surfaces the cap notice too", {
  cfg <- cap_cfg(run_cap = list(per_inning = 3L, open_last_inning = FALSE,
                                same_play_runs_count = TRUE, cap_ends_half = TRUE))
  start <- new_event("game_start", list(ruleset = cfg, first_bat = "away",
    home = list(team_id="H", name="Home", lineup = mk("h")),
    away = list(team_id="A", name="Away", lineup = list())), seq = 1L)
  s <- fold_events(list(start, new_event("half_runs", list(team = "away", runs = 5L), seq = 2L)))
  codes <- vapply(s$warnings, function(w) w$code, character(1))
  expect_true("run_cap" %in% codes)
  expect_equal(s$score$away, 5L)   # same_play_runs_count applies to the whole half entry
})
```

- [ ] **Step 2: Run it**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_reducer_runcap.R")'`
Expected: PASS. If the last two fail, `cap_hit_last_play` is being cleared by
`advance_half()` or by `.refresh_flags()` — check the ordering from Task 1 Step 6.

- [ ] **Step 3: Run the full suite and commit**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`

```bash
git add tests/test_reducer_runcap.R
git commit -m "test: run-cap semantics including the grand-slam-at-the-cap case"
```

---

### Task 4: Mercy schedule

**Files:**
- Modify: `R/rules_engine.R` (`game_should_end`)
- Test: `tests/test_rules_eval.R` (extend)

**Interfaces:**
- Produces: `game_should_end(cfg, state)` → `<lgl>`. Signature unchanged; reads `cfg$mercy_rule$tiers`.

- [ ] **Step 1: Write the failing test**

Append to `tests/test_rules_eval.R`:

```r
usa_mercy <- function() coerce_ruleset_config(list(innings = 7L, mercy_rule = list(tiers = list(
  list(after_inning = 3L, differential = 20L),
  list(after_inning = 4L, differential = 15L),
  list(after_inning = 5L, differential = 10L)))))

st_at <- function(cfg, inning, home, away)
  list(inning = inning, half = "top", score = list(home = home, away = away),
       ruleset = cfg, outs = 0L)

test_that("any satisfied mercy tier ends the game", {
  cfg <- usa_mercy()
  expect_true(game_should_end(cfg,  st_at(cfg, 3L, 25L, 3L)))   # 22 after 3
  expect_false(game_should_end(cfg, st_at(cfg, 3L, 15L, 3L)))   # 12 after 3: not yet
  expect_true(game_should_end(cfg,  st_at(cfg, 4L, 19L, 3L)))   # 16 after 4
  expect_true(game_should_end(cfg,  st_at(cfg, 5L, 14L, 3L)))   # 11 after 5
  expect_false(game_should_end(cfg, st_at(cfg, 5L, 12L, 3L)))   # 9 after 5: not yet
})

test_that("mercy works in either direction", {
  cfg <- usa_mercy()
  expect_true(game_should_end(cfg, st_at(cfg, 5L, 3L, 14L)))
})

test_that("no mercy tiers means only regulation ends the game", {
  cfg <- coerce_ruleset_config(list(innings = 7L))
  expect_false(game_should_end(cfg, st_at(cfg, 5L, 40L, 0L)))
  expect_true(game_should_end(cfg,  st_at(cfg, 8L, 1L, 0L)))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_rules_eval.R")'`
Expected: FAIL — `game_should_end` reads `m$differential`, which is now NULL, so mercy never fires.

- [ ] **Step 3: Replace `game_should_end()`**

```r
game_should_end <- function(cfg, state) {
  tiers <- cfg$mercy_rule$tiers %||% list()
  if (length(tiers)) {
    diff <- abs(state$score$home - state$score$away)
    for (t in tiers) {
      after <- t$after_inning; d <- t$differential
      if (!is.na(after) && !is.na(d) && state$inning >= after && diff >= d) return(TRUE)
    }
  }
  # Regulation complete: finished the bottom of the final inning.
  if (state$inning > cfg$innings) return(TRUE)
  FALSE
}
```

- [ ] **Step 4: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: exit 0. The two legacy mercy tests in `tests/test_rules_eval.R` (lines 17–22 and
the "no after_inning" one) pass through Task 1's migration.

- [ ] **Step 5: Commit**

```bash
git add R/rules_engine.R tests/test_rules_eval.R
git commit -m "feat: mercy rule as a schedule of tiers"
```

---

### Task 5: Home-run limit evaluator

Pure function only. Slice 2.2 calls it from `record_outcome_event`.

**Files:**
- Create: `R/rule_home_run.R`
- Test: `tests/test_home_run_limit.R` (create)

**Interfaces:**
- Produces: `evaluate_home_run_limit(cfg, state, batter, outcome)` →
  `list(outcome = <chr>, warning = NULL | list(severity=, code=, message=))`.
  `outcome` is the code that should actually be recorded — unchanged unless the limit is
  exceeded. Never mutates `state`.
- Also produces: `count_over_fence_home_runs(cfg, state, team)` → `<int>`.

- [ ] **Step 1: Write the failing test**

Create `tests/test_home_run_limit.R`:

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","rule_home_run.R"))
  source(file.path("R", f))

hr_cfg <- function(...) coerce_ruleset_config(list(home_run_rule = list(...)))

st_with <- function(outcomes, team = "away") {
  st <- list(pa_log = lapply(outcomes, function(o)
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

test_that("a per-gender limit overrides the overall limit", {
  cfg <- hr_cfg(over_fence_limit = 5L, limit_by_gender = list(M = 1L))
  expect_equal(evaluate_home_run_limit(cfg, st_with("HR"), bat("M"), "HR")$outcome, "GO")
  expect_equal(evaluate_home_run_limit(cfg, st_with("HR"), bat("F"), "HR")$outcome, "HR")
})

test_that("only the batting team's home runs are counted", {
  cfg <- hr_cfg(over_fence_limit = 1L)
  st <- list(pa_log = list(list(team = "home", outcome = "HR", batter_id = "h1")))
  r <- evaluate_home_run_limit(cfg, st, bat(), "HR")
  expect_equal(r$outcome, "HR")
})

test_that("non-home-run outcomes pass straight through", {
  cfg <- hr_cfg(over_fence_limit = 0L)
  expect_equal(evaluate_home_run_limit(cfg, st_with(character()), bat(), "1B")$outcome, "1B")
})
```

Note: `st_with()` builds `pa_log` entries whose `team` is `"away"`, and
`evaluate_home_run_limit` must infer the batting team from `state$batting_team`. Add
`batting_team = "away"` to the fixture — amend `st_with` to
`st <- list(batting_team = team, pa_log = ...)`.

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_home_run_limit.R")'`
Expected: FAIL — cannot open `R/rule_home_run.R`.

- [ ] **Step 3: Create `R/rule_home_run.R`**

```r
# Over-the-fence home-run limits. Pure: never mutates state, never touches Shiny.
# Called by the tracking module before an outcome is committed to an event.

.OVER_FENCE <- "HR"
.INSIDE_PARK <- "ITPHR"
.OVER_LIMIT_OUTCOME <- c(out = "GO", ground_rule_double = "2B", single = "1B")

# Home runs already hit by `team` that count toward the limit.
count_over_fence_home_runs <- function(cfg, state, team) {
  counted <- if (isTRUE(cfg$home_run_rule$inside_park_counts))
    c(.OVER_FENCE, .INSIDE_PARK) else .OVER_FENCE
  sum(vapply(state$pa_log %||% list(),
    function(r) identical(r$team, team) && (r$outcome %in% counted), logical(1)))
}

# The limit in force for this batter: a per-gender override wins over the overall limit.
.effective_hr_limit <- function(cfg, batter) {
  by_gender <- cfg$home_run_rule$limit_by_gender %||% list()
  g <- batter$gender %||% NA_character_
  if (!is.na(g) && !is.null(by_gender[[g]])) return(by_gender[[g]])
  cfg$home_run_rule$over_fence_limit
}

evaluate_home_run_limit <- function(cfg, state, batter, outcome) {
  pass <- list(outcome = outcome, warning = NULL)
  # An inside-the-park home run is never rewritten; inside_park_counts only affects
  # whether previous ones count toward the total.
  if (!identical(outcome, .OVER_FENCE)) return(pass)

  limit <- .effective_hr_limit(cfg, batter)
  if (is.null(limit) || length(limit) != 1 || is.na(limit)) return(pass)

  team <- state$batting_team
  already <- count_over_fence_home_runs(cfg, state, team)
  if (already < limit) return(pass)

  replacement <- unname(.OVER_LIMIT_OUTCOME[[cfg$home_run_rule$over_limit_result]])
  list(outcome = replacement,
       warning = list(severity = "notice", code = "home_run_limit",
         message = sprintf(
           "Home-run limit of %d reached; recorded as %s (%s).",
           limit, replacement, APP_CONFIG$outcome_meta[[replacement]]$label)))
}
```

- [ ] **Step 4: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_home_run_limit.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0.

If `APP_CONFIG$outcome_meta` does not exist yet (slice 2.0 not merged), the message
construction will error. Guard it: `lbl <- APP_CONFIG$outcome_meta[[replacement]]$label %||% replacement`.

- [ ] **Step 5: Commit**

```bash
git add R/rule_home_run.R tests/test_home_run_limit.R
git commit -m "feat: over-the-fence home-run limit evaluator with per-gender overrides"
```

---

### Task 6: Pinch-runner evaluator and log

**Files:**
- Create: `R/rule_pinch_runner.R`
- Modify: `R/game_reducer.R` (`initial_game_state`, `apply_substitution`)
- Test: `tests/test_pinch_runner.R` (create)

**Interfaces:**
- Produces:
  - `evaluate_pinch_runner(cfg, state, out_player, in_player)` → `list(ok = <lgl>, errors = <chr>)`.
  - `state$pinch_runner_log`: a list of `list(inning=, half=, team=, out_player_id=, in_player_id=)`, appended by the `courtesy_runner` branch of `apply_substitution()`.
- Consumes: `state$pa_log` (for `last_out` eligibility), `state$lineups`.

- [ ] **Step 1: Write the failing test**

Create `tests/test_pinch_runner.R`:

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R",
            "rule_pinch_runner.R"))
  source(file.path("R", f))

pr_cfg <- function(...) coerce_ruleset_config(list(pinch_runner = list(...)))

p <- function(id, gender = "M", pos = "SS") make_player(id, id, gender, 1L, 1L, pos)

base_state <- function(log = list(), pa_log = list())
  list(inning = 3L, half = "top", batting_team = "away",
       lineups = list(away = list(), home = list()),
       pinch_runner_log = log, pa_log = pa_log)

test_that("unlimited defaults allow anything", {
  r <- evaluate_pinch_runner(pr_cfg(), base_state(), p("r1"), p("r2", "F"))
  expect_true(r$ok)
})

test_that("max_per_inning counts only this inning and half", {
  cfg <- pr_cfg(max_per_inning = 1L)
  used_this <- list(list(inning = 3L, half = "top", team = "away",
                         out_player_id = "x", in_player_id = "y"))
  expect_false(evaluate_pinch_runner(cfg, base_state(used_this), p("r1"), p("r2"))$ok)

  used_other <- list(list(inning = 2L, half = "top", team = "away",
                          out_player_id = "x", in_player_id = "y"))
  expect_true(evaluate_pinch_runner(cfg, base_state(used_other), p("r1"), p("r2"))$ok)
})

test_that("max_per_game counts every inning for this team", {
  cfg <- pr_cfg(max_per_game = 2L)
  used <- lapply(1:2, function(i) list(inning = i, half = "top", team = "away",
                                       out_player_id = "x", in_player_id = "y"))
  expect_false(evaluate_pinch_runner(cfg, base_state(used), p("r1"), p("r2"))$ok)
})

test_that("max_per_game = 0 forbids pinch runners entirely", {
  expect_false(evaluate_pinch_runner(pr_cfg(max_per_game = 0L), base_state(),
                                     p("r1"), p("r2"))$ok)
})

test_that("max_per_player_per_game counts appearances by the incoming runner", {
  cfg <- pr_cfg(max_per_player_per_game = 1L)
  used <- list(list(inning = 1L, half = "top", team = "away",
                    out_player_id = "x", in_player_id = "r2"))
  expect_false(evaluate_pinch_runner(cfg, base_state(used), p("r1"), p("r2"))$ok)
  expect_true(evaluate_pinch_runner(cfg, base_state(used), p("r1"), p("r3"))$ok)
})

test_that("same_gender eligibility", {
  cfg <- pr_cfg(eligibility = "same_gender")
  expect_true(evaluate_pinch_runner(cfg, base_state(), p("r1", "F"), p("r2", "F"))$ok)
  expect_false(evaluate_pinch_runner(cfg, base_state(), p("r1", "F"), p("r2", "M"))$ok)
})

test_that("last_out eligibility requires the most recent out", {
  cfg <- pr_cfg(eligibility = "last_out")
  pal <- list(
    list(team = "away", batter_id = "o1", outcome = "K",  outs_on_play = 1L),
    list(team = "away", batter_id = "o2", outcome = "GO", outs_on_play = 1L),
    list(team = "away", batter_id = "o3", outcome = "1B", outs_on_play = 0L))
  st <- base_state(pa_log = pal)
  expect_true(evaluate_pinch_runner(cfg, st, p("r1"), p("o2"))$ok)
  expect_false(evaluate_pinch_runner(cfg, st, p("r1"), p("o1"))$ok)
})

test_that("last_same_gender_out looks past outs by the other gender", {
  cfg <- pr_cfg(eligibility = "last_same_gender_out")
  st <- base_state(pa_log = list(
    list(team = "away", batter_id = "f1", outcome = "K",  outs_on_play = 1L),
    list(team = "away", batter_id = "m1", outcome = "GO", outs_on_play = 1L)))
  st$lineups$away <- list(p("f1", "F"), p("m1", "M"))
  # Running for a female: the last female out is f1, not the more recent male out m1.
  expect_true(evaluate_pinch_runner(cfg, st, p("f2", "F"), p("f1", "F"))$ok)
  expect_false(evaluate_pinch_runner(cfg, st, p("f2", "F"), p("m1", "M"))$ok)
})

test_that("allowed_for = pitcher_catcher restricts who may be run for", {
  cfg <- pr_cfg(allowed_for = "pitcher_catcher")
  expect_true(evaluate_pinch_runner(cfg, base_state(),  p("r1", "M", "P"), p("r2"))$ok)
  expect_true(evaluate_pinch_runner(cfg, base_state(),  p("r1", "M", "C"), p("r2"))$ok)
  expect_false(evaluate_pinch_runner(cfg, base_state(), p("r1", "M", "SS"), p("r2"))$ok)
})

test_that("multiple failures are all reported", {
  cfg <- pr_cfg(max_per_game = 0L, eligibility = "same_gender")
  r <- evaluate_pinch_runner(cfg, base_state(), p("r1", "F"), p("r2", "M"))
  expect_false(r$ok)
  expect_gte(length(r$errors), 2L)
})

test_that("the reducer records a pinch runner in pinch_runner_log", {
  st <- initial_game_state()
  st$bases$first <- "a1"
  st$batting_team <- "away"
  evt <- new_event("substitution", list(team = "away", kind = "courtesy_runner",
    out_player_id = "a1", in_player = p("a9")))
  st2 <- apply_substitution(st, evt)
  expect_equal(st2$bases$first, "a9")
  expect_length(st2$pinch_runner_log, 1L)
  expect_equal(st2$pinch_runner_log[[1]]$out_player_id, "a1")
  expect_equal(st2$pinch_runner_log[[1]]$in_player_id, "a9")
  expect_equal(st2$pinch_runner_log[[1]]$inning, st$inning)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_pinch_runner.R")'`
Expected: FAIL — cannot open `R/rule_pinch_runner.R`.

- [ ] **Step 3: Create `R/rule_pinch_runner.R`**

```r
# Pinch- / courtesy-runner allowances. Pure: returns errors, never mutates state.
# The two concepts are unified — softball's "courtesy runner" and baseball's
# "pinch runner" are the same substitution as far as the scorebook is concerned.

# Batter ids of outs recorded by `team`, most recent first.
.recent_outs <- function(state, team) {
  outs <- Filter(function(r)
    identical(r$team, team) && as.integer(r$outs_on_play %||% 0L) > 0L,
    state$pa_log %||% list())
  rev(vapply(outs, function(r) r$batter_id %||% NA_character_, character(1)))
}

.gender_of <- function(state, team, player_id) {
  hit <- Filter(function(p) identical(p$player_id, player_id), state$lineups[[team]] %||% list())
  if (length(hit)) hit[[1]]$gender else NA_character_
}

evaluate_pinch_runner <- function(cfg, state, out_player, in_player) {
  pr <- cfg$pinch_runner
  team <- state$batting_team
  errors <- character()
  add <- function(m) errors <<- c(errors, m)

  log <- state$pinch_runner_log %||% list()
  for_team <- Filter(function(e) identical(e$team, team), log)

  n_inning <- sum(vapply(for_team, function(e)
    identical(e$inning, state$inning) && identical(e$half, state$half), logical(1)))
  if (!is.na(pr$max_per_inning) && n_inning >= pr$max_per_inning)
    add(sprintf("Only %d pinch runner(s) allowed per inning; %d already used.",
                pr$max_per_inning, n_inning))

  if (!is.na(pr$max_per_game) && length(for_team) >= pr$max_per_game)
    add(if (pr$max_per_game == 0L) "Pinch runners are not allowed under this ruleset."
        else sprintf("Only %d pinch runner(s) allowed per game; %d already used.",
                     pr$max_per_game, length(for_team)))

  n_player <- sum(vapply(for_team,
    function(e) identical(e$in_player_id, in_player$player_id), logical(1)))
  if (!is.na(pr$max_per_player_per_game) && n_player >= pr$max_per_player_per_game)
    add(sprintf("%s has already pinch run %d time(s) this game.", in_player$name, n_player))

  if (identical(pr$allowed_for, "pitcher_catcher") &&
      !isTRUE(as.character(out_player$position) %in% c("P", "C")))
    add("Only the pitcher or catcher may have a courtesy runner under this ruleset.")

  elig <- pr$eligibility
  if (identical(elig, "same_gender") && !identical(in_player$gender, out_player$gender))
    add(sprintf("The runner must be the same gender as %s.", out_player$name))

  if (elig %in% c("last_out", "last_same_gender_out")) {
    outs <- .recent_outs(state, team)
    if (identical(elig, "last_same_gender_out")) {
      want <- out_player$gender
      outs <- Filter(function(id) identical(.gender_of(state, team, id), want), outs)
    }
    expected <- if (length(outs)) outs[[1]] else NA_character_
    if (is.na(expected))
      add("No eligible previous out to run for yet.")
    else if (!identical(in_player$player_id, expected))
      add(sprintf("The runner must be the last %sout (%s).",
                  if (identical(elig, "last_same_gender_out")) "same-gender " else "",
                  expected))
  }

  list(ok = length(errors) == 0, errors = errors)
}
```

- [ ] **Step 4: Record pinch runners in the reducer**

Add `pinch_runner_log = list()` to `initial_game_state()`'s list.

In `apply_substitution()`, extend the `courtesy_runner` branch:

```r
  } else if (p$kind == "courtesy_runner") {
    for (b in c("first","second","third"))
      if (!is.na(state$bases[[b]]) && state$bases[[b]] == p$out_player_id)
        state$bases[[b]] <- p$in_player$player_id
    state$pinch_runner_log <- c(state$pinch_runner_log %||% list(), list(list(
      inning = state$inning, half = state$half, team = p$team %||% state$batting_team,
      out_player_id = p$out_player_id, in_player_id = p$in_player$player_id)))
  }
```

- [ ] **Step 5: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_pinch_runner.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0.

- [ ] **Step 6: Commit**

```bash
git add R/rule_pinch_runner.R R/game_reducer.R tests/test_pinch_runner.R
git commit -m "feat: pinch-runner limits and eligibility; log substitutions in state"
```

---

### Task 7: Rule presets

**Files:**
- Create: `R/rule_presets.R`
- Modify: `R/rules_engine.R` (add `ruleset_is_genderless`)
- Test: `tests/test_rule_presets.R` (create)

**Interfaces:**
- Produces:
  - `RULE_PRESETS` — an ordered named list; each entry `list(id=, label=, description=, config=)`.
  - `preset_ruleset(id)` → a fully coerced config. Errors on an unknown id.
  - `preset_choices()` → a named character vector for `selectInput` (`label = id`).
  - `ruleset_is_genderless(cfg)` → `<lgl>` (defined in `R/rules_engine.R` so the engine owns it).

- [ ] **Step 1: Write the failing test**

Create `tests/test_rule_presets.R`:

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","rule_presets.R"))
  source(file.path("R", f))

test_that("every preset is valid and round-trips through coercion", {
  for (id in names(RULE_PRESETS)) {
    cfg <- preset_ruleset(id)
    v <- validate_ruleset_config(cfg)
    expect_true(v$ok, info = paste(id, ":", paste(v$errors, collapse = "; ")))
    expect_identical(cfg, coerce_ruleset_config(cfg), info = paste(id, "is not idempotent"))
    expect_equal(cfg$preset, id)
  }
})

test_that("every preset has a label and a description", {
  for (id in names(RULE_PRESETS)) {
    expect_true(nzchar(RULE_PRESETS[[id]]$label), info = id)
    expect_true(nzchar(RULE_PRESETS[[id]]$description), info = id)
  }
})

test_that("anything_goes is the default and matches default_ruleset_config", {
  expect_equal(names(RULE_PRESETS)[1], "anything_goes")
  d <- default_ruleset_config()
  a <- preset_ruleset("anything_goes")
  expect_equal(a$starting_count, d$starting_count)
  expect_equal(a$foul_out_rule, d$foul_out_rule)
  expect_equal(a$batting_gender_rule$type, "none")
})

test_that("the standard presets differ in the four ways that matter", {
  bb <- preset_ruleset("standard_baseball")
  sp <- preset_ruleset("standard_slowpitch")
  fp <- preset_ruleset("standard_fastpitch")
  expect_equal(bb$innings, 9L);  expect_equal(sp$innings, 7L)
  expect_equal(bb$fielding$fielder_count, 9L)
  expect_equal(sp$fielding$fielder_count, 10L)
  expect_equal(bb$foul_out_rule, "unlimited")
  expect_equal(sp$foul_out_rule, "out")
  expect_length(bb$mercy_rule$tiers, 0L)
  expect_length(sp$mercy_rule$tiers, 3L)
  expect_equal(fp$pinch_runner$allowed_for, "pitcher_catcher")
})

test_that("the GameOn presets reuse STANDARD_COED_FIELDING", {
  for (id in c("gameon_summer", "gameon_spring")) {
    cfg <- preset_ruleset(id)
    expect_equal(cfg$fielding$min_females, STANDARD_COED_FIELDING$min_females)
    expect_equal(cfg$fielding$max_males,   STANDARD_COED_FIELDING$max_males)
    expect_length(cfg$fielding$tiers, length(STANDARD_COED_FIELDING$tiers))
    expect_equal(cfg$fielding$fielder_count, 10L)
    expect_equal(cfg$batting_gender_rule$type, "max_consecutive_males")
    expect_equal(cfg$batting_gender_rule$n, 2L)
    expect_equal(cfg$home_run_rule$over_fence_limit, 3L)
    expect_equal(cfg$home_run_rule$over_limit_result, "out")
    expect_equal(cfg$pinch_runner$max_per_inning, 1L)
    expect_equal(cfg$pinch_runner$eligibility, "same_gender")
    expect_equal(cfg$innings, 7L)
  }
})

test_that("GameOn Summer and Spring differ only in the starting count", {
  su <- preset_ruleset("gameon_summer"); sp <- preset_ruleset("gameon_spring")
  expect_equal(su$starting_count, list(balls = 0L, strikes = 0L))
  expect_equal(sp$starting_count, list(balls = 1L, strikes = 1L))
  su$starting_count <- NULL; sp$starting_count <- NULL
  su$preset <- NULL; sp$preset <- NULL
  expect_identical(su, sp)
})

test_that("ruleset_is_genderless separates the genderless presets from GameOn", {
  for (id in c("anything_goes", "standard_baseball", "standard_slowpitch",
               "standard_fastpitch"))
    expect_true(ruleset_is_genderless(preset_ruleset(id)), info = id)
  for (id in c("gameon_summer", "gameon_spring"))
    expect_false(ruleset_is_genderless(preset_ruleset(id)), info = id)
})

test_that("a genderless ruleset stops being genderless when a gender rule is added", {
  cfg <- preset_ruleset("anything_goes")
  expect_true(ruleset_is_genderless(cfg))
  cfg$fielding$min_females <- 2L
  expect_false(ruleset_is_genderless(cfg))
})

test_that("preset_ruleset rejects an unknown id", {
  expect_error(preset_ruleset("nope"), "unknown preset")
})

test_that("preset_choices maps labels to ids", {
  ch <- preset_choices()
  expect_equal(unname(ch[["Anything Goes"]]), "anything_goes")
  expect_length(ch, length(RULE_PRESETS))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_rule_presets.R")'`
Expected: FAIL — cannot open `R/rule_presets.R`.

- [ ] **Step 3: Add `ruleset_is_genderless()` to `R/rules_engine.R`**

Place it directly after `evaluate_fielding()`:

```r
# A ruleset is genderless when nothing in it references player gender. The setup UI
# uses this to drop the M/F column from the lineup table entirely.
ruleset_is_genderless <- function(cfg) {
  f <- cfg$fielding
  identical(cfg$batting_gender_rule$type, "none") &&
    identical(cfg$male_walk_rule, "none") &&
    (f$min_females %||% 0L) == 0L &&
    is.na(f$max_males %||% NA_integer_) &&
    length(f$tiers %||% list()) == 0L &&
    length(cfg$home_run_rule$limit_by_gender %||% list()) == 0L &&
    !cfg$pinch_runner$eligibility %in% c("same_gender", "last_same_gender_out")
}
```

- [ ] **Step 4: Create `R/rule_presets.R`**

```r
# Named rule presets. Each `config` is a partial ruleset merged over the defaults by
# preset_ruleset(); anything not named here takes its default_ruleset_config() value.

.USA_MERCY <- list(
  list(after_inning = 3L, differential = 20L),
  list(after_inning = 4L, differential = 15L),
  list(after_inning = 5L, differential = 10L)
)

# GameOn Summer and Spring are identical apart from the starting count, so the shared
# body lives here once.
.GAMEON_BASE <- list(
  foul_out_rule = "one_courtesy_foul",
  batting_gender_rule = list(type = "max_consecutive_males", n = 2L),
  innings = 7L,
  fielding = utils::modifyList(STANDARD_COED_FIELDING, list(fielder_count = 10L)),
  home_run_rule = list(over_fence_limit = 3L, over_limit_result = "out",
                       inside_park_counts = FALSE),
  pinch_runner = list(max_per_inning = 1L, eligibility = "same_gender")
)

RULE_PRESETS <- list(
  anything_goes = list(
    id = "anything_goes", label = "Anything Goes",
    description = "Genderless default. 0-0 count, unlimited fouls, everyone bats, 7 innings, no caps or limits.",
    config = list()),

  standard_baseball = list(
    id = "standard_baseball", label = "Standard Baseball",
    description = "9 innings, 9 fielders, 9 batters, unlimited fouls, no run cap or mercy rule.",
    config = list(innings = 9L, batting_size = 9L,
                  fielding = list(fielder_count = 9L))),

  standard_slowpitch = list(
    id = "standard_slowpitch", label = "Standard Slowpitch Softball",
    description = "7 innings, 10 fielders, everyone bats, a foul with two strikes is an out, USA Softball mercy schedule.",
    config = list(innings = 7L, foul_out_rule = "out",
                  fielding = list(fielder_count = 10L),
                  mercy_rule = list(tiers = .USA_MERCY))),

  standard_fastpitch = list(
    id = "standard_fastpitch", label = "Standard Fastpitch Softball",
    description = "7 innings, 9 fielders, 9 batters, unlimited fouls, USA Softball mercy schedule, courtesy runner for the pitcher or catcher only.",
    config = list(innings = 7L, batting_size = 9L,
                  fielding = list(fielder_count = 9L),
                  mercy_rule = list(tiers = .USA_MERCY),
                  pinch_runner = list(allowed_for = "pitcher_catcher"))),

  gameon_summer = list(
    id = "gameon_summer", label = "GameOn Summer",
    description = "Coed: 0-0 count, one courtesy foul, no three males in a row, standard coed fielding, 3 home runs, one same-gender courtesy runner per inning.",
    config = utils::modifyList(.GAMEON_BASE,
      list(starting_count = list(balls = 0L, strikes = 0L)))),

  gameon_spring = list(
    id = "gameon_spring", label = "GameOn Spring",
    description = "GameOn Summer with a 1-1 starting count.",
    config = utils::modifyList(.GAMEON_BASE,
      list(starting_count = list(balls = 1L, strikes = 1L))))
)

preset_ruleset <- function(id) {
  p <- RULE_PRESETS[[id]]
  if (is.null(p)) stop(sprintf("unknown preset: %s", id))
  cfg <- coerce_ruleset_config(p$config)
  cfg$preset <- id
  cfg
}

# Named vector for selectInput: names are labels, values are ids.
preset_choices <- function()
  stats::setNames(vapply(RULE_PRESETS, function(p) p$id, character(1)),
                  vapply(RULE_PRESETS, function(p) p$label, character(1)))
```

**Sourcing order matters here.** `.GAMEON_BASE` references `STANDARD_COED_FIELDING` at
*load* time, not inside a function, so `rules_engine.R` must be sourced before
`rule_presets.R`. `global.R` sources `R/` alphabetically after its `.r_first` list, and
`rule_presets.R` sorts *before* `rules_engine.R` (`rule_p` < `rule_s`) — so the default
order is wrong and the app fails to start. **Add `rules_engine.R` to the `.r_first` vector
in `global.R`**:

```r
.r_first <- file.path("R", c("brand_colors.R", "app_config.R", "rules_engine.R"))
```

- [ ] **Step 5: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_rule_presets.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0.

- [ ] **Step 6: Commit**

```bash
git add R/rule_presets.R R/rules_engine.R global.R tests/test_rule_presets.R
git commit -m "feat: six rule presets and a genderless-ruleset predicate"
```

---

### Task 8: `lineup_set` event and retroactive warnings

**Files:**
- Modify: `R/game_events.R` (`EVENT_TYPES`, `validate_event`)
- Modify: `R/game_reducer.R` (`apply_event`, `.refresh_flags`)
- Test: `tests/test_reducer_lineup_set.R` (create)

**Interfaces:**
- Produces: event type `"lineup_set"`, payload `list(team = <chr>, lineup = <list>)`. The
  reducer replaces `state$lineups[[team]]`, clamps `state$batting_index[[team]]`, refreshes
  the current batter, and re-runs the flags. Slice 2.2's setup/tracking UI emits it.

- [ ] **Step 1: Write the failing test**

Create `tests/test_reducer_lineup_set.R`:

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

mk <- function(prefix, genders) lapply(seq_along(genders), function(i)
  make_player(paste0(prefix, i), paste(prefix, i), genders[i], i, i, i))

start_runonly <- function(cfg = default_ruleset_config())
  new_event("game_start", list(ruleset = cfg, first_bat = "away",
    home = list(team_id="H", name="Home", lineup = mk("h", c("M","F","M","F"))),
    away = list(team_id="A", name="Away", lineup = list())), seq = 1L)

test_that("lineup_set is a known event type and validates its payload", {
  expect_true("lineup_set" %in% EVENT_TYPES)
  ok <- new_event("lineup_set", list(team = "away", lineup = list()))
  expect_true(validate_event(ok)$ok)
  expect_false(validate_event(new_event("lineup_set", list(team = "x", lineup = list())))$ok)
  expect_false(validate_event(new_event("lineup_set", list(team = "away")))$ok)
})

test_that("lineup_set installs a lineup on a run-only team and sets the batter", {
  s <- fold_events(list(
    start_runonly(),
    new_event("lineup_set", list(team = "away", lineup = mk("a", c("M","F","M"))), seq = 2L)))
  expect_length(s$lineups$away, 3L)
  expect_equal(s$current_batter$player_id, "a1")
})

test_that("lineup_set clamps a batting index that overruns the new lineup", {
  s0 <- fold_events(list(start_runonly()))
  s0$batting_index$home <- 9L
  evt <- new_event("lineup_set", list(team = "home", lineup = mk("h", c("M","F"))), seq = 2L)
  s <- apply_event(s0, evt)
  expect_length(s$lineups$home, 2L)
  # index 9 %% 2 == 1 -> second batter; must not error or return NULL
  expect_false(is.null(s$current_batter))
})

test_that("a late lineup triggers a retroactive batting-order violation", {
  cfg <- coerce_ruleset_config(list(
    batting_gender_rule = list(type = "max_consecutive_males", n = 1L)))
  # Two male plate appearances are recorded before the lineup identifies their genders.
  pa <- function(id, seq) new_event("plate_appearance", list(team = "away",
    batter_id = id, outcome = "1B", reached = 1L, rbi = 0L, outs_on_play = 0L,
    advances = list(make_advance(id, 0L, 1L))), seq = seq)
  s <- fold_events(list(
    start_runonly(cfg),
    new_event("lineup_set", list(team = "away", lineup = mk("a", c("M","M","F"))), seq = 2L),
    pa("a1", 3L), pa("a2", 4L)))
  codes <- vapply(s$warnings, function(w) w$code, character(1))
  expect_true("batting_gender" %in% codes)
})

test_that("lineup_set re-evaluates already-recorded plate appearances", {
  cfg <- coerce_ruleset_config(list(
    batting_gender_rule = list(type = "max_consecutive_males", n = 1L)))
  pa <- function(id, seq) new_event("plate_appearance", list(team = "away",
    batter_id = id, outcome = "1B", reached = 1L, rbi = 0L, outs_on_play = 0L,
    advances = list(make_advance(id, 0L, 1L))), seq = seq)
  # Batters recorded first; the lineup naming them both male arrives afterwards.
  s <- fold_events(list(
    start_runonly(cfg),
    pa("a1", 2L), pa("a2", 3L),
    new_event("lineup_set", list(team = "away", lineup = mk("a", c("M","M","F"))), seq = 4L)))
  hits <- Filter(function(w) identical(w$code, "batting_gender_retro"), s$warnings)
  expect_length(hits, 1L)
  expect_match(hits[[1]]$message, "a2")
  expect_equal(hits[[1]]$severity, "violation")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_reducer_lineup_set.R")'`
Expected: FAIL — `"lineup_set" %in% EVENT_TYPES` is FALSE.

- [ ] **Step 3: Register the event type**

In `R/game_events.R`, add `"lineup_set"` to `EVENT_TYPES` and add a validation branch:

```r
  if (identical(evt$type, "lineup_set")) {
    if (!isTRUE(evt$payload$team %in% c("home", "away")))
      add("lineup_set needs team home/away")
    if (!is.list(evt$payload$lineup)) add("lineup_set needs a lineup list")
  }
```

- [ ] **Step 4: Add the reducer branch and the retroactive check**

In `R/game_reducer.R`, add a helper above `.refresh_flags()`:

```r
# Replays the batting-order gender rule over plate appearances already recorded for
# `team`, using the lineup that is now known. Returns a single violation naming the
# earliest offending batter, or NULL. This is what surfaces a rule break that could not
# be evaluated while the lineup was empty.
.retro_batting_gender_violation <- function(state, team) {
  cfg <- state$ruleset
  if (identical(cfg$batting_gender_rule$type, "none")) return(NULL)
  recs <- Filter(function(r) identical(r$team, team), state$pa_log %||% list())
  if (length(recs) < 2L) return(NULL)
  gender_of <- function(id) {
    hit <- Filter(function(p) identical(p$player_id, id), state$lineups[[team]])
    if (length(hit)) hit[[1]]$gender else NA_character_
  }
  seen <- character()
  for (r in recs) {
    g <- gender_of(r$batter_id)
    if (is.na(g)) next
    if (!next_batter_gender_ok(cfg, seen, g)) {
      nm <- Filter(function(p) identical(p$player_id, r$batter_id), state$lineups[[team]])
      nm <- if (length(nm)) nm[[1]]$name else r$batter_id
      return(list(severity = "violation", code = "batting_gender_retro",
        message = sprintf(
          "Batting order: %s batted out of order under this ruleset (inning %d).",
          nm, r$inning)))
    }
    seen <- c(seen, g)
  }
  NULL
}
```

Then add the branch to `apply_event()`, before the `substitution` line:

```r
  if (type == "lineup_set") {
    team <- evt$payload$team
    state$lineups[[team]] <- evt$payload$lineup %||% list()
    n <- length(Filter(function(p) !is.na(p$order_slot), state$lineups[[team]]))
    if (n > 0L) state$batting_index[[team]] <- state$batting_index[[team]] %% n
    else state$batting_index[[team]] <- 0L
    state <- .set_current_batter(state)
    state <- .refresh_flags(state)
    retro <- .retro_batting_gender_violation(state, team)
    if (!is.null(retro)) state$warnings <- c(state$warnings, list(retro))
    return(state)
  }
```

The retroactive check runs *after* `.refresh_flags()` so it is appended rather than
overwritten — `.refresh_flags()` assigns `state$warnings` wholesale.

- [ ] **Step 5: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_reducer_lineup_set.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0.

- [ ] **Step 6: Commit**

```bash
git add R/game_events.R R/game_reducer.R tests/test_reducer_lineup_set.R
git commit -m "feat: lineup_set event with retroactive batting-order evaluation"
```

---

### Task 9: Lineup table

**Files:**
- Modify: `R/setup_module.R` (`.player_row` → `.lineup_table`, `.lineup_ui`, `collect_lineup`)
- Modify: `www/css/app.css`
- Test: `tests/test_setup_module.R` (extend)

**Interfaces:**
- Produces:
  - `.lineup_table_head(show_gender)` → a `<thead>` tag.
  - `.player_row(ns, prefix, id, order, show_gender)` → a `<tr>` whose cells hold the same
    Shiny inputs as before, with unchanged input ids (`<prefix>_<field>_<id>`).
  - `collect_lineup(input, prefix, row_ids, show_gender = TRUE)` — unchanged return shape;
    a blank or non-numeric jersey now yields `NA_integer_` rather than `0L`.

- [ ] **Step 1: Write the failing test**

Replace the jersey assertion in the existing `tests/test_setup_module.R` test
("collect_lineup reads rows, skips blanks, assigns order_slot"): the comment says
`# blank jersey -> 0` and asserts `expect_equal(lu[[2]]$jersey_number, 0L)`. Change it to
`expect_true(is.na(lu[[2]]$jersey_number))`. Then append:

```r
test_that("a non-numeric jersey becomes NA without a warning", {
  input <- list(t_name_1 = "Sam", t_gender_1 = "F", t_jersey_1 = "oops", t_pos_1 = "")
  expect_silent(lu <- collect_lineup(input, "t", 1))
  expect_true(is.na(lu[[1]]$jersey_number))
})

test_that("a jersey entered as a digit string is read as a number", {
  input <- list(t_name_1 = "Sam", t_gender_1 = "F", t_jersey_1 = "07", t_pos_1 = "")
  lu <- collect_lineup(input, "t", 1)
  expect_equal(lu[[1]]$jersey_number, 7L)
})

test_that("collect_lineup defaults gender when the column is not rendered", {
  input <- list(t_name_1 = "Sam", t_jersey_1 = "9", t_pos_1 = "SS")   # no t_gender_1
  lu <- collect_lineup(input, "t", 1, show_gender = FALSE)
  expect_equal(lu[[1]]$name, "Sam")
  expect_equal(lu[[1]]$gender, "M")
})

test_that("the lineup table renders a real table with the expected headers", {
  ns <- shiny::NS("setup")
  html <- as.character(.lineup_table_head(show_gender = TRUE))
  for (h in c("#", "Name", "Gender", "Jersey", "Position"))
    expect_true(grepl(paste0(">", h, "<"), html, fixed = TRUE), info = h)
})

test_that("the gender column disappears for a genderless ruleset", {
  html <- as.character(.lineup_table_head(show_gender = FALSE))
  expect_false(grepl(">Gender<", html, fixed = TRUE))
  expect_true(grepl(">Name<", html, fixed = TRUE))
})

test_that("a player row is a tr with cells and keeps its input ids", {
  ns <- shiny::NS("setup")
  html <- as.character(.player_row(ns, "away", 3L, order = 2L, show_gender = TRUE))
  expect_true(grepl("^<tr", html))
  expect_true(grepl('id="setup-away_name_3"', html, fixed = TRUE))
  expect_true(grepl('id="setup-away_pos_3"', html, fixed = TRUE))
  expect_true(grepl('id="setup-away_gender_3"', html, fixed = TRUE))
})

test_that("a genderless player row omits the gender input entirely", {
  ns <- shiny::NS("setup")
  html <- as.character(.player_row(ns, "away", 3L, order = 1L, show_gender = FALSE))
  expect_false(grepl("away_gender_3", html, fixed = TRUE))
  expect_true(grepl('id="setup-away_name_3"', html, fixed = TRUE))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_setup_module.R")'`
Expected: FAIL — `.lineup_table_head` not found; `.player_row` has no `order`/`show_gender` arguments.

- [ ] **Step 3: Rewrite the row markup in `R/setup_module.R`**

```r
.POS_CHOICES <- function()
  c("(pos)" = "", stats::setNames(APP_CONFIG$positions, APP_CONFIG$positions))

.lineup_table_head <- function(show_gender) {
  cols <- c("#", "Name", if (show_gender) "Gender", "Jersey", "Position", "")
  tags$thead(tags$tr(!!!lapply(cols, function(h) tags$th(scope = "col", h))))
}

.player_row <- function(ns, prefix, id, order, show_gender) {
  cell <- function(...) tags$td(class = "bw-cell", ...)
  tags$tr(id = ns(paste0(prefix, "_row_", id)),
    tags$td(class = "bw-order", order),
    cell(textInput(ns(paste0(prefix, "_name_", id)), NULL, placeholder = "Name")),
    if (show_gender)
      cell(selectInput(ns(paste0(prefix, "_gender_", id)), NULL,
                       c("M" = "M", "F" = "F"), width = "5rem")),
    cell(textInput(ns(paste0(prefix, "_jersey_", id)), NULL,
                   placeholder = "#", width = "5rem")),
    cell(selectInput(ns(paste0(prefix, "_pos_", id)), NULL, .POS_CHOICES(),
                     width = "7rem")),
    cell(actionButton(ns(paste0(prefix, "_del_", id)), "×",
                      class = "btn-sm btn-outline-danger")))
}
```

The jersey `textInput` needs `inputmode="numeric"`; Shiny has no argument for it, so tag it
after construction. Wrap the jersey cell:

```r
    cell(tagAppendAttributes(
      textInput(ns(paste0(prefix, "_jersey_", id)), NULL, placeholder = "#", width = "5rem"),
      inputmode = "numeric", .cssSelector = "input")),
```

Rewrite `.lineup_ui()` to emit the table shell that rows are inserted into:

```r
.lineup_ui <- function(ns, prefix, title, show_gender) {
  tagList(
    tags$h5(title),
    tags$p(class = "text-muted small",
      "Leave this lineup empty to just record this team's runs each inning."),
    tags$div(class = "bw-lineup-wrap",
      tags$table(class = "table table-sm bw-lineup",
        .lineup_table_head(show_gender),
        tags$tbody(id = ns(paste0(prefix, "_rows"))))),
    div(class = "d-flex gap-2",
      actionButton(ns(paste0(prefix, "_add")), "Add player",
                   class = "btn-sm btn-outline-secondary"),
      actionButton(ns(paste0(prefix, "_save")), "Save lineup",
                   class = "btn-sm btn-primary")),
    uiOutput(ns(paste0(prefix, "_validation"))))
}
```

The `id` moves from a `<div>` onto the `<tbody>`, so `insertUI(..., where = "beforeEnd")`
appends `<tr>`s into the table body. Task 10 wires the `_save` button and
`_validation` output.

- [ ] **Step 4: Make `collect_lineup` tolerant**

```r
.parse_jersey <- function(x) {
  if (is.null(x) || length(x) != 1) return(NA_integer_)
  x <- trimws(as.character(x))
  if (!nzchar(x) || is.na(x) || !grepl("^[0-9]+$", x)) return(NA_integer_)
  as.integer(x)
}

collect_lineup <- function(input, prefix, row_ids, show_gender = TRUE) {
  players <- list()
  for (id in row_ids) {
    nm <- trimws(input[[paste0(prefix, "_name_", id)]] %||% "")
    if (!nzchar(nm)) next
    pos <- input[[paste0(prefix, "_pos_", id)]] %||% ""
    pos <- if (!nzchar(pos)) NA_character_ else pos
    gender <- if (show_gender) (input[[paste0(prefix, "_gender_", id)]] %||% "M") else "M"
    slot <- length(players) + 1L
    players[[slot]] <- make_player(uuid::UUIDgenerate(), nm, gender,
      jersey_number = .parse_jersey(input[[paste0(prefix, "_jersey_", id)]]),
      order_slot = slot, position = pos)
  }
  players
}
```

`make_player()` already does `as.integer(jersey_number)`; passing `NA_integer_` through it
is safe and stays `NA_integer_`.

- [ ] **Step 5: Style the table**

Append to `www/css/app.css`:

```css
/* Lineup editor: a real table whose cells hold Shiny inputs. */
.bw-lineup-wrap { overflow-x: auto; }
.bw-lineup { margin-bottom: .5rem; }
.bw-lineup thead th { position: sticky; top: 0; z-index: 2;
  background: var(--bs-light); font-size: .8rem; text-transform: uppercase;
  letter-spacing: .03em; }
.bw-lineup td.bw-cell { padding: .15rem .25rem; vertical-align: middle; }
.bw-lineup td.bw-order { width: 2rem; text-align: right; padding-right: .5rem;
  color: var(--bs-secondary); font-variant-numeric: tabular-nums; }
/* Inputs inside cells must not carry Bootstrap's block margin, or the rows
   stop reading as table rows. */
.bw-lineup .form-group, .bw-lineup .shiny-input-container { margin-bottom: 0; width: auto; }
.bw-lineup .shiny-input-container:not(.shiny-input-container-inline) { width: auto; }
/* Keep the name column readable when the table scrolls horizontally. */
.bw-lineup td:nth-child(2) { min-width: 9rem; }
@media (max-width: 576px) { .bw-lineup { font-size: .9rem; } }
```

- [ ] **Step 6: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_setup_module.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: PASS, exit 0. `tests/test_app_flow.R` drives the setup form via
`session$setInputs`; the input ids are unchanged so it still passes.

- [ ] **Step 7: Commit**

```bash
git add R/setup_module.R www/css/app.css tests/test_setup_module.R
git commit -m "feat: lineup editor as a real table; tolerant jersey parsing"
```

---

### Task 10: Rules-first setup with per-team validation

**Files:**
- Modify: `R/setup_module.R` (`setup_ui`, `setup_server`, `collect_ruleset`)
- Create: `R/lineup_validation.R`
- Test: `tests/test_lineup_validation.R` (create), `tests/test_app_flow.R` (update inputs)

**Interfaces:**
- Produces:
  - `validate_lineup(cfg, lineup, team_label)` → `list(ok = <lgl>, items = list(list(severity=, message=)))`.
  - `setup_ui(id)` renders Rules before Teams, with a preset picker at the top.
  - `collect_ruleset(input)` reads the preset plus every advanced control.

- [ ] **Step 1: Write the failing test**

Create `tests/test_lineup_validation.R`:

```r
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_lineup_validation.R")'`
Expected: FAIL — cannot open `R/lineup_validation.R`.

- [ ] **Step 3: Create `R/lineup_validation.R`**

```r
# Validates one team's lineup against a ruleset, for the setup screen's Save button.
# Pure: no Shiny, no state. Returns severity-tagged items for inline display.
# Nothing here blocks the user — a scorer may knowingly field an illegal lineup.

validate_lineup <- function(cfg, lineup, team_label) {
  items <- list()
  add <- function(severity, message)
    items[[length(items) + 1]] <<- list(severity = severity, message = message)

  batters <- Filter(function(p) !is.na(p$order_slot), lineup)
  if (length(batters) == 0L) {
    add("notice", sprintf("%s has no lineup — it will be tracked by runs per inning.",
                          team_label))
    return(list(ok = TRUE, items = items))
  }
  batters <- batters[order(vapply(batters, function(p) p$order_slot, integer(1)))]

  bs <- cfg$batting_size
  if (!is.na(bs) && length(batters) != bs)
    add("notice", sprintf("%d batters entered; this ruleset expects %d.",
                          length(batters), bs))

  jerseys <- vapply(batters, function(p) p$jersey_number, integer(1))
  dupe_j <- unique(jerseys[!is.na(jerseys) & duplicated(jerseys)])
  for (j in dupe_j) add("notice", sprintf("Jersey number %d is used more than once.", j))

  names_ <- tolower(trimws(vapply(batters, function(p) p$name, character(1))))
  for (n in unique(names_[duplicated(names_)]))
    add("notice", sprintf("More than one player is named \"%s\".", n))

  # Batting-order gender rule, checked forwards and then around the turn, because the
  # order repeats: the last batter is followed by the first.
  if (!identical(cfg$batting_gender_rule$type, "none")) {
    g <- vapply(batters, function(p) p$gender, character(1))
    seen <- character()
    for (i in seq_along(g)) {
      if (!next_batter_gender_ok(cfg, seen, g[i])) {
        add("violation", sprintf("Batting order: %s (slot %d) breaks the gender rule.",
                                 batters[[i]]$name, i))
        break
      }
      seen <- c(seen, g[i])
    }
    # Wrap-around: replay the tail of the order into the head.
    wrapped <- c(g, g)
    seen2 <- character(); flagged <- FALSE
    for (i in seq_along(wrapped)) {
      if (!next_batter_gender_ok(cfg, seen2, wrapped[i]) && i > length(g)) {
        add("violation", sprintf(
          "Batting order: the order breaks the gender rule where it wraps around (slot %d back to slot 1).",
          length(g)))
        flagged <- TRUE
        break
      }
      seen2 <- c(seen2, wrapped[i])
      if (flagged) break
    }
  }

  fielders <- Filter(function(p) !is.na(.position_category(p$position)), lineup)
  fc <- cfg$fielding$fielder_count
  if (!is.na(fc) && length(fielders) > 0L && length(fielders) != fc)
    add("notice", sprintf("%d fielding positions assigned; this ruleset expects %d.",
                          length(fielders), fc))

  for (v in evaluate_fielding(cfg, lineup)) add(v$severity, v$message)

  list(ok = !any(vapply(items, function(i) identical(i$severity, "violation"), logical(1))),
       items = items)
}
```

`.position_category()` is defined in `R/rules_engine.R`; `lineup_validation.R` sorts after
it alphabetically, and `rules_engine.R` is in `.r_first` after Task 7, so the load order is
safe either way.

- [ ] **Step 4: Reorder `setup_ui()` and add the preset picker**

Replace `setup_ui()`:

```r
setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("New game"),

    tags$h4(class = "mt-3", "1. Rules"),
    selectInput(ns("preset"), "Ruleset", preset_choices(), selected = "anything_goes"),
    uiOutput(ns("preset_desc")),
    accordion(open = FALSE,
      accordion_panel("Advanced rules",
        layout_columns(col_widths = c(6, 6),
          numericInput(ns("start_balls"), "Starting balls", 0, 0, 3),
          numericInput(ns("start_strikes"), "Starting strikes", 0, 0, 2)),
        selectInput(ns("foul_out"), "Foul with 2 strikes",
          c("Out" = "out", "One courtesy foul" = "one_courtesy_foul",
            "Unlimited (never an out)" = "unlimited"), selected = "unlimited"),
        selectInput(ns("batting_size"), "Number of batters",
          c("Unlimited (everyone bats)" = "0", "9" = "9", "10" = "10")),
        selectInput(ns("gender_rule"), "Batting gender rule",
          c("None" = "none",
            "Max males in a row" = "max_consecutive_males",
            "Max of either gender in a row" = "max_consecutive_same_gender",
            "At least one F every N" = "min_females_per_n")),
        conditionalPanel(sprintf("input['%s'] != 'none'", ns("gender_rule")),
          numericInput(ns("gender_n"), "N", 1, 1, 12)),
        layout_columns(col_widths = c(4, 4, 4),
          numericInput(ns("innings"), "Innings", 7, 1, 12),
          numericInput(ns("fielder_count"), "Fielders (0 = any)", 0, 0, 12),
          numericInput(ns("run_cap"), "Run cap/inning (0 = none)", 0, 0, 30)),
        layout_columns(col_widths = c(6, 6),
          checkboxInput(ns("cap_same_play"), "Runs on the same play all count", TRUE),
          checkboxInput(ns("cap_ends_half"), "Reaching the cap ends the half-inning", TRUE)),
        checkboxInput(ns("open_last"), "No cap in the last inning", TRUE)),

      accordion_panel("Mercy rule",
        tags$p(class = "text-muted small",
          "The game ends as soon as any row is satisfied. Leave a differential at 0 to disable that row."),
        layout_columns(col_widths = c(6, 6),
          numericInput(ns("mercy_after_1"), "After inning", 3, 1, 12),
          numericInput(ns("mercy_diff_1"), "Run differential (0 = off)", 0, 0, 50)),
        layout_columns(col_widths = c(6, 6),
          numericInput(ns("mercy_after_2"), "After inning", 4, 1, 12),
          numericInput(ns("mercy_diff_2"), "Run differential (0 = off)", 0, 0, 50)),
        layout_columns(col_widths = c(6, 6),
          numericInput(ns("mercy_after_3"), "After inning", 5, 1, 12),
          numericInput(ns("mercy_diff_3"), "Run differential (0 = off)", 0, 0, 50))),

      accordion_panel("Home runs",
        layout_columns(col_widths = c(4, 4, 4),
          numericInput(ns("hr_limit"), "Over-the-fence limit (0 = none)", 0, 0, 30),
          numericInput(ns("hr_limit_m"), "Limit for men (0 = use overall)", 0, 0, 30),
          numericInput(ns("hr_limit_f"), "Limit for women (0 = use overall)", 0, 0, 30)),
        selectInput(ns("hr_over"), "A home run past the limit is",
          c("An out" = "out", "A ground-rule double" = "ground_rule_double",
            "A single" = "single")),
        checkboxInput(ns("hr_itp_counts"),
          "Inside-the-park home runs count toward the limit", FALSE)),

      accordion_panel("Pinch / courtesy runners",
        layout_columns(col_widths = c(4, 4, 4),
          numericInput(ns("pr_inning"), "Max per inning (0 = unlimited)", 0, 0, 12),
          numericInput(ns("pr_game"), "Max per game (0 = unlimited)", 0, 0, 30),
          numericInput(ns("pr_player"), "Max per player (0 = unlimited)", 0, 0, 12)),
        selectInput(ns("pr_elig"), "Who may run",
          c("Anyone" = "anyone", "Same gender" = "same_gender",
            "The last out" = "last_out",
            "The last same-gender out" = "last_same_gender_out")),
        selectInput(ns("pr_for"), "Who may be run for",
          c("Anyone" = "anyone", "Pitcher or catcher only" = "pitcher_catcher"))),

      accordion_panel("Fielding gender rules",
        selectInput(ns("fielding_preset"), "Preset",
          c("None" = "none", "Standard coed (10-player)" = "standard_coed",
            "Custom" = "custom")),
        conditionalPanel(sprintf("input['%s'] == 'custom'", ns("fielding_preset")),
          layout_columns(col_widths = c(6, 6),
            numericInput(ns("min_females"), "Min females in field", 0, 0, 10),
            numericInput(ns("max_males"), "Max males in field (0 = none)", 0, 0, 12)),
          layout_columns(col_widths = c(4, 4, 4),
            numericInput(ns("of_females"), "Min F outfield", 0, 0, 5),
            numericInput(ns("if_females"), "Min F infield", 0, 0, 5),
            selectInput(ns("battery_mode"), "Pitcher/Catcher",
              c("Any" = "any", "Opposite genders" = "one")))))),

    tags$h4(class = "mt-4", "2. Teams"),
    textInput(ns("away_name"), "Away team", "Away"),
    uiOutput(ns("away_lineup")),
    textInput(ns("home_name"), "Home team", "Home"),
    uiOutput(ns("home_lineup")),

    actionButton(ns("start"), "Start game", class = "btn-primary bw-outcome-btn mt-3"))
}
```

The two lineup blocks become `uiOutput`s because the gender column's presence depends on
the chosen ruleset, which is only known at runtime.

- [ ] **Step 5: Rewrite `collect_ruleset()`**

```r
.pos_or_na <- function(x) if (is.null(x) || is.na(x) || x <= 0) NA_integer_ else as.integer(x)

collect_ruleset <- function(input) {
  fielding <- switch(input$fielding_preset %||% "none",
    "standard_coed" = STANDARD_COED_FIELDING,
    "custom" = list(
      min_females = input$min_females %||% 0L,
      max_males = .pos_or_na(input$max_males),
      tiers = list(list(females = 0L,
        outfield = input$of_females %||% 0L, infield = input$if_females %||% 0L,
        battery = input$battery_mode %||% "any")),
      position_requirements = list()),
    list(min_females = 0L, max_males = NA_integer_, tiers = list(),
         position_requirements = list()))
  fielding$fielder_count <- .pos_or_na(input$fielder_count)

  mercy <- Filter(Negate(is.null), lapply(1:3, function(i) {
    d <- .pos_or_na(input[[paste0("mercy_diff_", i)]])
    if (is.na(d)) return(NULL)
    list(after_inning = as.integer(input[[paste0("mercy_after_", i)]] %||% 1L),
         differential = d)
  }))

  by_gender <- Filter(Negate(is.na), list(
    M = .pos_or_na(input$hr_limit_m), F = .pos_or_na(input$hr_limit_f)))

  gender_type <- input$gender_rule %||% "none"
  coerce_ruleset_config(list(
    preset = input$preset %||% "anything_goes",
    starting_count = list(balls = input$start_balls, strikes = input$start_strikes),
    foul_out_rule = input$foul_out,
    batting_gender_rule = list(type = gender_type,
      n = if (identical(gender_type, "none")) NA_integer_ else (input$gender_n %||% 1L)),
    batting_size = as.integer(input$batting_size %||% "0"),
    fielding = fielding,
    innings = input$innings,
    run_cap = list(
      per_inning = .pos_or_na(input$run_cap),
      open_last_inning = isTRUE(input$open_last),
      same_play_runs_count = isTRUE(input$cap_same_play),
      cap_ends_half = isTRUE(input$cap_ends_half)),
    mercy_rule = list(tiers = mercy),
    home_run_rule = list(
      over_fence_limit = .pos_or_na(input$hr_limit),
      limit_by_gender = by_gender,
      over_limit_result = input$hr_over %||% "out",
      inside_park_counts = isTRUE(input$hr_itp_counts)),
    pinch_runner = list(
      max_per_inning = .pos_or_na(input$pr_inning),
      max_per_game = .pos_or_na(input$pr_game),
      max_per_player_per_game = .pos_or_na(input$pr_player),
      eligibility = input$pr_elig %||% "anyone",
      allowed_for = input$pr_for %||% "anyone")))
}
```

`batting_size = 0` maps to `NA` (unlimited) inside `coerce_ruleset_config`, which already
handles it.

- [ ] **Step 6: Wire the server — preset application, lineup rendering, Save**

Replace `setup_server()`:

```r
setup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    game_start <- reactiveVal(NULL)
    rows <- list(away = reactiveVal(integer()), home = reactiveVal(integer()))
    counter <- reactiveVal(0L)

    # The chosen ruleset, read live so the lineup table can drop its gender column.
    ruleset <- reactive(tryCatch(collect_ruleset(input),
                                 error = function(e) default_ruleset_config()))
    show_gender <- reactive(!ruleset_is_genderless(ruleset()))

    output$preset_desc <- renderUI({
      p <- RULE_PRESETS[[input$preset %||% "anything_goes"]]
      req(p)
      tags$p(class = "text-muted small", p$description)
    })

    # Applying a preset writes its values into the advanced controls, so the two views
    # never disagree. Editing a control afterwards simply changes the effective ruleset.
    observeEvent(input$preset, {
      cfg <- preset_ruleset(input$preset)
      updateNumericInput(session, "start_balls",   value = cfg$starting_count$balls)
      updateNumericInput(session, "start_strikes", value = cfg$starting_count$strikes)
      updateSelectInput(session,  "foul_out",      selected = cfg$foul_out_rule)
      updateSelectInput(session,  "batting_size",
        selected = as.character(if (is.na(cfg$batting_size)) 0L else cfg$batting_size))
      updateSelectInput(session,  "gender_rule",   selected = cfg$batting_gender_rule$type)
      updateNumericInput(session, "gender_n",
        value = if (is.na(cfg$batting_gender_rule$n)) 1L else cfg$batting_gender_rule$n)
      updateNumericInput(session, "innings",       value = cfg$innings)
      updateNumericInput(session, "fielder_count",
        value = if (is.na(cfg$fielding$fielder_count)) 0L else cfg$fielding$fielder_count)
      updateNumericInput(session, "run_cap",
        value = if (is.na(cfg$run_cap$per_inning)) 0L else cfg$run_cap$per_inning)
      updateCheckboxInput(session, "cap_same_play",  value = cfg$run_cap$same_play_runs_count)
      updateCheckboxInput(session, "cap_ends_half",  value = cfg$run_cap$cap_ends_half)
      updateCheckboxInput(session, "open_last",      value = cfg$run_cap$open_last_inning)
      for (i in 1:3) {
        t <- cfg$mercy_rule$tiers[[i]] %||% NULL
        updateNumericInput(session, paste0("mercy_after_", i),
          value = if (is.null(t)) c(3L, 4L, 5L)[i] else t$after_inning)
        updateNumericInput(session, paste0("mercy_diff_", i),
          value = if (is.null(t)) 0L else t$differential)
      }
      hr <- cfg$home_run_rule
      updateNumericInput(session, "hr_limit",
        value = if (is.na(hr$over_fence_limit)) 0L else hr$over_fence_limit)
      updateNumericInput(session, "hr_limit_m", value = hr$limit_by_gender$M %||% 0L)
      updateNumericInput(session, "hr_limit_f", value = hr$limit_by_gender$F %||% 0L)
      updateSelectInput(session,  "hr_over",     selected = hr$over_limit_result)
      updateCheckboxInput(session, "hr_itp_counts", value = hr$inside_park_counts)
      pr <- cfg$pinch_runner
      pr_map <- list(pr_inning = "max_per_inning", pr_game = "max_per_game",
                     pr_player = "max_per_player_per_game")
      for (input_id in names(pr_map)) {
        v <- pr[[pr_map[[input_id]]]]
        updateNumericInput(session, input_id, value = if (is.na(v)) 0L else v)
      }
      updateSelectInput(session, "pr_elig", selected = pr$eligibility)
      updateSelectInput(session, "pr_for",  selected = pr$allowed_for)
      updateSelectInput(session, "fielding_preset",
        selected = if (length(cfg$fielding$tiers)) "standard_coed" else "none")
    }, ignoreInit = FALSE)

    # Lineup shells re-render only when the gender column's presence changes, so typing
    # in the rules panel does not wipe entered names.
    output$away_lineup <- renderUI(.lineup_ui(ns, "away", "Away lineup", show_gender()))
    output$home_lineup <- renderUI(.lineup_ui(ns, "home", "Home lineup", show_gender()))

    add_row <- function(prefix) {
      counter(counter() + 1L); id <- counter()
      rows[[prefix]](c(rows[[prefix]](), id))
      insertUI(sprintf("#%s", ns(paste0(prefix, "_rows"))), where = "beforeEnd",
        ui = .player_row(ns, prefix, id, order = length(rows[[prefix]]()),
                         show_gender = isolate(show_gender())))
      observeEvent(input[[paste0(prefix, "_del_", id)]], {
        removeUI(sprintf("#%s", ns(paste0(prefix, "_row_", id))))
        rows[[prefix]](setdiff(rows[[prefix]](), id))
      }, ignoreInit = TRUE, once = TRUE)
    }
    observeEvent(input$away_add, add_row("away"), ignoreInit = TRUE)
    observeEvent(input$home_add, add_row("home"), ignoreInit = TRUE)

    .validation_ui <- function(prefix, label) {
      cfg <- ruleset()
      lu <- collect_lineup(input, prefix, rows[[prefix]](), show_gender = show_gender())
      r <- validate_lineup(cfg, lu, label)
      cls <- function(sev) switch(sev, violation = "text-danger",
                                  notice = "text-warning", "text-muted")
      div(class = "small mt-1",
        tags$div(class = if (r$ok) "text-success" else "text-danger",
          sprintf("%d batter(s) saved.%s", length(lu),
                  if (r$ok) "" else " Rule problems below.")),
        !!!lapply(r$items, function(i) tags$div(class = cls(i$severity), i$message)))
    }
    observeEvent(input$away_save,
      output$away_validation <- renderUI(.validation_ui("away", input$away_name %||% "Away")),
      ignoreInit = TRUE)
    observeEvent(input$home_save,
      output$home_validation <- renderUI(.validation_ui("home", input$home_name %||% "Home")),
      ignoreInit = TRUE)

    observeEvent(input$start, {
      cfg <- collect_ruleset(input)
      sg <- show_gender()
      away <- list(team_id = uuid::UUIDgenerate(), name = input$away_name,
                   lineup = collect_lineup(input, "away", rows$away(), show_gender = sg))
      home <- list(team_id = uuid::UUIDgenerate(), name = input$home_name,
                   lineup = collect_lineup(input, "home", rows$home(), show_gender = sg))
      output$away_validation <- renderUI(.validation_ui("away", input$away_name %||% "Away"))
      output$home_validation <- renderUI(.validation_ui("home", input$home_name %||% "Home"))
      game_start(build_game_start_event(cfg, home, away, "away"))
    })
    game_start
  })
}
```

- [ ] **Step 7: Update the app-flow test's setup inputs**

`tests/test_app_flow.R` sets `setup-start_balls`, `setup-foul_out`, `setup-gender_rule` and
friends. Add the new required inputs so `collect_ruleset()` does not read NULLs:
`setup-preset = "anything_goes"`, `setup-fielder_count = 0`, `setup-open_last = TRUE`,
`setup-cap_same_play = TRUE`, `setup-cap_ends_half = TRUE`, `setup-hr_limit = 0`,
`setup-hr_limit_m = 0`, `setup-hr_limit_f = 0`, `setup-hr_over = "out"`,
`setup-hr_itp_counts = FALSE`, `setup-pr_inning = 0`, `setup-pr_game = 0`,
`setup-pr_player = 0`, `setup-pr_elig = "anyone"`, `setup-pr_for = "anyone"`,
`setup-mercy_diff_1 = 0`, `setup-mercy_diff_2 = 0`, `setup-mercy_diff_3 = 0`.

`collect_ruleset` uses `%||%` for every read, so missing inputs fall back rather than error
— but set them anyway so the test exercises the real shape.

- [ ] **Step 8: Run the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: exit 0.

- [ ] **Step 9: Manual check**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'shiny::runApp(".", port = 4321, launch.browser = FALSE)'`

Confirm: choosing **GameOn Summer** shows the Gender column; choosing **Anything Goes**
removes it; **Save lineup** on a four-male GameOn lineup reports the fielding violation;
the app is usable at 375px wide.

- [ ] **Step 10: Commit**

```bash
git add R/setup_module.R R/lineup_validation.R tests/test_lineup_validation.R tests/test_app_flow.R
git commit -m "feat: rules-first setup with presets and per-team lineup validation"
```

---

## Definition of done for slice 2.1

- `Rscript run_tests.R` exits 0.
- Every preset validates and is idempotent under coercion.
- A pre-slice-2 ruleset folds without loss: `coerce_ruleset_config(list(run_cap_per_inning = 5L, mercy_rule = list(differential = 10L), batting_gender_rule = list(type = "every_other")))` produces the nested shape.
- Choosing a genderless preset removes the Gender column from both lineup tables.
- Save lineup reports batting size, duplicate jerseys and names, batting-order gender violations (including around the turn), fielder count, and fielding gender balance.
- `evaluate_home_run_limit()` and `evaluate_pinch_runner()` exist and are tested, but are not yet called from the tracking module — that is slice 2.2.
