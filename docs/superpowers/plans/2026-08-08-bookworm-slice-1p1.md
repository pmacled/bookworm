# Bookworm Slice 1.1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a real lineup editor, run-only teams, a `batting_size` rule, two config fixes, position labels+categories, a configurable coed fielding gender-balance engine (with count-specific tiers), and modal/toast warning surfacing — on top of the merged slice-one event-sourced app.

**Architecture:** Extends the existing pure event-sourced core (`R/game_reducer.R` folds events; `R/rules_engine.R` holds rules) and the Shiny modules (`R/setup_module.R`, `R/tracking_module.R`). New pure logic (`evaluate_fielding`, `collect_lineup`, `record_half_runs_event`, `half_runs` reducer branch, structured warnings) is unit-tested; UI changes are verified via `testServer` and a headless boot check.

**Tech Stack:** R, Shiny, bslib, testthat. Builds on slice one (merged to `main`).

## Global Constraints

- **`Rscript` is NOT on PATH.** Every test/boot command uses the full path: begin Bash commands with `RSCRIPT="/c/Program Files/R/R-4.5.3/bin/Rscript.exe"`, then e.g. `"$RSCRIPT" tests/test_rules_engine.R`. Run from the project root.
- Tests are flat files `tests/test_<area>.R` (`library(testthat)`, `source()` the R files under test, `test_that`/`expect_*`), runnable individually and via `Rscript run_tests.R`.
- The reducer and rules engine stay **pure** (no Shiny/DB/`Sys.time()`).
- `%||%` is defined once in `R/app_config.R` — never redefine it.
- Player **positions are optional label strings** (not integers). Blank/`NA`/`DH` = non-fielder.
- Warnings are **non-blocking**; they are surfaced (modals for violations, toasts for notices) but never hard-block scoring.
- Keep the full suite green after every task (`"$RSCRIPT" run_tests.R`, exit 0).
- Build on the merged slice-one branch `main`; do this work on a feature branch (the executor's SDD setup handles branching).

## Shared shapes (contracts used across tasks)

- **Position category map** (`R/app_config.R`): `APP_CONFIG$POSITION_CATEGORY`, a named character vector label→category. Categories: `"battery"` (P, C), `"infield"` (1B, 2B, SS, 3B), `"outfield"` (LF, LCF, CF, RCF, RF, OF, ROVER). `DH` and any unlisted/blank label ⇒ not a fielder.
- **Fielding config** (`ruleset$fielding`): `list(min_females = int, max_males = int|NA, tiers = list(<tier>...), position_requirements = list())`. A **tier**: `list(females = int, outfield = int, infield = int, battery = "one"|"any")`.
- **Structured warning item**: `list(severity = "violation"|"notice", code = <chr>, message = <chr>)`. `state$warnings` is a **list** of these (slice one used a character vector — this task migrates it).
- **`half_runs` event**: `new_event("half_runs", list(team = "home"|"away", runs = int))`.

---

## Task 1: Position labels + categories; player position as a label

**Files:**
- Modify: `R/app_config.R` (replace `positions` with label vocab + `POSITION_CATEGORY`)
- Modify: `R/game_events.R:8-15` (`make_player` position → label string)
- Modify: `R/game_reducer.R:209-217` (defensive-sub: stop integer-coercing position)
- Test: `tests/test_game_events.R` (add position-label assertions)

**Interfaces:**
- Produces: `APP_CONFIG$positions` (character vector of labels incl. `DH`), `APP_CONFIG$POSITION_CATEGORY` (named chr: label→`battery|infield|outfield`), `make_player(..., position = NA_character_)` storing an optional label string.
- Consumes: nothing new.

- [ ] **Step 1: Add the position-label test to `tests/test_game_events.R`** (append inside the file, after the existing `make_player` test)

```r
test_that("make_player keeps position as an optional label string", {
  expect_equal(make_player("p1","Sam","F", position = "SS")$position, "SS")
  expect_true(is.na(make_player("p2","Mo","M")$position))
  expect_equal(make_player("p3","Al","M", position = "DH")$position, "DH")
})

test_that("POSITION_CATEGORY groups positions", {
  expect_equal(APP_CONFIG$POSITION_CATEGORY[["P"]], "battery")
  expect_equal(APP_CONFIG$POSITION_CATEGORY[["SS"]], "infield")
  expect_equal(APP_CONFIG$POSITION_CATEGORY[["ROVER"]], "outfield")
  expect_null(APP_CONFIG$POSITION_CATEGORY[["DH"]])
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `"$RSCRIPT" tests/test_game_events.R`
Expected: failures on the two new tests (`make_player` integer-coerces position; `POSITION_CATEGORY` absent).

- [ ] **Step 3: Update `R/app_config.R`** — replace the `positions = c(...)` entry inside `APP_CONFIG` with a label vector, and add a `POSITION_CATEGORY` entry:

```r
APP_CONFIG <- list(
  app_name = "Bookworm",
  positions = c("P","C","1B","2B","3B","SS","LF","LCF","CF","RCF","RF","OF","ROVER","DH"),
  POSITION_CATEGORY = c(
    P = "battery", C = "battery",
    "1B" = "infield", "2B" = "infield", SS = "infield", "3B" = "infield",
    LF = "outfield", LCF = "outfield", CF = "outfield", RCF = "outfield",
    RF = "outfield", OF = "outfield", ROVER = "outfield"
    # DH and blank are intentionally absent: they are non-fielders.
  ),
  outcome_codes = c(
    "1B", "2B", "3B", "HR", "BB", "IBB", "HBP",
    "K", "KL", "GO", "FO", "LO", "PO", "FC", "E", "SF", "SAC"
  )
)
```

(Leave the `%||%` definition and `DB_TABLES` unchanged.)

- [ ] **Step 4: Update `make_player` in `R/game_events.R`** — change the `position` default and handling to a label string:

```r
make_player <- function(player_id, name, gender,
                        jersey_number = NA_integer_, order_slot = NA_integer_,
                        position = NA_character_) {
  list(player_id = player_id, name = name, gender = gender,
       jersey_number = as.integer(jersey_number),
       order_slot = as.integer(order_slot),
       position = if (length(position) != 1 || is.na(position)) NA_character_ else as.character(position))
}
```

- [ ] **Step 5: Update the defensive-sub branch in `R/game_reducer.R`** — in `apply_substitution`, change the `defensive` branch's position line from `as.integer(...)` to a label:

Replace `inp$position <- if (is.null(p$position)) NA_integer_ else as.integer(p$position)`
with `inp$position <- if (length(p$position) != 1 || is.na(p$position)) NA_character_ else as.character(p$position)`

- [ ] **Step 6: Run the affected suites, expect PASS**

Run: `"$RSCRIPT" tests/test_game_events.R && "$RSCRIPT" tests/test_reducer_subs.R && "$RSCRIPT" tests/test_reducer_core.R`
Expected: all pass. (Old tests that passed integer positions now store them as characters like `"6"`, which are simply treated as non-fielders — no assertion in those tests depends on the position value.)

- [ ] **Step 7: Commit**

```bash
git add R/app_config.R R/game_events.R R/game_reducer.R tests/test_game_events.R
git commit -m "feat: position label vocabulary + categories; player position as optional label"
```

---

## Task 2: `foul_out_rule = "unlimited"` and `batting_size` rule

**Files:**
- Modify: `R/rules_engine.R` (`default_ruleset_config`, `coerce_ruleset_config`, `validate_ruleset_config`)
- Test: `tests/test_rules_engine.R`

**Interfaces:**
- Produces: `default_ruleset_config()$batting_size` (`NA_integer_`); `foul_out_rule` accepts `"unlimited"`; `validate_ruleset_config` accepts both.
- Consumes: `.as_int_or_na` (already in `rules_engine.R`).

- [ ] **Step 1: Add tests to `tests/test_rules_engine.R`**

```r
test_that("foul_out_rule accepts unlimited", {
  cfg <- default_ruleset_config(); cfg$foul_out_rule <- "unlimited"
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("batting_size defaults to unlimited (NA) and validates", {
  expect_true(is.na(default_ruleset_config()$batting_size))
  expect_equal(coerce_ruleset_config(list(batting_size = 10))$batting_size, 10L)
  expect_equal(coerce_ruleset_config(list(batting_size = 0))$batting_size, NA_integer_)  # 0 => unlimited
  bad <- default_ruleset_config(); bad$batting_size <- -3L
  expect_false(validate_ruleset_config(bad)$ok)
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `"$RSCRIPT" tests/test_rules_engine.R`

- [ ] **Step 3: Edit `R/rules_engine.R`**

(a) Add `batting_size = NA_integer_,` to the `default_ruleset_config()` list (e.g. after `courtesy_runner = FALSE` — add a comma and the field).

(b) In `coerce_ruleset_config`, after the existing coercions add:
```r
  d$batting_size <- .as_int_or_na(d$batting_size)
```
Note `.as_int_or_na(0)` returns `0L`, not `NA`; to make `0 ⇒ unlimited`, add right after:
```r
  if (!is.na(d$batting_size) && d$batting_size < 1L) d$batting_size <- NA_integer_
```

(c) In `validate_ruleset_config`, change the foul check to allow `unlimited`, and add a `batting_size` check:
```r
  if (!cfg$foul_out_rule %in% c("out", "one_courtesy_foul", "unlimited")) add("invalid foul_out_rule")
```
and near the end (before the `list(ok=...)`):
```r
  if (!is.na(cfg$batting_size) && (!is.numeric(cfg$batting_size) || cfg$batting_size < 1)) {
    add("batting_size must be a positive integer or NA (unlimited)")
  }
```

- [ ] **Step 4: Run test, expect PASS**

Run: `"$RSCRIPT" tests/test_rules_engine.R`

- [ ] **Step 5: Commit**

```bash
git add R/rules_engine.R tests/test_rules_engine.R
git commit -m "feat: foul_out_rule unlimited + batting_size ruleset field"
```

---

## Task 3: Coed fielding gender-balance engine (`evaluate_fielding`)

**Files:**
- Modify: `R/rules_engine.R` (extend `fielding` in default/coerce; add `evaluate_fielding`, `STANDARD_COED_FIELDING`, `.position_category`)
- Test: `tests/test_fielding.R` (new)

**Interfaces:**
- Consumes: `APP_CONFIG$POSITION_CATEGORY`, `%||%`, `make_player`.
- Produces:
  - `.position_category(pos)` → `"battery"|"infield"|"outfield"|NA_character_`.
  - `evaluate_fielding(cfg, defense_lineup)` → **list of violation items** `list(severity="violation", code, message)`; empty list when no fielders or all checks pass.
  - `STANDARD_COED_FIELDING` — the preset config block.

Do NOT wire this into the reducer yet (Task 6 does). `fielding_warnings` stays in place until Task 6.

- [ ] **Step 1: Write `tests/test_fielding.R`**

```r
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

test_that("6+ females relaxes battery (both may be female)", {
  cfg <- fld(STANDARD_COED_FIELDING)
  d <- list(
    pl("F","P"), pl("F","C"),                                # both female
    pl("F","SS"), pl("F","1B"), pl("M","2B"), pl("M","3B"),
    pl("F","LF"), pl("F","LCF"), pl("M","RCF"), pl("M","RF")
  )  # F total = 6 -> tier battery "any"
  expect_false("battery_opposite" %in% codes(evaluate_fielding(cfg, d)))
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `"$RSCRIPT" tests/test_fielding.R`
Expected: `could not find function "evaluate_fielding"` / object `STANDARD_COED_FIELDING` not found.

- [ ] **Step 3: Add to `R/rules_engine.R`** (append these; do not remove `fielding_warnings` yet)

```r
STANDARD_COED_FIELDING <- list(
  min_females = 4L, max_males = 6L,
  tiers = list(
    list(females = 3L, outfield = 1L, infield = 1L, battery = "one"),
    list(females = 4L, outfield = 1L, infield = 1L, battery = "one"),
    list(females = 5L, outfield = 2L, infield = 2L, battery = "one"),
    list(females = 6L, outfield = 1L, infield = 1L, battery = "any")
  ),
  position_requirements = list()
)

.position_category <- function(pos) {
  if (is.null(pos) || length(pos) != 1 || is.na(pos)) return(NA_character_)
  cat <- APP_CONFIG$POSITION_CATEGORY[[as.character(pos)]]
  if (is.null(cat)) NA_character_ else cat
}

evaluate_fielding <- function(cfg, defense_lineup) {
  f <- cfg$fielding
  viol <- list()
  add <- function(code, message)
    viol[[length(viol) + 1]] <<- list(severity = "violation", code = code, message = message)

  fielders <- Filter(function(p) !is.na(.position_category(p$position)), defense_lineup)
  if (length(fielders) == 0) return(list())  # cannot evaluate without positions

  cat_of <- vapply(fielders, function(p) .position_category(p$position), character(1))
  is_f <- vapply(fielders, function(p) identical(p$gender, "F"), logical(1))
  Ftot <- sum(is_f); Mtot <- sum(!is_f)
  n_of <- sum(is_f & cat_of == "outfield")
  n_if <- sum(is_f & cat_of == "infield")

  minf <- f$min_females %||% 0L
  if (Ftot < minf) add("min_females", sprintf("Need at least %d females in the field (have %d).", minf, Ftot))
  maxm <- f$max_males
  if (!is.null(maxm) && !is.na(maxm) && Mtot > maxm)
    add("max_males", sprintf("No more than %d males in the field (have %d).", maxm, Mtot))

  tiers <- f$tiers %||% list()
  if (length(tiers) > 0) {
    thr <- vapply(tiers, function(t) as.integer(t$females), integer(1))
    ord <- order(thr); tiers <- tiers[ord]; thr <- thr[ord]
    hits <- which(thr <= Ftot)
    tier <- if (length(hits)) tiers[[max(hits)]] else tiers[[1]]
    if (n_of < as.integer(tier$outfield))
      add("outfield_min", sprintf("Need at least %d females in the outfield (have %d).", tier$outfield, n_of))
    if (n_if < as.integer(tier$infield))
      add("infield_min", sprintf("Need at least %d females in the infield (have %d).", tier$infield, n_if))
    if (identical(tier$battery, "one")) {
      ppos <- Filter(function(p) identical(as.character(p$position), "P"), fielders)
      cpos <- Filter(function(p) identical(as.character(p$position), "C"), fielders)
      if (length(ppos) && length(cpos) && identical(ppos[[1]]$gender, cpos[[1]]$gender))
        add("battery_opposite", "Pitcher and catcher must be opposite genders.")
    }
  }
  viol
}
```

- [ ] **Step 4: Extend the `fielding` config in `default_ruleset_config` and `coerce_ruleset_config`.**

In `default_ruleset_config`, change the `fielding` line to include the new fields:
```r
    fielding = list(min_females = 0L, max_males = NA_integer_, tiers = list(),
                    position_requirements = list()),
```
In `coerce_ruleset_config`, after `d$fielding$min_females <- as.integer(d$fielding$min_females)` add:
```r
  d$fielding$max_males <- .as_int_or_na(d$fielding$max_males)
  d$fielding$tiers <- d$fielding$tiers %||% list()
```

- [ ] **Step 5: Run fielding + rules tests, expect PASS**

Run: `"$RSCRIPT" tests/test_fielding.R && "$RSCRIPT" tests/test_rules_engine.R && "$RSCRIPT" tests/test_rules_eval.R`
Expected: all pass (the old `fielding_warnings` test in `test_rules_eval.R` is untouched and still passes).

- [ ] **Step 6: Commit**

```bash
git add R/rules_engine.R tests/test_fielding.R
git commit -m "feat: configurable coed fielding gender-balance engine with tiers"
```

---

## Task 4: `half_runs` event type + validation

**Files:**
- Modify: `R/game_events.R` (`EVENT_TYPES`, `validate_event`)
- Test: `tests/test_game_events.R`

**Interfaces:**
- Produces: `"half_runs"` in `EVENT_TYPES`; `validate_event` checks `team ∈ {home,away}` and non-negative integer `runs`.

- [ ] **Step 1: Add tests to `tests/test_game_events.R`**

```r
test_that("half_runs event validates team and runs", {
  ok <- new_event("half_runs", list(team = "home", runs = 3L))
  expect_true(validate_event(ok)$ok)
  expect_false(validate_event(new_event("half_runs", list(team = "nobody", runs = 1L)))$ok)
  expect_false(validate_event(new_event("half_runs", list(team = "home", runs = -1L)))$ok)
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `"$RSCRIPT" tests/test_game_events.R`

- [ ] **Step 3: Edit `R/game_events.R`**

(a) Add `"half_runs"` to `EVENT_TYPES`:
```r
EVENT_TYPES <- c("game_start", "plate_appearance", "substitution",
                 "count_override", "inning_end", "half_runs")
```
(b) In `validate_event`, before the final `list(ok=...)`, add:
```r
  if (identical(evt$type, "half_runs")) {
    if (!isTRUE(evt$payload$team %in% c("home", "away"))) add("half_runs needs team home/away")
    r <- evt$payload$runs
    if (is.null(r) || !is.numeric(r) || r < 0) add("half_runs needs non-negative runs")
  }
```

- [ ] **Step 4: Run test, expect PASS**

Run: `"$RSCRIPT" tests/test_game_events.R`

- [ ] **Step 5: Commit**

```bash
git add R/game_events.R tests/test_game_events.R
git commit -m "feat: half_runs event type and validation"
```

---

## Task 5: `half_runs` reducer branch

**Files:**
- Modify: `R/game_reducer.R` (`apply_event`)
- Test: `tests/test_reducer_halfruns.R` (new)

**Interfaces:**
- Consumes: `apply_run_cap`, `advance_half`, `.refresh_flags`.
- Produces: `apply_event` handles `half_runs` — adds capped runs to the batting team's score + `runs_this_half`, then advances the half.

- [ ] **Step 1: Write `tests/test_reducer_halfruns.R`**

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

start_evt <- function(ruleset = default_ruleset_config()) new_event("game_start", list(
  ruleset = ruleset, first_bat = "away",
  home = list(team_id="H", name="Home", lineup = list()),   # empty -> run-only
  away = list(team_id="A", name="Away", lineup = list())
), seq = 1L)

test_that("half_runs adds runs to the batting team and advances the half", {
  s <- fold_events(list(start_evt(),
    new_event("half_runs", list(team = "away", runs = 3L), seq = 2L)))
  expect_equal(s$score$away, 3L)
  expect_equal(s$half, "bottom")           # away's top half ended
  expect_equal(s$batting_team, "home")
  expect_equal(s$line_score$away, 3L)
})

test_that("half_runs respects the run cap", {
  rs <- default_ruleset_config(); rs$run_cap_per_inning <- 5L; rs$open_last_inning <- TRUE
  s <- fold_events(list(start_evt(rs),
    new_event("half_runs", list(team = "away", runs = 9L), seq = 2L)))
  expect_equal(s$score$away, 5L)           # capped (inning 1, not the open last inning)
})
```

- [ ] **Step 2: Run test, expect FAIL** (`half_runs` falls through to the no-op tail of `apply_event`)

Run: `"$RSCRIPT" tests/test_reducer_halfruns.R`

- [ ] **Step 3: Add the branch to `apply_event` in `R/game_reducer.R`** — insert before the final `if (type == "substitution")` line:

```r
  if (type == "half_runs") {
    team <- state$batting_team
    runs <- as.integer(evt$payload$runs %||% 0L)
    capped <- apply_run_cap(state$ruleset, state$runs_this_half + runs, state$inning) - state$runs_this_half
    runs <- max(0L, capped)
    state$score[[team]] <- state$score[[team]] + runs
    state$runs_this_half <- state$runs_this_half + runs
    return(.refresh_flags(advance_half(state)))
  }
```

- [ ] **Step 4: Run test + regressions, expect PASS**

Run: `"$RSCRIPT" tests/test_reducer_halfruns.R && "$RSCRIPT" tests/test_reducer_core.R && "$RSCRIPT" tests/test_reducer_pa.R`

- [ ] **Step 5: Commit**

```bash
git add R/game_reducer.R tests/test_reducer_halfruns.R
git commit -m "feat: half_runs reducer branch (run-only halves)"
```

---

## Task 6: Structured warnings; wire fielding + gender + notices into `.refresh_flags`

**Files:**
- Modify: `R/game_reducer.R` (`initial_game_state` warnings init; `.refresh_flags`)
- Modify: `R/rules_engine.R` (remove now-unused `fielding_warnings`)
- Modify: `tests/test_rules_eval.R` (replace the `fielding_warnings` test and update the gender-warning assertion to the structured shape)
- Test: `tests/test_reducer_warnings.R` (new)

**Interfaces:**
- Produces: `state$warnings` is a **list** of `list(severity, code, message)` items. `.refresh_flags` assembles: fielding violations (via `evaluate_fielding` on the defensive team), a `batting_gender` violation for the batter due up, and `notice` items for run-cap-reached, game-final, and batting-size mismatch.
- Consumes: `evaluate_fielding`, `next_batter_gender_ok`, `game_should_end`, `apply_run_cap` context.

- [ ] **Step 1: Write `tests/test_reducer_warnings.R`**

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

mk <- function(prefix, genders, positions = NA_character_) {
  positions <- rep(positions, length.out = length(genders))
  lapply(seq_along(genders), function(i)
    make_player(paste0(prefix,i), paste(prefix,i), genders[i], i, i, positions[i]))
}
has_code <- function(w, code) any(vapply(w, function(x) identical(x$code, code), logical(1)))

test_that("warnings is a list of structured items and flags batting gender order", {
  cfg <- coerce_ruleset_config(list(batting_gender_rule = list(type = "no_two_males_consecutive")))
  lu <- mk("m", c("M","M"))
  s <- fold_events(list(
    new_event("game_start", list(ruleset = cfg, first_bat = "away",
      home = list(team_id="H", name="H", lineup = lu),
      away = list(team_id="A", name="A", lineup = lu)), seq = 1L),
    new_event("plate_appearance", list(team="away", batter_id="m1", outcome="1B",
      reached=1L, rbi=0L, outs_on_play=0L, advances=list()), seq = 2L)))
  expect_true(is.list(s$warnings))
  expect_true(has_code(s$warnings, "batting_gender"))
  expect_true(all(vapply(s$warnings, function(x) x$severity %in% c("violation","notice"), logical(1))))
})

test_that("a fielding violation surfaces as a warning item during defense", {
  cfg <- coerce_ruleset_config(list(fielding = STANDARD_COED_FIELDING))
  # away bats (top); home is the defense with an all-male positioned defense.
  # Fold a plate appearance too: game_start does NOT run .refresh_flags, so a
  # non-game_start event is needed for warnings to compute.
  home <- mk("h", rep("M", 4), positions = c("P","C","SS","LF"))
  away <- mk("a", c("M","F"))
  s <- fold_events(list(
    new_event("game_start", list(ruleset = cfg, first_bat = "away",
      home = list(team_id="H", name="H", lineup = home),
      away = list(team_id="A", name="A", lineup = away)), seq = 1L),
    new_event("plate_appearance", list(team="away", batter_id="a1", outcome="1B",
      reached=1L, rbi=0L, outs_on_play=0L, advances=list()), seq = 2L)))
  expect_true(has_code(s$warnings, "min_females"))  # 0 females on the home defense
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `"$RSCRIPT" tests/test_reducer_warnings.R`

- [ ] **Step 3: Migrate warnings init.** In `R/game_reducer.R` `initial_game_state`, change `warnings = character()` to `warnings = list()`.

- [ ] **Step 4: Replace `.refresh_flags` in `R/game_reducer.R`** with the structured version:

```r
.refresh_flags <- function(state) {
  cfg <- state$ruleset
  def_team <- if (identical(state$batting_team, "away")) "home" else "away"
  w <- evaluate_fielding(cfg, state$lineups[[def_team]])  # list of violation items

  if (!is.null(state$current_batter)) {
    bt <- state$batting_team
    prev_genders <- vapply(
      Filter(function(r) identical(r$team, bt), state$pa_log),
      function(r) {
        pl <- Filter(function(p) identical(p$player_id, r$batter_id), state$lineups[[bt]])
        if (length(pl)) pl[[1]]$gender else NA_character_
      }, character(1))
    prev_genders <- prev_genders[!is.na(prev_genders)]
    if (!next_batter_gender_ok(cfg, prev_genders, state$current_batter$gender)) {
      w <- c(w, list(list(severity = "violation", code = "batting_gender",
        message = "Batting order: the batter due up violates the gender rule.")))
    }
  }

  cap <- cfg$run_cap_per_inning
  if (!is.na(cap) && !(isTRUE(cfg$open_last_inning) && state$inning >= cfg$innings) &&
      state$runs_this_half >= cap) {
    w <- c(w, list(list(severity = "notice", code = "run_cap",
      message = sprintf("Run cap of %d reached this inning.", cap))))
  }

  bs <- cfg$batting_size
  if (!is.na(bs)) {
    n_bat <- length(Filter(function(p) !is.na(p$order_slot), state$lineups[[state$batting_team]]))
    if (n_bat > 0 && n_bat != bs) {
      w <- c(w, list(list(severity = "notice", code = "batting_size",
        message = sprintf("Batting team has %d batters; rule expects %d.", n_bat, bs))))
    }
  }

  if (game_should_end(cfg, state)) {
    state$status <- "final"
    w <- c(w, list(list(severity = "notice", code = "final", message = "Game is final.")))
  }

  state$warnings <- w
  state
}
```

- [ ] **Step 5: Remove the now-unused `fielding_warnings` from `R/rules_engine.R`** (delete the whole `fielding_warnings <- function(...) {...}` block, lines 72–80 in the current file).

- [ ] **Step 6: Update `tests/test_rules_eval.R`.**

(a) Delete the `test_that("fielding_warnings triggers below min_females", {...})` block (the function is gone; `evaluate_fielding` is covered in `tests/test_fielding.R`).

(b) Replace the body of `test_that("reducer surfaces a gender-order warning for the batter due up", {...})` assertion line — change the final `expect_true(any(grepl("gender", s$warnings, ignore.case = TRUE)))` to:
```r
  expect_true(any(vapply(s$warnings, function(x) identical(x$code, "batting_gender"), logical(1))))
```

- [ ] **Step 7: Run the reducer/rules suites, expect PASS**

Run: `"$RSCRIPT" tests/test_reducer_warnings.R && "$RSCRIPT" tests/test_rules_eval.R && "$RSCRIPT" tests/test_reducer_core.R && "$RSCRIPT" tests/test_reducer_pa.R && "$RSCRIPT" tests/test_reducer_subs.R && "$RSCRIPT" tests/test_reducer_halfruns.R`
Expected: all pass.

- [ ] **Step 8: Commit**

```bash
git add R/game_reducer.R R/rules_engine.R tests/test_rules_eval.R tests/test_reducer_warnings.R
git commit -m "feat: structured warnings; wire fielding/gender/notices into refresh_flags"
```

---

## Task 7: Setup pure helpers — `collect_lineup`, `collect_ruleset`, empty/variable lineups

**Files:**
- Modify: `R/setup_module.R` (add `collect_lineup`, `collect_ruleset`; keep `build_game_start_event` accepting empty/variable lineups)
- Test: `tests/test_setup_module.R`

**Interfaces:**
- Produces:
  - `collect_lineup(input, prefix, row_ids)` → list of `make_player(...)`, one per row id, reading `input[[paste0(prefix,"_name_",id)]]`, `_gender_`, `_jersey_`, `_pos_`; skips rows whose name is blank; `order_slot` = position in the kept sequence; jersey blank ⇒ `0L`; position blank ⇒ `NA_character_`.
  - `collect_ruleset(input)` → a coerced ruleset list from the setup inputs (starting count, foul, gender rule + n, batting_size, innings, run cap, mercy, and `fielding` from the preset/knobs).
- Consumes: `make_player`, `coerce_ruleset_config`, `STANDARD_COED_FIELDING`.

- [ ] **Step 1: Add tests to `tests/test_setup_module.R`**

```r
test_that("collect_lineup reads rows, skips blanks, assigns order_slot", {
  input <- list(
    t_name_1 = "Sam", t_gender_1 = "F", t_jersey_1 = 9, t_pos_1 = "SS",
    t_name_2 = "",    t_gender_2 = "M", t_jersey_2 = NA, t_pos_2 = "",   # blank name -> skipped
    t_name_3 = "Mo",  t_gender_3 = "M", t_jersey_3 = NA, t_pos_3 = ""    # blank jersey -> 0
  )
  lu <- collect_lineup(input, "t", c(1,2,3))
  expect_equal(length(lu), 2L)
  expect_equal(lu[[1]]$name, "Sam"); expect_equal(lu[[1]]$order_slot, 1L)
  expect_equal(lu[[1]]$position, "SS"); expect_equal(lu[[1]]$jersey_number, 9L)
  expect_equal(lu[[2]]$name, "Mo"); expect_equal(lu[[2]]$order_slot, 2L)
  expect_equal(lu[[2]]$jersey_number, 0L); expect_true(is.na(lu[[2]]$position))
})

test_that("collect_lineup returns an empty list when no rows have names", {
  expect_equal(length(collect_lineup(list(), "t", integer())), 0L)
})

test_that("build_game_start_event accepts an empty lineup (run-only team)", {
  home <- list(team_id="H", name="Home", lineup = list())  # empty
  away <- list(team_id="A", name="Away", lineup = list(make_player("a1","A1","F",1L,1L,"SS")))
  evt <- build_game_start_event(default_ruleset_config(), home, away, "away")
  expect_true(validate_event(evt)$ok)
  expect_equal(length(evt$payload$home$lineup), 0L)
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `"$RSCRIPT" tests/test_setup_module.R`

- [ ] **Step 3: Add helpers to `R/setup_module.R`** (above `setup_ui`)

```r
collect_lineup <- function(input, prefix, row_ids) {
  players <- list()
  for (id in row_ids) {
    nm <- input[[paste0(prefix, "_name_", id)]] %||% ""
    nm <- trimws(nm)
    if (!nzchar(nm)) next
    jersey <- input[[paste0(prefix, "_jersey_", id)]]
    jersey <- if (is.null(jersey) || is.na(jersey)) 0L else as.integer(jersey)
    pos <- input[[paste0(prefix, "_pos_", id)]] %||% ""
    pos <- if (!nzchar(pos)) NA_character_ else pos
    slot <- length(players) + 1L
    players[[slot]] <- make_player(uuid::UUIDgenerate(), nm,
      input[[paste0(prefix, "_gender_", id)]] %||% "M",
      jersey_number = jersey, order_slot = slot, position = pos)
  }
  players
}

collect_ruleset <- function(input) {
  fielding <- switch(input$fielding_preset %||% "none",
    "standard_coed" = STANDARD_COED_FIELDING,
    "custom" = list(
      min_females = input$min_females %||% 0L,
      max_males = if ((input$max_males %||% 0) > 0) input$max_males else NA_integer_,
      tiers = list(list(females = 0L,
        outfield = input$of_females %||% 0L, infield = input$if_females %||% 0L,
        battery = input$battery_mode %||% "any")),
      position_requirements = list()),
    list(min_females = 0L, max_males = NA_integer_, tiers = list(), position_requirements = list()))
  coerce_ruleset_config(list(
    starting_count = list(balls = input$start_balls, strikes = input$start_strikes),
    foul_out_rule = input$foul_out,
    batting_gender_rule = list(type = input$gender_rule, n = input$gender_n),
    batting_size = if ((input$batting_size %||% 0) > 0) input$batting_size else NA_integer_,
    fielding = fielding,
    innings = input$innings,
    run_cap_per_inning = if ((input$run_cap %||% 0) > 0) input$run_cap else NA_integer_,
    mercy_rule = list(differential = if ((input$mercy_diff %||% 0) > 0) input$mercy_diff else NA_integer_,
                      after_inning = 1L)))
}
```

- [ ] **Step 4: Run test, expect PASS**

Run: `"$RSCRIPT" tests/test_setup_module.R`

- [ ] **Step 5: Commit**

```bash
git add R/setup_module.R tests/test_setup_module.R
git commit -m "feat: setup pure helpers collect_lineup/collect_ruleset; empty lineups allowed"
```

---

## Task 8: Setup UI — accordion, dynamic lineup editors, fielding subsection

**Files:**
- Modify: `R/setup_module.R` (`setup_ui`, `setup_server`; drop the 4-player stub)
- Test: manual `testServer` + boot check (dynamic-row UI is not unit-testable in isolation; the pure helpers from Task 7 carry the logic)

**Interfaces:**
- Consumes: `collect_lineup`, `collect_ruleset`, `build_game_start_event`, `APP_CONFIG$positions`.
- Produces: `setup_ui(id)` / `setup_server(id)` returning the `game_start` reactive (unchanged signature). Each team lineup uses row ids from a per-team `reactiveVal` counter; positions come from `APP_CONFIG$positions` (blank option first).

- [ ] **Step 1: Rewrite `setup_ui` in `R/setup_module.R`**

```r
.lineup_ui <- function(ns, prefix, title) {
  tagList(
    tags$h5(title),
    tags$p(class = "text-muted small",
      "Leave this lineup empty to just record this team's runs each inning."),
    tags$div(id = ns(paste0(prefix, "_rows"))),
    actionButton(ns(paste0(prefix, "_add")), "Add player", class = "btn-sm btn-outline-secondary")
  )
}

setup_ui <- function(id) {
  ns <- NS(id)
  pos_choices <- c("(no position)" = "", stats::setNames(APP_CONFIG$positions, APP_CONFIG$positions))
  tagList(
    tags$h3("New game"),
    textInput(ns("away_name"), "Away team", "Away"),
    .lineup_ui(ns, "away", "Away lineup"),
    textInput(ns("home_name"), "Home team", "Home"),
    .lineup_ui(ns, "home", "Home lineup"),
    accordion(open = FALSE,
      accordion_panel("Rules",
        layout_columns(col_widths = c(6,6),
          numericInput(ns("start_balls"), "Starting balls", 1, 0, 3),
          numericInput(ns("start_strikes"), "Starting strikes", 1, 0, 2)),
        selectInput(ns("foul_out"), "Foul with 2 strikes",
          c("Out" = "out", "One courtesy foul" = "one_courtesy_foul",
            "Unlimited (never an out)" = "unlimited")),
        selectInput(ns("batting_size"), "Number of batters",
          c("Unlimited (everyone bats)" = "0", "9" = "9", "10" = "10")),
        selectInput(ns("gender_rule"), "Batting gender rule",
          c("None" = "none", "No two males in a row" = "no_two_males_consecutive",
            "Every other" = "every_other", "At least one F every N" = "every_n")),
        conditionalPanel(sprintf("input['%s'] == 'every_n'", ns("gender_rule")),
          numericInput(ns("gender_n"), "N (for 'every N')", 2, 2, 12)),
        layout_columns(col_widths = c(4,4,4),
          numericInput(ns("innings"), "Innings", 7, 1, 12),
          numericInput(ns("run_cap"), "Run cap/inning (0 = none)", 0, 0, 30),
          numericInput(ns("mercy_diff"), "Mercy differential (0 = none)", 0, 0, 50))),
      accordion_panel("Fielding gender rules",
        selectInput(ns("fielding_preset"), "Preset",
          c("None" = "none", "Standard coed (10-player)" = "standard_coed", "Custom" = "custom")),
        conditionalPanel(sprintf("input['%s'] == 'custom'", ns("fielding_preset")),
          layout_columns(col_widths = c(6,6),
            numericInput(ns("min_females"), "Min females in field", 0, 0, 10),
            numericInput(ns("max_males"), "Max males in field (0 = none)", 0, 0, 12)),
          layout_columns(col_widths = c(4,4,4),
            numericInput(ns("of_females"), "Min F outfield", 0, 0, 5),
            numericInput(ns("if_females"), "Min F infield", 0, 0, 5),
            selectInput(ns("battery_mode"), "Pitcher/Catcher",
              c("Any" = "any", "Opposite genders" = "one")))))
    ),
    actionButton(ns("start"), "Start game", class = "btn-primary bw-outcome-btn")
  )
}

.player_row <- function(ns, prefix, id) {
  pos_choices <- c("(pos)" = "", stats::setNames(APP_CONFIG$positions, APP_CONFIG$positions))
  tags$div(class = "d-flex gap-1 align-items-end mb-1", id = ns(paste0(prefix, "_row_", id)),
    textInput(ns(paste0(prefix, "_name_", id)), NULL, placeholder = "Name"),
    radioButtons(ns(paste0(prefix, "_gender_", id)), NULL, c("M","F"), inline = TRUE),
    numericInput(ns(paste0(prefix, "_jersey_", id)), NULL, value = NA, min = 0, max = 99),
    selectInput(ns(paste0(prefix, "_pos_", id)), NULL, pos_choices),
    actionButton(ns(paste0(prefix, "_del_", id)), "×", class = "btn-sm btn-outline-danger"))
}
```

- [ ] **Step 2: Rewrite `setup_server` in `R/setup_module.R`** to manage dynamic rows and assemble via the Task-7 helpers:

```r
setup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    game_start <- reactiveVal(NULL)
    rows <- list(away = reactiveVal(integer()), home = reactiveVal(integer()))
    counter <- reactiveVal(0L)

    add_row <- function(prefix) {
      counter(counter() + 1L); id <- counter()
      rows[[prefix]](c(rows[[prefix]](), id))
      insertUI(sprintf("#%s", ns(paste0(prefix, "_rows"))), where = "beforeEnd",
        ui = .player_row(ns, prefix, id))
      observeEvent(input[[paste0(prefix, "_del_", id)]], {
        removeUI(sprintf("#%s", ns(paste0(prefix, "_row_", id))))
        rows[[prefix]](setdiff(rows[[prefix]](), id))
      }, ignoreInit = TRUE, once = TRUE)
    }
    observeEvent(input$away_add, add_row("away"), ignoreInit = TRUE)
    observeEvent(input$home_add, add_row("home"), ignoreInit = TRUE)

    observeEvent(input$start, {
      cfg <- collect_ruleset(input)
      away <- list(team_id = uuid::UUIDgenerate(), name = input$away_name,
                   lineup = collect_lineup(input, "away", rows$away()))
      home <- list(team_id = uuid::UUIDgenerate(), name = input$home_name,
                   lineup = collect_lineup(input, "home", rows$home()))
      game_start(build_game_start_event(cfg, home, away, "away"))
    })
    game_start
  })
}
```

- [ ] **Step 3: `testServer` smoke test** — add to `tests/test_setup_module.R`:

```r
test_that("setup_server produces a game_start with a run-only home team", {
  library(shiny)
  testServer(setup_server, {
    session$setInputs(away_add = 1)                 # one away row
    # Fill the away row (id 1), leave home empty, set required rule inputs, start.
    session$setInputs(away_name_1 = "Sam", away_gender_1 = "F", away_jersey_1 = 9, away_pos_1 = "SS",
      away_name = "Away", home_name = "Home", start_balls = 1, start_strikes = 1,
      foul_out = "out", batting_size = "0", gender_rule = "none", innings = 7,
      run_cap = 0, mercy_diff = 0, fielding_preset = "none", start = 1)
    gs <- session$returned()
    expect_equal(gs$type, "game_start")
    expect_equal(length(gs$payload$away$lineup), 1L)
    expect_equal(length(gs$payload$home$lineup), 0L)   # run-only home
  })
})
```

- [ ] **Step 4: Run the setup test, expect PASS**

Run: `"$RSCRIPT" tests/test_setup_module.R`
Expected: pass. If the dynamic-row `insertUI` prevents the `testServer` from seeing `away_name_1`, set the row inputs directly as above (testServer records set inputs regardless of `insertUI`); the assertion relies on `collect_lineup` reading `away_name_1` from the input map, which the test sets explicitly.

- [ ] **Step 5: Boot check (headless, non-blocking)**

Run:
```bash
"$RSCRIPT" -e "source('global.R'); a <- shinyApp(bookworm_ui(), bookworm_server); stopifnot(inherits(a,'shiny.appobj')); cat('app object OK\n')"
```
Expected: `app object OK`, no errors (confirms `accordion`/`conditionalPanel`/inputs construct).

- [ ] **Step 6: Commit**

```bash
git add R/setup_module.R tests/test_setup_module.R
git commit -m "feat: setup UI — accordion, dynamic lineup editors, batting size, fielding presets"
```

---

## Task 9: Tracking — run-only branch + `record_half_runs_event`

**Files:**
- Modify: `R/tracking_module.R` (`record_half_runs_event`, `tracking_ui`, `tracking_server`)
- Test: `tests/test_tracking_module.R`

**Interfaces:**
- Consumes: `new_event`, `fold_events`, storage interface.
- Produces:
  - `record_half_runs_event(state, runs)` → `new_event("half_runs", list(team = state$batting_team, runs = int))`.
  - `tracking_ui`/`tracking_server` show a run-only panel (numeric "Runs this inning" + "End half-inning") when the batting team's lineup is empty, else the outcome grid.

- [ ] **Step 1: Add tests to `tests/test_tracking_module.R`**

```r
test_that("record_half_runs_event targets the batting team", {
  s <- list(batting_team = "home")
  e <- record_half_runs_event(s, 4)
  expect_equal(e$type, "half_runs")
  expect_equal(e$payload$team, "home")
  expect_equal(e$payload$runs, 4L)
})

test_that("run-only half: entering runs advances the half via storage", {
  library(shiny)
  for (f in c("storage.R")) source(file.path("R", f))
  st <- make_storage("guest"); gid <- st$create_game(list(name = "T"))
  gs <- new_event("game_start", list(ruleset = default_ruleset_config(), first_bat = "away",
    home = list(team_id="H", name="Home", lineup = list()),
    away = list(team_id="A", name="Away", lineup = list())), seq = 1L)  # both run-only
  testServer(tracking_server, args = list(storage = st, game_id = gid, game_start_event = gs), {
    session$flushReact()
    session$setInputs(half_runs_n = 3, half_runs_go = 1)
    s <- state()
    expect_equal(s$score$away, 3L)
    expect_equal(s$half, "bottom")
  })
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `"$RSCRIPT" tests/test_tracking_module.R`

- [ ] **Step 3: Add `record_half_runs_event` and the run-only UI branch to `R/tracking_module.R`.**

(a) Add the helper near the top (after `.OUT_OUTCOMES`):
```r
record_half_runs_event <- function(state, runs) {
  new_event("half_runs", list(team = state$batting_team, runs = as.integer(runs %||% 0L)))
}

.batting_team_has_lineup <- function(state) {
  lu <- state$lineups[[state$batting_team]]
  length(Filter(function(p) !is.na(p$order_slot), lu)) > 0
}
```

(b) In `tracking_ui`, replace the fixed outcome grid block with a `uiOutput` that the server fills based on the batting team, plus add the run-only inputs. Change the `tagList(...)` body to:
```r
  tagList(
    uiOutput(ns("situation")),
    uiOutput(ns("action_panel")),
    div(class = "d-flex gap-2 mt-2",
        actionButton(ns("undo"), "Undo", class = "btn-warning"),
        actionButton(ns("sub"), "Substitution", class = "btn-outline-secondary")),
    navset_tab(
      nav_panel("Scorebook", uiOutput(ns("scorebook"))),
      nav_panel("Box score", tableOutput(ns("box_away")), tableOutput(ns("box_home"))))
  )
```

(c) In `tracking_server`, add the `action_panel` renderer and a `half_runs` handler. Insert after the existing `record`/outcome observers:
```r
    output$action_panel <- renderUI({
      s <- state()
      if (.batting_team_has_lineup(s)) {
        outcomes <- c("1B","2B","3B","HR","BB","K","GO","FO","FC","E")
        btns <- lapply(outcomes, function(o)
          actionButton(session$ns(paste0("o_", o)), o, class = "btn-outline-primary bw-outcome-btn"))
        div(class = "bw-outcome-grid d-grid",
            style = "grid-template-columns: repeat(5,1fr); gap:.5rem;", !!!btns)
      } else {
        div(class = "p-2",
          tags$p(class = "text-muted small",
            sprintf("%s is tracked by runs only (no lineup entered).", s$batting_team)),
          numericInput(session$ns("half_runs_n"), "Runs this inning", value = 0, min = 0, max = 50),
          actionButton(session$ns("half_runs_go"), "End half-inning", class = "btn-primary bw-outcome-btn"))
      }
    })

    observeEvent(input$half_runs_go, {
      s <- isolate(state())
      if (identical(s$status, "final")) return(invisible())
      evt <- record_half_runs_event(s, input$half_runs_n)
      appended <- storage$append_event(game_id, evt)
      events(c(isolate(events()), list(appended)))
      storage$save_snapshot(game_id, isolate(state()))
    }, ignoreInit = TRUE)
```

(Keep the existing `outcomes`/`record` observers; the outcome buttons are now rendered by `action_panel` but their ids (`o_1B` …) and the observers still match.)

- [ ] **Step 4: Run test + regressions, expect PASS**

Run: `"$RSCRIPT" tests/test_tracking_module.R && "$RSCRIPT" tests/test_app_flow.R`

- [ ] **Step 5: Commit**

```bash
git add R/tracking_module.R tests/test_tracking_module.R
git commit -m "feat: run-only half tracking in the tracking module"
```

---

## Task 10: Tracking — surface warnings as modals (violations) and toasts (notices)

**Files:**
- Modify: `R/tracking_module.R` (`tracking_server`: warning observer + a pure `partition_warnings` helper)
- Test: `tests/test_tracking_module.R`

**Interfaces:**
- Produces: `partition_warnings(warnings)` → `list(violations = <chr messages>, notices = <chr messages>)`. `tracking_server` observes `state()$warnings`, shows a `modalDialog` listing violations (only when the set of violation codes changes) and `showNotification` for each new notice.

- [ ] **Step 1: Add a test to `tests/test_tracking_module.R`**

```r
test_that("partition_warnings splits violations and notices", {
  w <- list(
    list(severity = "violation", code = "min_females", message = "Need 4 F"),
    list(severity = "notice", code = "run_cap", message = "cap reached"))
  p <- partition_warnings(w)
  expect_equal(p$violations, "Need 4 F")
  expect_equal(p$notices, "cap reached")
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `"$RSCRIPT" tests/test_tracking_module.R`

- [ ] **Step 3: Add `partition_warnings` + the observer to `R/tracking_module.R`.**

(a) Helper (near the top):
```r
partition_warnings <- function(warnings) {
  warnings <- warnings %||% list()
  msg <- function(sev) unlist(lapply(warnings,
    function(x) if (identical(x$severity, sev)) x$message else NULL), use.names = FALSE) %||% character()
  list(violations = msg("violation"), notices = msg("notice"))
}
.violation_codes <- function(warnings) sort(unlist(lapply(warnings,
  function(x) if (identical(x$severity, "violation")) x$code else NULL), use.names = FALSE) %||% character())
```

(b) In `tracking_server`, after the `state <- reactive(...)` line, add the surfacing observer:
```r
    shown_violation_sig <- reactiveVal("")
    shown_notice_codes <- reactiveVal(character())
    observeEvent(state()$warnings, {
      w <- state()$warnings
      p <- partition_warnings(w)
      sig <- paste(.violation_codes(w), collapse = "|")
      if (length(p$violations) && !identical(sig, shown_violation_sig())) {
        showModal(modalDialog(title = "Rule violation",
          tags$ul(!!!lapply(p$violations, tags$li)),
          easyClose = TRUE, footer = modalButton("Got it")))
      }
      shown_violation_sig(sig)
      new_notice_codes <- setdiff(
        vapply(Filter(function(x) identical(x$severity,"notice"), w), function(x) x$code, character(1)),
        shown_notice_codes())
      for (m in p$notices) showNotification(m, type = "message", duration = 4)
      shown_notice_codes(union(shown_notice_codes(),
        vapply(Filter(function(x) identical(x$severity,"notice"), w), function(x) x$code, character(1))))
    }, ignoreInit = FALSE)
```

(The `sig` de-dup ensures a standing violation isn't re-modaled every pitch; it re-shows only when the set of violation codes changes.)

- [ ] **Step 4: Run test + regressions, expect PASS**

Run: `"$RSCRIPT" tests/test_tracking_module.R && "$RSCRIPT" tests/test_app_flow.R`

- [ ] **Step 5: Commit**

```bash
git add R/tracking_module.R tests/test_tracking_module.R
git commit -m "feat: surface rule warnings as modals (violations) and toasts (notices)"
```

---

## Task 11: Full-suite gate + README update

**Files:**
- Modify: `README.md` (known-limitations / roadmap updates)
- Verify: `run_tests.R` over all suites

**Interfaces:** none.

- [ ] **Step 1: Run the full suite, expect exit 0**

Run: `"$RSCRIPT" run_tests.R`
Expected: every `tests/test_*.R` prints and no `[ FAIL > 0 ]`; exit status 0. (Suites now include `test_fielding.R`, `test_reducer_halfruns.R`, `test_reducer_warnings.R`.)

- [ ] **Step 2: Update `README.md`** — under "Known slice-one limitations", replace the "default 4-batter lineup" bullet and add the slice-1.1 state:

```markdown
## Known limitations
- Undo reverts in-session; persisted event rows are pruned in a later phase.
- Lineups are entered per game (no cross-game roster persistence yet — Phase 3).
- Row-Level Security is defined but not enforced (app-level owner scoping).
- Fielding gender tiers are configurable via presets + base knobs; full per-tier
  hand-editing in the UI is deferred (the engine supports arbitrary tiers).

## Rules supported
Arbitrary starting count; foul-with-2-strikes (out / one courtesy foul / unlimited);
batting gender order (none / no-two-males / every-other / one-F-every-N); number of
batters (unlimited / 9 / 10); innings, per-inning run cap, mercy; coed fielding
gender balance (min females, max males, per-category minimums, P/C opposite, and
count-specific tiers). A team with an empty lineup is tracked by runs per inning.
```

- [ ] **Step 3: Commit**

```bash
git add README.md
git commit -m "docs: update README for slice 1.1 (lineups, run-only, fielding rules)"
```

---

## Self-Review notes (for the executor)

- **Spec coverage:** config fixes (T2), batting_size (T2), positions+categories (T1), fielding engine + tiers + preset (T3), half_runs event+reducer (T4/T5), structured warnings + wiring (T6), lineup editor helpers + UI + empty lineups (T7/T8), run-only tracking (T9), modal/toast surfacing (T10), full-suite + docs (T11). Every spec section maps to a task.
- **Ordering keeps the suite green:** label positions (T1) don't break the old `fielding_warnings` test (which only checks non-NA position); `evaluate_fielding` is added alongside (T3) and only swapped in — with the old test removed — in T6, where the warning shape also migrates and its test is updated in the same commit.
- **Interface consistency:** `evaluate_fielding`, `.position_category`, `STANDARD_COED_FIELDING`, `collect_lineup`, `collect_ruleset`, `record_half_runs_event`, `partition_warnings`, the `half_runs` event shape, and the structured warning item shape are named identically wherever referenced.
- **Deferred-with-note:** full per-tier UI editing, cross-game roster persistence, and position-requirements-beyond-min-females remain out of scope per the spec's non-goals.
