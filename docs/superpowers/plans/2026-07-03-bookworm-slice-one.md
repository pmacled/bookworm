# Bookworm Slice One Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the first shippable slice of Bookworm — track a softball/baseball game end-to-end under a league's configurable rules, render it as a traditional SVG scorebook with a box score, persist to Supabase (with an ephemeral guest mode), and support substitutions and JSON export/import.

**Architecture:** An append-only event log is the source of truth; a pure R reducer folds events into game state, and the scorebook, box score, and stats are all derived views of that state. UI is mobile-first `bslib`/Shiny modules. Persistence goes through a storage interface with two backends: Supabase (Postgres via `DBI`/`RPostgres`, auth via GoTrue REST) and an in-memory guest backend.

**Tech Stack:** R, Shiny, bslib (Bootstrap 5), httr2, DBI + RPostgres, jsonlite, htmltools (inline SVG), testthat. Structural template: the sister app `../vasper`.

## Global Constraints

- **File layout mirrors vasper:** `app.R` + `global.R` at root; `R/` holds modules/helpers; `www/` holds `css/`, `js/`, `icons/`; `_brand.yml` for theme; `.Renviron.template` for secrets; `manifest.json` for Posit Connect Cloud.
- **`global.R` sources `R/` alphabetically**, with `brand_colors.R` and `app_config.R` sourced first (vasper pattern).
- **Tests follow vasper's flat pattern:** standalone files `tests/test_<area>.R`, each `library(testthat)`, `source()`s the R files under test, uses `describe()`/`test_that()`/`expect_*()`, and is runnable with `Rscript tests/test_<area>.R` from the project root. Do **not** use a `tests/testthat/` package layout.
- **The reducer and rules engine are pure functions:** no Shiny, no DB, no `Sys.time()` inside fold logic (timestamps are passed in). This keeps them unit-testable.
- **Never commit secrets:** `.Renviron` is gitignored (already in `.gitignore`); only `.Renviron.template` is committed.
- **Use `%||%` (null-coalesce)** defined once in `R/app_config.R`; do not redefine it per file.
- **All IDs generated in R are UUID strings** via `uuid::UUIDgenerate()` except DB-assigned `seq` integers.
- **Player gender** is one of `"M"` / `"F"` (string); positions are baseball scoring numbers `1`–`9` plus `"EH"`/`"DH"` where used, and softball adds `10` (rover/short fielder).

---

## Shared Data Shapes (referenced by many tasks)

These shapes are the contract between tasks. Implement them exactly.

**Ruleset config** (stored as `rulesets.config` JSONB; an R named list):

```r
list(
  starting_count = list(balls = 1L, strikes = 1L),
  foul_out_rule  = "out",                 # "out" | "one_courtesy_foul"
  batting_gender_rule = list(              # type drives validation
    type = "none", n = NA_integer_         # "none"|"no_two_males_consecutive"|"every_other"|"every_n"
  ),
  male_walk_rule = "none",                 # "none" | "two_bases_then_female"
  fielding = list(min_females = 0L, position_requirements = list()),
  innings = 7L,
  run_cap_per_inning = NA_integer_,        # NA = no cap
  open_last_inning = TRUE,
  mercy_rule = list(differential = NA_integer_, after_inning = NA_integer_),
  short_lineup_auto_out = FALSE,
  courtesy_runner = FALSE
)
```

**Player** (element of a lineup):

```r
list(player_id = "<uuid>", name = "Sam Lee", gender = "M",
     jersey_number = 7L, order_slot = 1L, position = 6L)  # position NA for bench
```

**Event** (element of the append-only log):

```r
list(seq = 1L, type = "game_start", ts = "<iso8601 or NA>", payload = list(...))
```

Event `type` values and their `payload`:
- `"game_start"`: `list(ruleset = <config>, home = list(team_id, name, lineup=<list of Player>), away = list(...), first_bat = "away")`
- `"plate_appearance"`: `list(team = "away", batter_id, outcome, fielding = "6-3", rbi = 0L, outs_on_play = 1L, reached = NA_integer_, advances = list(<Advance>...))`
  - `outcome` in `c("1B","2B","3B","HR","BB","IBB","K","KL","GO","FO","LO","PO","FC","E","SF","SAC","HBP")`
  - `reached` = base the batter ends on (`1`,`2`,`3`,`4`=scored, `NA`=out)
  - `<Advance>` = `list(runner_id, from = 1L, to = 3L, scored = FALSE, out = FALSE, earned = TRUE)`
- `"substitution"`: `list(team = "home", kind = "batting", out_player_id, in_player = <Player>, order_slot, position = NA)`  (`kind` in `c("batting","defensive","courtesy_runner")`)
- `"count_override"`: `list(balls = 0L, strikes = 2L)`
- `"inning_end"`: `list()`  (manual end; reducer also ends automatically at 3 outs)

**Game state** (output of `fold_events`):

```r
list(
  status = "in_progress",                  # "in_progress" | "final"
  inning = 1L, half = "top",               # "top" | "bottom"
  outs = 0L,
  count = list(balls = 1L, strikes = 1L),
  bases = list(first = NA_character_, second = NA_character_, third = NA_character_),  # runner_ids
  score = list(home = 0L, away = 0L),
  runs_this_half = 0L,
  lineups = list(home = <list of Player>, away = <list of Player>),  # current, subs applied
  batting_index = list(home = 0L, away = 0L),   # 0-based index of next batter
  batting_team = "away",
  current_batter = <Player or NULL>,
  pa_log = list(<one record per plate appearance, for scorebook>),
  line_score = list(home = integer(), away = integer()),  # runs per completed half-inning
  warnings = character(),                  # rule warnings for the *current* situation
  ruleset = <config>
)
```

---

## Task 1: Project scaffold boots to a placeholder screen

**Files:**
- Create: `R/app_config.R`, `R/brand_colors.R`, `_brand.yml`, `global.R`, `app.R`, `.Renviron.template`
- Create: `www/css/app.css`, `www/.gitkeep` under `www/js/` and `www/icons/`
- Test: `tests/test_app_config.R`

**Interfaces:**
- Produces: `%||%` operator; `APP_CONFIG` list (`app_name`, `positions`, `outcome_codes`); `BRAND_COLORS` list.

- [ ] **Step 1: Write `_brand.yml`** (softball palette; mirror vasper's structure)

```yaml
meta:
  name: Bookworm
color:
  palette:
    field-green: "#2E7D32"
    field-green-light: "#e8f5e9"
    clay-orange: "#C2571A"
    clay-orange-light: "#fbe9de"
    chalk: "#F8F9FA"
    ink: "#1F2933"
    danger-red: "#C62828"
    danger-red-light: "#ffebee"
    warning-amber: "#FFA726"
    warning-amber-light: "#fff3e0"
    neutral-gray: "#6C757D"
    paper: "#FFFFFF"
  foreground: ink
  background: paper
  primary: field-green
  secondary: neutral-gray
  success: field-green
  warning: warning-amber
  danger: danger-red
  light: chalk
  dark: ink
typography:
  base: system-ui
  headings:
    family: system-ui
    weight: 600
defaults:
  bootstrap:
    enable-rounded: true
    border-radius: "0.5rem"
```

- [ ] **Step 2: Write `R/brand_colors.R`** — copy vasper's `R/brand_colors.R` verbatim (it is data-driven off `_brand.yml` and needs no changes). It defines `BRAND_COLORS`.

- [ ] **Step 3: Write `R/app_config.R`**

```r
# App configuration and small shared utilities. Sourced early (with brand_colors.R).

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

APP_CONFIG <- list(
  app_name = "Bookworm",
  positions = c(
    "P" = 1L, "C" = 2L, "1B" = 3L, "2B" = 4L, "3B" = 5L,
    "SS" = 6L, "LF" = 7L, "CF" = 8L, "RF" = 9L, "ROVER" = 10L
  ),
  outcome_codes = c(
    "1B", "2B", "3B", "HR", "BB", "IBB", "HBP",
    "K", "KL", "GO", "FO", "LO", "PO", "FC", "E", "SF", "SAC"
  )
)

# Supabase table names (Postgres), one source of truth.
DB_TABLES <- list(
  profiles = "profiles", leagues = "leagues", rulesets = "rulesets",
  teams = "teams", players = "players", games = "games",
  game_events = "game_events", plate_appearances = "plate_appearances"
)
```

- [ ] **Step 4: Write `tests/test_app_config.R`**

```r
library(testthat)
source(file.path("R", "app_config.R"))

test_that("%||% returns fallback for NULL and empty", {
  expect_equal(NULL %||% 5, 5)
  expect_equal(character(0) %||% "x", "x")
  expect_equal(3 %||% 9, 3)
})

test_that("outcome codes include the core outcomes", {
  expect_true(all(c("1B", "HR", "BB", "K", "FC") %in% APP_CONFIG$outcome_codes))
})
```

- [ ] **Step 5: Run the test, expect PASS**

Run: `Rscript tests/test_app_config.R`
Expected: all tests pass (`[ FAIL 0 ...`).

- [ ] **Step 6: Write `global.R`** (package loads + alphabetical source with brand/config first)

```r
library(shiny)
library(bslib)
library(htmltools)
library(jsonlite)
library(DBI)
library(httr2)
library(uuid)

# Source R/ with brand_colors.R and app_config.R first (vasper pattern).
.r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
.r_first <- file.path("R", c("brand_colors.R", "app_config.R"))
invisible(lapply(c(.r_first, setdiff(.r_files, .r_first)), source))
rm(.r_files, .r_first)

app_theme <- bs_theme(version = 5, preset = "shiny", brand = "_brand.yml")
```

- [ ] **Step 7: Write minimal `app.R`**

```r
source("global.R")

ui <- page_fillable(
  title = APP_CONFIG$app_name,
  theme = app_theme,
  tags$head(tags$link(rel = "stylesheet", href = "css/app.css")),
  tags$div(class = "p-4", tags$h2("Bookworm"), tags$p("Scaffold OK."))
)

server <- function(input, output, session) {}

shinyApp(ui, server)
```

- [ ] **Step 8: Write `.Renviron.template`**

```
# Copy to .Renviron and fill in. NEVER commit .Renviron.
# Supabase Postgres connection (Session pooler or direct)
SUPABASE_DB_HOST=your-project.pooler.supabase.com
SUPABASE_DB_PORT=5432
SUPABASE_DB_NAME=postgres
SUPABASE_DB_USER=postgres.your-project-ref
SUPABASE_DB_PASSWORD=your_db_password
# Supabase Auth (GoTrue) REST
SUPABASE_URL=https://your-project.supabase.co
SUPABASE_ANON_KEY=your_anon_key
```

- [ ] **Step 9: Write `www/css/app.css`** (mobile-first base; large tap targets)

```css
:root { --tap: 3rem; }
.bw-outcome-btn { min-height: var(--tap); font-size: 1.1rem; font-weight: 600; }
.bw-scorebook { overflow-x: auto; }
@media (max-width: 576px) { .bw-outcome-grid { gap: .4rem; } }
```

- [ ] **Step 10: Verify the app boots**

Run: `Rscript -e "shiny::runApp('.', launch.browser = FALSE, port = 8100)"` for ~5s, confirm no source/parse errors in console (Ctrl-C to stop). Expected: "Listening on http://...:8100" with no errors.

- [ ] **Step 11: Commit**

```bash
git add -A
git commit -m "feat: scaffold bookworm app (config, brand, boot screen)"
```

---

## Task 2: Ruleset config defaults + validation

**Files:**
- Create: `R/rules_engine.R`
- Test: `tests/test_rules_engine.R`

**Interfaces:**
- Consumes: `%||%` from `R/app_config.R`.
- Produces:
  - `default_ruleset_config()` → the Shared-Data-Shapes ruleset list.
  - `validate_ruleset_config(cfg)` → `list(ok = TRUE/FALSE, errors = character())`.
  - `coerce_ruleset_config(cfg)` → merges a partial user config over defaults, coercing integer fields.

- [ ] **Step 1: Write `tests/test_rules_engine.R`**

```r
library(testthat)
source(file.path("R", "app_config.R"))
source(file.path("R", "rules_engine.R"))

test_that("default config is valid", {
  cfg <- default_ruleset_config()
  expect_equal(cfg$starting_count$balls, 1L)
  expect_true(validate_ruleset_config(cfg)$ok)
})

test_that("coerce merges partial over defaults", {
  cfg <- coerce_ruleset_config(list(innings = 5, starting_count = list(balls = 0, strikes = 2)))
  expect_equal(cfg$innings, 5L)
  expect_equal(cfg$starting_count$strikes, 2L)
  expect_equal(cfg$foul_out_rule, "out")  # untouched default
})

test_that("validation rejects bad starting count and unknown enums", {
  bad <- default_ruleset_config()
  bad$starting_count$strikes <- 3L
  expect_false(validate_ruleset_config(bad)$ok)

  bad2 <- default_ruleset_config()
  bad2$foul_out_rule <- "explode"
  expect_false(validate_ruleset_config(bad2)$ok)

  bad3 <- default_ruleset_config()
  bad3$batting_gender_rule$type <- "every_n"
  bad3$batting_gender_rule$n <- NA_integer_   # every_n requires n
  expect_false(validate_ruleset_config(bad3)$ok)
})
```

- [ ] **Step 2: Run test, expect FAIL** (`could not find function "default_ruleset_config"`)

Run: `Rscript tests/test_rules_engine.R`

- [ ] **Step 3: Implement `R/rules_engine.R` (config portion)**

```r
default_ruleset_config <- function() {
  list(
    starting_count = list(balls = 1L, strikes = 1L),
    foul_out_rule = "out",
    batting_gender_rule = list(type = "none", n = NA_integer_),
    male_walk_rule = "none",
    fielding = list(min_females = 0L, position_requirements = list()),
    innings = 7L,
    run_cap_per_inning = NA_integer_,
    open_last_inning = TRUE,
    mercy_rule = list(differential = NA_integer_, after_inning = NA_integer_),
    short_lineup_auto_out = FALSE,
    courtesy_runner = FALSE
  )
}

.as_int_or_na <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA_integer_ else as.integer(x)

coerce_ruleset_config <- function(cfg) {
  d <- default_ruleset_config()
  cfg <- cfg %||% list()
  d <- utils::modifyList(d, cfg)
  d$starting_count$balls <- as.integer(d$starting_count$balls)
  d$starting_count$strikes <- as.integer(d$starting_count$strikes)
  d$innings <- as.integer(d$innings)
  d$run_cap_per_inning <- .as_int_or_na(d$run_cap_per_inning)
  d$batting_gender_rule$n <- .as_int_or_na(d$batting_gender_rule$n)
  d$fielding$min_females <- as.integer(d$fielding$min_females)
  d$mercy_rule$differential <- .as_int_or_na(d$mercy_rule$differential)
  d$mercy_rule$after_inning <- .as_int_or_na(d$mercy_rule$after_inning)
  d
}

validate_ruleset_config <- function(cfg) {
  errors <- character()
  add <- function(msg) errors <<- c(errors, msg)

  b <- cfg$starting_count$balls; s <- cfg$starting_count$strikes
  if (!is.numeric(b) || b < 0 || b > 3) add("starting balls must be 0-3")
  if (!is.numeric(s) || s < 0 || s > 2) add("starting strikes must be 0-2")
  if (!identical(cfg$foul_out_rule, "out") &&
      !identical(cfg$foul_out_rule, "one_courtesy_foul")) add("invalid foul_out_rule")

  bg <- cfg$batting_gender_rule$type
  if (!bg %in% c("none", "no_two_males_consecutive", "every_other", "every_n")) {
    add("invalid batting_gender_rule type")
  }
  if (identical(bg, "every_n") && is.na(cfg$batting_gender_rule$n)) {
    add("every_n batting rule requires n")
  }
  if (!cfg$male_walk_rule %in% c("none", "two_bases_then_female")) add("invalid male_walk_rule")
  if (!is.numeric(cfg$innings) || cfg$innings < 1) add("innings must be >= 1")

  list(ok = length(errors) == 0, errors = errors)
}
```

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_rules_engine.R`

- [ ] **Step 5: Commit**

```bash
git add R/rules_engine.R tests/test_rules_engine.R
git commit -m "feat: ruleset config defaults, coercion, and validation"
```

---

## Task 3: Event constructors + validation

**Files:**
- Create: `R/game_events.R`
- Test: `tests/test_game_events.R`

**Interfaces:**
- Produces:
  - `EVENT_TYPES` character vector.
  - `new_event(type, payload, seq = NA_integer_, ts = NA_character_)` → event list.
  - `validate_event(evt)` → `list(ok, errors)`.
  - `make_player(player_id, name, gender, jersey_number = NA, order_slot = NA, position = NA)` → Player list.
  - `make_advance(runner_id, from, to, scored = FALSE, out = FALSE, earned = TRUE)` → Advance list.

- [ ] **Step 1: Write `tests/test_game_events.R`**

```r
library(testthat)
source(file.path("R", "app_config.R"))
source(file.path("R", "game_events.R"))

test_that("new_event builds a well-formed event", {
  e <- new_event("plate_appearance", list(team = "away", batter_id = "p1",
    outcome = "1B", reached = 1L, rbi = 0L, outs_on_play = 0L, advances = list()))
  expect_equal(e$type, "plate_appearance")
  expect_true(validate_event(e)$ok)
})

test_that("validate_event rejects unknown type and bad outcome", {
  expect_false(validate_event(new_event("nope", list()))$ok)
  bad <- new_event("plate_appearance", list(team = "away", batter_id = "p1", outcome = "ZZ"))
  expect_false(validate_event(bad)$ok)
})

test_that("make_player and make_advance shape fields", {
  p <- make_player("p1", "Sam", "F", 9L, 1L, 6L)
  expect_equal(p$gender, "F")
  a <- make_advance("p1", 1L, 4L, scored = TRUE)
  expect_true(a$scored)
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `Rscript tests/test_game_events.R`

- [ ] **Step 3: Implement `R/game_events.R`**

```r
EVENT_TYPES <- c("game_start", "plate_appearance", "substitution",
                 "count_override", "inning_end")

new_event <- function(type, payload, seq = NA_integer_, ts = NA_character_) {
  list(seq = as.integer(seq), type = type, ts = ts, payload = payload %||% list())
}

make_player <- function(player_id, name, gender,
                        jersey_number = NA_integer_, order_slot = NA_integer_,
                        position = NA_integer_) {
  list(player_id = player_id, name = name, gender = gender,
       jersey_number = as.integer(jersey_number),
       order_slot = as.integer(order_slot),
       position = if (is.na(position)) NA_integer_ else as.integer(position))
}

make_advance <- function(runner_id, from, to, scored = FALSE, out = FALSE, earned = TRUE) {
  list(runner_id = runner_id, from = as.integer(from), to = as.integer(to),
       scored = isTRUE(scored), out = isTRUE(out), earned = isTRUE(earned))
}

validate_event <- function(evt) {
  errors <- character()
  add <- function(m) errors <<- c(errors, m)
  if (!evt$type %in% EVENT_TYPES) add(paste("unknown event type:", evt$type))
  if (identical(evt$type, "plate_appearance")) {
    o <- evt$payload$outcome
    if (is.null(o) || !o %in% c(APP_CONFIG$outcome_codes)) add(paste("bad outcome:", o %||% "NULL"))
    if (!evt$payload$team %in% c("home", "away")) add("plate_appearance needs team home/away")
  }
  if (identical(evt$type, "substitution")) {
    if (!evt$payload$kind %in% c("batting", "defensive", "courtesy_runner")) add("bad sub kind")
  }
  list(ok = length(errors) == 0, errors = errors)
}
```

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_game_events.R`

- [ ] **Step 5: Commit**

```bash
git add R/game_events.R tests/test_game_events.R
git commit -m "feat: event constructors, player/advance builders, validation"
```

---

## Task 4: Reducer core — initial state, count, outs, half-inning transitions

**Files:**
- Create: `R/game_reducer.R`
- Test: `tests/test_reducer_core.R`

**Interfaces:**
- Consumes: `default_ruleset_config()`, `coerce_ruleset_config()`, `new_event()`, `make_player()`.
- Produces:
  - `initial_game_state(ruleset)` → Game state at pre-first-pitch, empty lineups.
  - `apply_event(state, evt)` → new state.
  - `fold_events(events, ruleset = NULL)` → final state (reads ruleset from `game_start` if present).
  - Internal helper `reset_count(state)` and `advance_half(state)` (not exported contract, but named for later tasks).

This task handles: `game_start` (loads lineups, sets `batting_team` from `first_bat`, sets starting count, current batter), `count_override`, `inning_end`, and the **out-accumulation → automatic half-inning advance at 3 outs** using only `plate_appearance$outs_on_play` (advance/scoring logic lands in Task 5).

- [ ] **Step 1: Write `tests/test_reducer_core.R`**

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

mk_lineup <- function(prefix, genders = c("M","F","M","F")) {
  lapply(seq_along(genders), function(i)
    make_player(paste0(prefix, i), paste(prefix, i), genders[i], i, i, i))
}
start_evt <- function() new_event("game_start", list(
  ruleset = default_ruleset_config(), first_bat = "away",
  home = list(team_id = "H", name = "Home", lineup = mk_lineup("h")),
  away = list(team_id = "A", name = "Away", lineup = mk_lineup("a"))
), seq = 1L)

test_that("game_start seeds count, batting team, current batter", {
  s <- fold_events(list(start_evt()))
  expect_equal(s$batting_team, "away")
  expect_equal(s$count$balls, 1L)
  expect_equal(s$current_batter$player_id, "a1")
  expect_equal(s$outs, 0L)
})

test_that("three outs flips to bottom half and resets outs/count", {
  outs3 <- lapply(1:3, function(i) new_event("plate_appearance",
    list(team = "away", batter_id = paste0("a", i), outcome = "GO",
         reached = NA_integer_, rbi = 0L, outs_on_play = 1L, advances = list()),
    seq = 1L + i))
  s <- fold_events(c(list(start_evt()), outs3))
  expect_equal(s$half, "bottom")
  expect_equal(s$outs, 0L)
  expect_equal(s$batting_team, "home")
  expect_equal(s$count$balls, 1L)          # reset to starting count
  expect_equal(s$current_batter$player_id, "h1")
})

test_that("count_override sets the live count", {
  s <- fold_events(list(start_evt(), new_event("count_override",
        list(balls = 3L, strikes = 2L), seq = 2L)))
  expect_equal(s$count, list(balls = 3L, strikes = 2L))
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `Rscript tests/test_reducer_core.R`

- [ ] **Step 3: Implement `R/game_reducer.R` (core)**

```r
initial_game_state <- function(ruleset = default_ruleset_config()) {
  ruleset <- coerce_ruleset_config(ruleset)
  list(
    status = "in_progress", inning = 1L, half = "top", outs = 0L,
    count = list(balls = ruleset$starting_count$balls,
                 strikes = ruleset$starting_count$strikes),
    bases = list(first = NA_character_, second = NA_character_, third = NA_character_),
    score = list(home = 0L, away = 0L), runs_this_half = 0L,
    lineups = list(home = list(), away = list()),
    batting_index = list(home = 0L, away = 0L),
    batting_team = "away", current_batter = NULL,
    pa_log = list(), line_score = list(home = integer(), away = integer()),
    warnings = character(), ruleset = ruleset
  )
}

reset_count <- function(state) {
  state$count <- list(balls = state$ruleset$starting_count$balls,
                      strikes = state$ruleset$starting_count$strikes)
  state
}

.set_current_batter <- function(state) {
  team <- state$batting_team
  lineup <- state$lineups[[team]]
  if (length(lineup) == 0) { state$current_batter <- NULL; return(state) }
  batters <- Filter(function(p) !is.na(p$order_slot), lineup)
  batters <- batters[order(vapply(batters, function(p) p$order_slot, integer(1)))]
  idx <- (state$batting_index[[team]] %% length(batters))
  state$current_batter <- batters[[idx + 1]]
  state
}

advance_half <- function(state) {
  # record runs for the completed half into the line score
  state$line_score[[state$batting_team]] <-
    c(state$line_score[[state$batting_team]], state$runs_this_half)
  if (identical(state$half, "top")) {
    state$half <- "bottom"; state$batting_team <- "home"
  } else {
    state$half <- "top"; state$batting_team <- "away"; state$inning <- state$inning + 1L
  }
  state$outs <- 0L
  state$runs_this_half <- 0L
  state$bases <- list(first = NA_character_, second = NA_character_, third = NA_character_)
  state <- reset_count(state)
  state <- .set_current_batter(state)
  state
}

apply_event <- function(state, evt) {
  type <- evt$type
  if (type == "game_start") {
    p <- evt$payload
    state <- initial_game_state(p$ruleset %||% state$ruleset)
    state$lineups$home <- p$home$lineup
    state$lineups$away <- p$away$lineup
    state$teams <- list(home = p$home[c("team_id","name")], away = p$away[c("team_id","name")])
    state$batting_team <- p$first_bat %||% "away"
    state <- .set_current_batter(state)
    return(state)
  }
  if (type == "count_override") {
    state$count <- list(balls = as.integer(evt$payload$balls),
                        strikes = as.integer(evt$payload$strikes))
    return(state)
  }
  if (type == "inning_end") return(advance_half(state))
  if (type == "plate_appearance") {
    # Full advance/scoring logic added in Task 5; core handles outs + turn here.
    state <- apply_plate_appearance(state, evt)   # defined in Task 5
    return(state)
  }
  if (type == "substitution") return(apply_substitution(state, evt))  # Task 7
  state
}

fold_events <- function(events, ruleset = NULL) {
  state <- initial_game_state(ruleset %||% default_ruleset_config())
  for (evt in events) state <- apply_event(state, evt)
  state
}
```

Also add a **temporary minimal** `apply_plate_appearance` at the bottom of the file so Task 4 tests pass in isolation; Task 5 replaces its body:

```r
apply_plate_appearance <- function(state, evt) {
  p <- evt$payload
  state$outs <- state$outs + as.integer(p$outs_on_play %||% 0L)
  # advance the batting order and reset the count for the next batter
  team <- state$batting_team
  state$batting_index[[team]] <- state$batting_index[[team]] + 1L
  state <- reset_count(state)
  if (state$outs >= 3L) state <- advance_half(state) else state <- .set_current_batter(state)
  state
}
apply_substitution <- function(state, evt) state  # replaced in Task 7
```

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_reducer_core.R`

- [ ] **Step 5: Commit**

```bash
git add R/game_reducer.R tests/test_reducer_core.R
git commit -m "feat: reducer core — start, count, outs, half-inning transitions"
```

---

## Task 5: Reducer — plate-appearance outcomes, runner advancement, scoring, RBIs, PA log

**Files:**
- Modify: `R/game_reducer.R` (replace `apply_plate_appearance` body; add `.place_runner`, `.score_runner`, `suggest_advances`)
- Test: `tests/test_reducer_pa.R`

**Interfaces:**
- Consumes: state shape, `make_advance()`.
- Produces:
  - Replaced `apply_plate_appearance(state, evt)` that applies `advances`, sets bases, increments score + `runs_this_half`, appends a `pa_log` record, and respects `reached`.
  - `suggest_advances(state, outcome)` → `list(<Advance>...)` — default runner movement for an outcome, used by the UI to pre-fill (batter + forced runners). Pure; no randomness.
  - `pa_log` record shape: `list(inning, half, team, batter_id, outcome, fielding, rbi, outs_on_play, reached, bases_after = list(first,second,third))`.

- [ ] **Step 1: Write `tests/test_reducer_pa.R`**

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))
mk_lineup <- function(prefix, genders = c("M","F","M","F"))
  lapply(seq_along(genders), function(i)
    make_player(paste0(prefix, i), paste(prefix, i), genders[i], i, i, i))
start_evt <- function() new_event("game_start", list(
  ruleset = default_ruleset_config(), first_bat = "away",
  home = list(team_id="H", name="Home", lineup = mk_lineup("h")),
  away = list(team_id="A", name="Away", lineup = mk_lineup("a"))), seq = 1L)
pa <- function(batter, outcome, reached, rbi = 0L, outs = 0L, advances = list(), seq = 2L)
  new_event("plate_appearance", list(team="away", batter_id=batter, outcome=outcome,
    reached=reached, rbi=rbi, outs_on_play=outs, advances=advances), seq = seq)

test_that("a single puts the batter on first", {
  s <- fold_events(list(start_evt(), pa("a1", "1B", 1L)))
  expect_equal(s$bases$first, "a1")
  expect_equal(s$outs, 0L)
})

test_that("home run with a runner on first scores two and adds RBIs", {
  s <- fold_events(list(
    start_evt(),
    pa("a1", "1B", 1L, seq = 2L),
    pa("a2", "HR", 4L, rbi = 2L, seq = 3L,
       advances = list(make_advance("a1", 1L, 4L, scored = TRUE),
                       make_advance("a2", 0L, 4L, scored = TRUE)))
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
```

- [ ] **Step 2: Run test, expect FAIL** (current minimal `apply_plate_appearance` ignores advances)

Run: `Rscript tests/test_reducer_pa.R`

- [ ] **Step 3: Replace `apply_plate_appearance` and add helpers in `R/game_reducer.R`**

```r
.clear_base_of <- function(bases, runner_id) {
  for (b in c("first","second","third"))
    if (!is.na(bases[[b]]) && bases[[b]] == runner_id) bases[[b]] <- NA_character_
  bases
}
.base_slot <- function(n) c("1"="first","2"="second","3"="third")[as.character(n)]

apply_plate_appearance <- function(state, evt) {
  p <- evt$payload
  team <- state$batting_team
  bases <- state$bases
  runs <- 0L

  # Apply each advance: remove runner from old base, place at new base or score/out.
  for (a in (p$advances %||% list())) {
    bases <- .clear_base_of(bases, a$runner_id)
    if (isTRUE(a$scored)) {
      runs <- runs + 1L
    } else if (!isTRUE(a$out) && a$to %in% c(1L,2L,3L)) {
      bases[[.base_slot(a$to)]] <- a$runner_id
    }
  }
  # Batter's own landing spot if not covered by an advance and not out.
  # %||% guards NULL (e.g. from a JSON round trip where NA_integer_ serialized to null).
  reached <- p$reached %||% NA_integer_
  if (!is.na(reached) && reached %in% c(1L,2L,3L)) {
    already <- any(vapply(p$advances %||% list(),
      function(a) identical(a$runner_id, p$batter_id), logical(1)))
    if (!already) bases[[.base_slot(reached)]] <- p$batter_id
  }
  if (!is.na(reached) && reached == 4L) {
    already <- any(vapply(p$advances %||% list(),
      function(a) identical(a$runner_id, p$batter_id) && isTRUE(a$scored), logical(1)))
    if (!already) runs <- runs + 1L
  }

  state$bases <- bases
  state$score[[team]] <- state$score[[team]] + runs
  state$runs_this_half <- state$runs_this_half + runs
  state$outs <- state$outs + as.integer(p$outs_on_play %||% 0L)

  state$pa_log <- c(state$pa_log, list(list(
    inning = state$inning, half = state$half, team = team,
    batter_id = p$batter_id, outcome = p$outcome, fielding = p$fielding %||% NA_character_,
    rbi = as.integer(p$rbi %||% 0L), outs_on_play = as.integer(p$outs_on_play %||% 0L),
    reached = reached,
    bases_after = list(first = bases$first, second = bases$second, third = bases$third)
  )))

  state$batting_index[[team]] <- state$batting_index[[team]] + 1L
  state <- reset_count(state)
  if (state$outs >= 3L) state <- advance_half(state) else state <- .set_current_batter(state)
  state
}

suggest_advances <- function(state, outcome) {
  b <- state$bases
  occ <- c(first = !is.na(b$first), second = !is.na(b$second), third = !is.na(b$third))
  adv <- list()
  push <- function(id, from, to, scored = FALSE)
    adv[[length(adv) + 1]] <<- make_advance(id, from, to, scored = scored)

  bump <- switch(outcome, "1B" = 1L, "2B" = 2L, "3B" = 3L, "HR" = 4L,
                 "BB" = 1L, "IBB" = 1L, "HBP" = 1L, 0L)
  if (bump == 0L) return(adv)  # outs: no automatic advance suggestion

  is_walk <- outcome %in% c("BB","IBB","HBP")
  # Runners advance by `bump` bases on hits; on walks only forced runners move.
  if (occ["third"]) {
    to <- if (is_walk) (if (occ["second"] && occ["first"]) 4L else 3L) else min(4L, 3L + bump)
    push(b$third, 3L, to, scored = to >= 4L)
  }
  if (occ["second"]) {
    to <- if (is_walk) (if (occ["first"]) 3L else 2L) else min(4L, 2L + bump)
    push(b$second, 2L, to, scored = to >= 4L)
  }
  if (occ["first"]) {
    to <- min(4L, 1L + bump)
    push(b$first, 1L, to, scored = to >= 4L)
  }
  adv
}
```

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_reducer_pa.R` (also re-run `tests/test_reducer_core.R` to confirm no regressions).

- [ ] **Step 5: Commit**

```bash
git add R/game_reducer.R tests/test_reducer_pa.R
git commit -m "feat: reducer plate-appearance outcomes, advancement, scoring, PA log"
```

---

## Task 6: Rules-engine live evaluation (warnings, run cap, mercy, foul-out)

**Files:**
- Modify: `R/rules_engine.R` (add evaluators), `R/game_reducer.R` (call evaluators into `state$warnings`, apply run cap on half-advance, set `status = "final"` on mercy/last inning)
- Test: `tests/test_rules_eval.R`

**Interfaces:**
- Produces (in `rules_engine.R`):
  - `next_batter_gender_ok(cfg, prev_genders, next_gender)` → logical.
  - `fielding_warnings(cfg, defense_lineup)` → character().
  - `game_should_end(cfg, state)` → logical (mercy or regulation innings complete).
  - `apply_run_cap(cfg, runs_this_half, inning)` → integer capped runs (open last inning ignores cap).
- Consumes in reducer: the above.

- [ ] **Step 1: Write `tests/test_rules_eval.R`**

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))

test_that("no_two_males_consecutive flags a male after a male", {
  cfg <- coerce_ruleset_config(list(batting_gender_rule = list(type = "no_two_males_consecutive")))
  expect_false(next_batter_gender_ok(cfg, prev_genders = c("M"), next_gender = "M"))
  expect_true(next_batter_gender_ok(cfg, prev_genders = c("M"), next_gender = "F"))
})

test_that("fielding_warnings triggers below min_females", {
  cfg <- coerce_ruleset_config(list(fielding = list(min_females = 4L)))
  defense <- lapply(1:9, function(i) make_player(paste0("d",i), "x", "M", i, i, i))
  expect_true(length(fielding_warnings(cfg, defense)) > 0)
})

test_that("run cap limits non-open innings", {
  cfg <- coerce_ruleset_config(list(run_cap_per_inning = 5L, innings = 7L, open_last_inning = TRUE))
  expect_equal(apply_run_cap(cfg, runs_this_half = 8L, inning = 3L), 5L)
  expect_equal(apply_run_cap(cfg, runs_this_half = 8L, inning = 7L), 8L)  # open last
})

test_that("mercy ends the game", {
  cfg <- coerce_ruleset_config(list(mercy_rule = list(differential = 10L, after_inning = 4L)))
  st <- list(inning = 5L, half = "top", score = list(home = 15L, away = 3L),
             ruleset = cfg, outs = 0L)
  expect_true(game_should_end(cfg, st))
})

test_that("reducer surfaces a gender-order warning for the batter due up", {
  cfg <- coerce_ruleset_config(list(batting_gender_rule = list(type = "no_two_males_consecutive")))
  lineup <- list(make_player("m1","M1","M",1L,1L,6L), make_player("m2","M2","M",2L,2L,4L))
  start <- new_event("game_start", list(ruleset = cfg, first_bat = "away",
    home = list(team_id="H", name="Home", lineup = lineup),
    away = list(team_id="A", name="Away", lineup = lineup)), seq = 1L)
  pa1 <- new_event("plate_appearance", list(team="away", batter_id="m1", outcome="1B",
    reached=1L, rbi=0L, outs_on_play=0L, advances=list()), seq = 2L)
  s <- fold_events(list(start, pa1))   # m2 (M) now due up after m1 (M)
  expect_true(any(grepl("gender", s$warnings, ignore.case = TRUE)))
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `Rscript tests/test_rules_eval.R`

- [ ] **Step 3: Add evaluators to `R/rules_engine.R`**

```r
next_batter_gender_ok <- function(cfg, prev_genders, next_gender) {
  rule <- cfg$batting_gender_rule
  type <- rule$type
  if (type == "none") return(TRUE)
  last <- if (length(prev_genders)) tail(prev_genders, 1) else NA_character_
  if (type == "no_two_males_consecutive") return(!(identical(last, "M") && identical(next_gender, "M")))
  if (type == "every_other") return(is.na(last) || !identical(last, next_gender))
  if (type == "every_n") {
    n <- cfg$batting_gender_rule$n
    recent <- tail(c(prev_genders, next_gender), n)
    return(any(recent == "F"))  # at least one F in every window of n
  }
  TRUE
}

fielding_warnings <- function(cfg, defense_lineup) {
  warns <- character()
  on_d <- Filter(function(p) !is.na(p$position), defense_lineup)
  n_f <- sum(vapply(on_d, function(p) identical(p$gender, "F"), logical(1)))
  if (n_f < (cfg$fielding$min_females %||% 0L))
    warns <- c(warns, sprintf("Fielding requires ≥ %d female players (currently %d).",
                              cfg$fielding$min_females, n_f))
  warns
}

apply_run_cap <- function(cfg, runs_this_half, inning) {
  cap <- cfg$run_cap_per_inning
  if (is.na(cap)) return(as.integer(runs_this_half))
  if (isTRUE(cfg$open_last_inning) && inning >= cfg$innings) return(as.integer(runs_this_half))
  as.integer(min(runs_this_half, cap))
}

game_should_end <- function(cfg, state) {
  m <- cfg$mercy_rule
  if (!is.na(m$differential)) {
    diff <- abs(state$score$home - state$score$away)
    after <- if (is.na(m$after_inning)) 1L else m$after_inning  # %||% won't catch NA
    if (state$inning >= after && diff >= m$differential) return(TRUE)
  }
  # Regulation complete: finished the bottom of the final inning.
  if (state$inning > cfg$innings) return(TRUE)
  FALSE
}
```

- [ ] **Step 4: Wire evaluators into the reducer.** In `R/game_reducer.R`:
  - **(a) Cap runs in `apply_plate_appearance`.** After the advances loop computes `runs` (and after the batter's own run is added), and immediately **before** `state$score[[team]] <- state$score[[team]] + runs`, insert:
    ```r
    capped <- apply_run_cap(state$ruleset, state$runs_this_half + runs, state$inning) - state$runs_this_half
    runs <- max(0L, capped)
    ```
  - **(b) Add the `.refresh_flags` helper** to `R/game_reducer.R`:
    ```r
    .refresh_flags <- function(state) {
      cfg <- state$ruleset
      def_team <- if (identical(state$batting_team, "away")) "home" else "away"
      w <- fielding_warnings(cfg, state$lineups[[def_team]])
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
          w <- c(w, "Batting order: the batter due up violates the gender rule.")
        }
      }
      state$warnings <- w
      if (game_should_end(cfg, state)) state$status <- "final"
      state
    }
    ```
  - **(c) Route every non-`game_start` branch of `apply_event` through `.refresh_flags`.** Change those branches' returns to:
    - `count_override`: `return(.refresh_flags(state))`
    - `inning_end`: `return(.refresh_flags(advance_half(state)))`
    - `plate_appearance`: `state <- apply_plate_appearance(state, evt); return(.refresh_flags(state))`
    - `substitution`: `return(.refresh_flags(apply_substitution(state, evt)))`
    Leave the `game_start` branch returning its state directly (no flags before the first pitch).

- [ ] **Step 5: Run all reducer + rules tests, expect PASS**

Run: `Rscript tests/test_rules_eval.R && Rscript tests/test_reducer_core.R && Rscript tests/test_reducer_pa.R`

- [ ] **Step 6: Commit**

```bash
git add R/rules_engine.R R/game_reducer.R tests/test_rules_eval.R
git commit -m "feat: live rule evaluation — gender order, fielding, run cap, mercy"
```

---

## Task 7: Substitutions in the reducer

**Files:**
- Modify: `R/game_reducer.R` (implement `apply_substitution`)
- Test: `tests/test_reducer_subs.R`

**Interfaces:**
- Produces: `apply_substitution(state, evt)` that, by `kind`:
  - `"batting"`: replaces the player at `order_slot` in the batting team's lineup with `in_player` (keeping `order_slot`).
  - `"defensive"`: replaces `out_player_id` with `in_player` in the defensive lineup, assigning `position`.
  - `"courtesy_runner"`: swaps the runner id currently on a base (`out_player_id`) for `in_player$player_id` on that base.

- [ ] **Step 1: Write `tests/test_reducer_subs.R`**

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))
mk_lineup <- function(prefix, genders = c("M","F","M","F"))
  lapply(seq_along(genders), function(i)
    make_player(paste0(prefix,i), paste(prefix,i), genders[i], i, i, i))
start_evt <- function() new_event("game_start", list(
  ruleset = default_ruleset_config(), first_bat = "away",
  home = list(team_id="H", name="Home", lineup = mk_lineup("h")),
  away = list(team_id="A", name="Away", lineup = mk_lineup("a"))), seq = 1L)

test_that("batting sub replaces the player in the order slot", {
  sub <- new_event("substitution", list(team="away", kind="batting",
    out_player_id="a1", order_slot=1L,
    in_player = make_player("x9","Pinch","F",99L,1L,NA_integer_)), seq = 2L)
  s <- fold_events(list(start_evt(), sub))
  slot1 <- Filter(function(p) p$order_slot==1L, s$lineups$away)[[1]]
  expect_equal(slot1$player_id, "x9")
})

test_that("courtesy runner swaps the runner on base", {
  pa1 <- new_event("plate_appearance", list(team="away", batter_id="a1",
    outcome="1B", reached=1L, rbi=0L, outs_on_play=0L, advances=list()), seq = 2L)
  cr <- new_event("substitution", list(team="away", kind="courtesy_runner",
    out_player_id="a1", in_player = make_player("cr1","Runner","M",50L,NA_integer_,NA_integer_)),
    seq = 3L)
  s <- fold_events(list(start_evt(), pa1, cr))
  expect_equal(s$bases$first, "cr1")
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `Rscript tests/test_reducer_subs.R`

- [ ] **Step 3: Implement `apply_substitution` in `R/game_reducer.R`**

```r
apply_substitution <- function(state, evt) {
  p <- evt$payload
  team <- p$team
  if (p$kind == "batting") {
    lineup <- state$lineups[[team]]
    for (i in seq_along(lineup)) {
      if (identical(lineup[[i]]$order_slot, as.integer(p$order_slot))) {
        inp <- p$in_player; inp$order_slot <- as.integer(p$order_slot)
        lineup[[i]] <- inp
      }
    }
    state$lineups[[team]] <- lineup
    state <- .set_current_batter(state)
  } else if (p$kind == "defensive") {
    lineup <- state$lineups[[team]]
    for (i in seq_along(lineup)) {
      if (identical(lineup[[i]]$player_id, p$out_player_id)) {
        inp <- p$in_player; inp$position <- if (is.null(p$position)) NA_integer_ else as.integer(p$position)
        lineup[[i]] <- inp
      }
    }
    state$lineups[[team]] <- lineup
  } else if (p$kind == "courtesy_runner") {
    for (b in c("first","second","third"))
      if (!is.na(state$bases[[b]]) && state$bases[[b]] == p$out_player_id)
        state$bases[[b]] <- p$in_player$player_id
  }
  state
}
```

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_reducer_subs.R`

- [ ] **Step 5: Commit**

```bash
git add R/game_reducer.R tests/test_reducer_subs.R
git commit -m "feat: substitutions (batting, defensive, courtesy runner)"
```

---

## Task 8: Box score & line score

**Files:**
- Create: `R/boxscore.R`
- Test: `tests/test_boxscore.R`

**Interfaces:**
- Consumes: final `state` from `fold_events` (`pa_log`, `line_score`, `lineups`, `teams`).
- Produces:
  - `line_score(state)` → `list(home = list(runs = <int vector>, R, H, E), away = list(...))` where per-inning runs come from `state$line_score` (pad current half).
  - `batting_lines(state, team)` → `data.frame(player_id, name, AB, R, H, RBI, BB, K)` computed from `pa_log`.

- [ ] **Step 1: Write `tests/test_boxscore.R`**

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R","boxscore.R"))
  source(file.path("R", f))
mk_lineup <- function(prefix, genders = c("M","F","M","F"))
  lapply(seq_along(genders), function(i)
    make_player(paste0(prefix,i), paste(prefix,i), genders[i], i, i, i))
start_evt <- function() new_event("game_start", list(
  ruleset = default_ruleset_config(), first_bat = "away",
  home = list(team_id="H", name="Home", lineup = mk_lineup("h")),
  away = list(team_id="A", name="Away", lineup = mk_lineup("a"))), seq = 1L)
pa <- function(b,o,r,seq,rbi=0L,outs=0L,adv=list()) new_event("plate_appearance",
  list(team="away",batter_id=b,outcome=o,reached=r,rbi=rbi,outs_on_play=outs,advances=adv), seq=seq)

test_that("batting lines count H, AB, K, BB", {
  s <- fold_events(list(start_evt(),
    pa("a1","1B",1L,2L),
    pa("a2","K",NA_integer_,3L,outs=1L),
    pa("a3","BB",1L,4L)))
  bl <- batting_lines(s, "away")
  a1 <- bl[bl$player_id=="a1",]; a2 <- bl[bl$player_id=="a2",]; a3 <- bl[bl$player_id=="a3",]
  expect_equal(a1$H, 1L); expect_equal(a1$AB, 1L)
  expect_equal(a2$K, 1L); expect_equal(a2$AB, 1L)
  expect_equal(a3$BB, 1L); expect_equal(a3$AB, 0L)  # walks are not at-bats
})

test_that("line score exposes R/H/E totals", {
  s <- fold_events(list(start_evt(), pa("a1","1B",1L,2L)))
  ls <- line_score(s)
  expect_true(is.list(ls$away))
  expect_true(!is.null(ls$away$H))
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `Rscript tests/test_boxscore.R`

- [ ] **Step 3: Implement `R/boxscore.R`**

```r
.HIT <- c("1B","2B","3B","HR")
.AB_EXCLUDE <- c("BB","IBB","HBP","SF","SAC")  # not at-bats

batting_lines <- function(state, team) {
  lineup <- state$lineups[[team]]
  ids <- vapply(lineup, function(p) p$player_id, character(1))
  names_ <- vapply(lineup, function(p) p$name, character(1))
  init <- function() setNames(as.list(rep(0L, length(ids))), ids)
  AB<-init(); R<-init(); H<-init(); RBI<-init(); BB<-init(); K<-init()

  for (rec in state$pa_log) {
    if (!identical(rec$team, team)) next
    id <- rec$batter_id
    if (is.null(AB[[id]])) next
    o <- rec$outcome
    if (!o %in% .AB_EXCLUDE) AB[[id]] <- AB[[id]] + 1L
    if (o %in% .HIT) H[[id]] <- H[[id]] + 1L
    if (o %in% c("K","KL")) K[[id]] <- K[[id]] + 1L
    if (o %in% c("BB","IBB")) BB[[id]] <- BB[[id]] + 1L
    RBI[[id]] <- RBI[[id]] + as.integer(rec$rbi %||% 0L)
    if (!is.na(rec$reached) && rec$reached == 4L) R[[id]] <- R[[id]] + 1L
  }
  data.frame(player_id = ids, name = names_,
    AB = unlist(AB[ids]), R = unlist(R[ids]), H = unlist(H[ids]),
    RBI = unlist(RBI[ids]), BB = unlist(BB[ids]), K = unlist(K[ids]),
    row.names = NULL, stringsAsFactors = FALSE)
}

line_score <- function(state) {
  totals <- function(team) {
    runs <- state$line_score[[team]]
    if (identical(team, state$batting_team)) runs <- c(runs, state$runs_this_half)
    h <- sum(vapply(state$pa_log, function(r)
      identical(r$team, team) && r$outcome %in% .HIT, logical(1)))
    list(runs = as.integer(runs), R = sum(as.integer(runs)), H = as.integer(h), E = 0L)
  }
  list(home = totals("home"), away = totals("away"))
}
```

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_boxscore.R`

- [ ] **Step 5: Commit**

```bash
git add R/boxscore.R tests/test_boxscore.R
git commit -m "feat: box score batting lines and line score"
```

---

## Task 9: JSON export / import

**Files:**
- Create: `R/json_io.R`
- Test: `tests/test_json_io.R`

**Interfaces:**
- Produces:
  - `game_to_json(events, meta = list())` → JSON string `{ version, meta, events }`.
  - `game_from_json(txt)` → `list(version, meta, events)` with events coerced back to integer `seq` and valid payloads.
  - Round-trip guarantee: `fold_events(game_from_json(game_to_json(evts))$events)` equals `fold_events(evts)` in score/bases.

- [ ] **Step 1: Write `tests/test_json_io.R`**

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R","json_io.R"))
  source(file.path("R", f))
mk_lineup <- function(prefix) lapply(1:4, function(i)
  make_player(paste0(prefix,i), paste(prefix,i), c("M","F","M","F")[i], i, i, i))
evts <- list(
  new_event("game_start", list(ruleset = default_ruleset_config(), first_bat="away",
    home=list(team_id="H",name="Home",lineup=mk_lineup("h")),
    away=list(team_id="A",name="Away",lineup=mk_lineup("a"))), seq=1L),
  new_event("plate_appearance", list(team="away",batter_id="a1",outcome="1B",
    reached=1L,rbi=0L,outs_on_play=0L,advances=list()), seq=2L))

test_that("round trip preserves game outcome", {
  txt <- game_to_json(evts)
  back <- game_from_json(txt)
  s1 <- fold_events(evts); s2 <- fold_events(back$events)
  expect_equal(s2$bases$first, s1$bases$first)
  expect_equal(s2$score, s1$score)
  expect_equal(back$events[[2]]$seq, 2L)  # seq restored as integer
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `Rscript tests/test_json_io.R`

- [ ] **Step 3: Implement `R/json_io.R`**

```r
game_to_json <- function(events, meta = list()) {
  jsonlite::toJSON(list(version = 1L, meta = meta, events = events),
                   auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE)
}

game_from_json <- function(txt) {
  raw <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  events <- lapply(raw$events, function(e) {
    e$seq <- if (is.null(e$seq)) NA_integer_ else as.integer(e$seq)
    e
  })
  list(version = raw$version %||% 1L, meta = raw$meta %||% list(), events = events)
}
```

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_json_io.R`

- [ ] **Step 5: Commit**

```bash
git add R/json_io.R tests/test_json_io.R
git commit -m "feat: JSON export/import with fold round-trip guarantee"
```

---

## Task 10: SVG scorebook rendering

**Files:**
- Create: `R/scorebook_render.R`
- Test: `tests/test_scorebook_render.R`

**Interfaces:**
- Consumes: final `state` (`pa_log`, `lineups`, `teams`).
- Produces:
  - `render_scorebook_svg(state, team)` → an `htmltools::HTML` string containing one `<svg>` grid: batter rows × inning columns; each played cell draws a diamond (`.bw-diamond` path), fills bases reached, prints `outcome`/`fielding` text and RBI dots.
  - `scorebook_cell_svg(rec, x, y, cell)` → character SVG fragment for one plate appearance (pure, testable).

- [ ] **Step 1: Write `tests/test_scorebook_render.R`**

```r
library(testthat)
# brand_colors.R must be sourced (scorebook_render.R uses BRAND_COLORS); it reads _brand.yml
# relative to the project root, so run this test from the project root.
for (f in c("app_config.R","brand_colors.R","rules_engine.R","game_events.R","game_reducer.R","scorebook_render.R"))
  source(file.path("R", f))

test_that("cell svg contains a diamond polygon and the outcome text", {
  rec <- list(inning=1L, half="top", team="away", batter_id="a1", outcome="1B",
              fielding=NA_character_, rbi=0L, outs_on_play=0L, reached=1L,
              bases_after=list(first="a1",second=NA,third=NA))
  frag <- scorebook_cell_svg(rec, x = 0, y = 0, cell = 60)
  expect_true(grepl("polygon", frag))
  expect_true(grepl("1B", frag))
})

test_that("render_scorebook_svg returns an svg with a row per batter", {
  mk <- lapply(1:3, function(i) make_player(paste0("a",i), paste("A",i), "M", i, i, i))
  st <- initial_game_state(); st$lineups$away <- mk
  st$teams <- list(away = list(team_id="A", name="Away"))
  st$pa_log <- list(list(inning=1L,half="top",team="away",batter_id="a1",outcome="1B",
    fielding=NA_character_,rbi=0L,outs_on_play=0L,reached=1L,
    bases_after=list(first="a1",second=NA,third=NA)))
  html <- as.character(render_scorebook_svg(st, "away"))
  expect_true(grepl("<svg", html))
  expect_true(grepl("A 1", html) || grepl("A1", html))
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `Rscript tests/test_scorebook_render.R`

- [ ] **Step 3: Implement `R/scorebook_render.R`**

```r
# A diamond centered in a `cell`-sized box at (x, y); bases filled per rec$reached.
scorebook_cell_svg <- function(rec, x, y, cell) {
  cx <- x + cell / 2; cy <- y + cell / 2; r <- cell * 0.36
  top <- sprintf("%f,%f", cx, cy - r); right <- sprintf("%f,%f", cx + r, cy)
  bot <- sprintf("%f,%f", cx, cy + r); left <- sprintf("%f,%f", cx - r, cy)
  reached <- rec$reached %||% NA
  filled <- if (!is.na(reached) && reached == 4L) BRAND_COLORS$primary else "none"
  parts <- c(
    sprintf('<polygon points="%s %s %s %s" fill="%s" stroke="%s" stroke-width="1"/>',
            top, right, bot, left, filled, BRAND_COLORS$dark),
    sprintf('<text x="%f" y="%f" font-size="%f" text-anchor="middle" fill="%s">%s</text>',
            cx, y + cell - 4, cell * 0.18, BRAND_COLORS$dark,
            paste0(rec$outcome, if (!is.na(rec$fielding %||% NA)) paste0(" ", rec$fielding) else ""))
  )
  if (!is.na(rec$rbi %||% 0L) && rec$rbi > 0)
    parts <- c(parts, sprintf('<circle cx="%f" cy="%f" r="2.5" fill="%s"/>',
                              x + 6, y + 8, BRAND_COLORS$danger))
  paste(parts, collapse = "")
}

render_scorebook_svg <- function(state, team) {
  lineup <- state$lineups[[team]]
  batters <- Filter(function(p) !is.na(p$order_slot), lineup)
  batters <- batters[order(vapply(batters, function(p) p$order_slot, integer(1)))]
  innings <- max(1L, state$inning)
  cell <- 60; label_w <- 120; header_h <- 24
  w <- label_w + innings * cell; h <- header_h + length(batters) * cell

  rows <- character()
  for (bi in seq_along(batters)) {
    p <- batters[[bi]]; y <- header_h + (bi - 1) * cell
    rows <- c(rows, sprintf('<text x="6" y="%f" font-size="12" fill="%s">%s %s</text>',
      y + cell/2, BRAND_COLORS$dark,
      if (!is.na(p$jersey_number)) paste0("#", p$jersey_number) else "", p$name))
    for (rec in state$pa_log) {
      if (!identical(rec$team, team) || !identical(rec$batter_id, p$player_id)) next
      x <- label_w + (rec$inning - 1L) * cell
      rows <- c(rows, scorebook_cell_svg(rec, x, y, cell))
    }
  }
  grid <- sprintf('<rect x="0" y="0" width="%d" height="%d" fill="none" stroke="%s"/>',
                  w, h, BRAND_COLORS$secondary)
  htmltools::HTML(sprintf(
    '<div class="bw-scorebook"><svg width="%d" height="%d" viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg">%s%s</svg></div>',
    w, h, w, h, grid, paste(rows, collapse = "")))
}
```

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_scorebook_render.R`

- [ ] **Step 5: Visual smoke check (optional but recommended).** Write the SVG to a temp HTML file and open it:

Run:
```bash
Rscript -e 'for (f in c("app_config.R","brand_colors.R","rules_engine.R","game_events.R","game_reducer.R","scorebook_render.R")) source(file.path("R",f)); mk<-lapply(1:3,function(i)make_player(paste0("a",i),paste("A",i),"M",i,i,i)); st<-initial_game_state(); st$lineups$away<-mk; st$pa_log<-list(list(inning=1L,half="top",team="away",batter_id="a1",outcome="1B",fielding="6-3",rbi=0L,outs_on_play=0L,reached=1L,bases_after=list())); writeLines(as.character(render_scorebook_svg(st,"away")), "scorebook_preview.html")'
```
Expected: `scorebook_preview.html` exists and opens showing a labeled grid with a diamond. Delete it after (`rm scorebook_preview.html`).

- [ ] **Step 6: Commit**

```bash
git add R/scorebook_render.R tests/test_scorebook_render.R
git commit -m "feat: SVG diamond-in-cell scorebook rendering"
```

---

## Task 11: Supabase schema SQL

**Files:**
- Create: `data-raw/supabase_schema.sql`
- Create: `data-raw/README.md` (how to apply the schema)

**Interfaces:**
- Produces: the DDL matching `DB_TABLES` and the Shared-Data-Shapes columns. No R code; applied manually in the Supabase SQL editor.

- [ ] **Step 1: Write `data-raw/supabase_schema.sql`**

```sql
-- Bookworm schema. Apply in the Supabase SQL editor.
create extension if not exists "pgcrypto";

create table if not exists profiles (
  id uuid primary key references auth.users(id) on delete cascade,
  display_name text,
  created_at timestamptz not null default now()
);

create table if not exists leagues (
  id uuid primary key default gen_random_uuid(),
  owner_id uuid not null references auth.users(id) on delete cascade,
  name text not null,
  sport text not null default 'softball' check (sport in ('softball','baseball')),
  created_at timestamptz not null default now()
);

create table if not exists rulesets (
  id uuid primary key default gen_random_uuid(),
  league_id uuid not null references leagues(id) on delete cascade,
  name text not null,
  config jsonb not null,
  created_at timestamptz not null default now()
);

create table if not exists teams (
  id uuid primary key default gen_random_uuid(),
  league_id uuid not null references leagues(id) on delete cascade,
  name text not null,
  created_at timestamptz not null default now()
);

create table if not exists players (
  id uuid primary key default gen_random_uuid(),
  team_id uuid not null references teams(id) on delete cascade,
  name text not null,
  gender text check (gender in ('M','F')),
  jersey_number int,
  default_position int
);

create table if not exists games (
  id uuid primary key default gen_random_uuid(),
  owner_id uuid not null references auth.users(id) on delete cascade,
  league_id uuid references leagues(id) on delete set null,
  ruleset_id uuid references rulesets(id) on delete set null,
  home_team_id uuid references teams(id) on delete set null,
  away_team_id uuid references teams(id) on delete set null,
  played_on date,
  location text,
  status text not null default 'in_progress' check (status in ('in_progress','final')),
  state_snapshot jsonb,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

create table if not exists game_events (
  id uuid primary key default gen_random_uuid(),
  game_id uuid not null references games(id) on delete cascade,
  seq int not null,
  type text not null,
  payload jsonb not null,
  created_at timestamptz not null default now(),
  unique (game_id, seq)
);

create table if not exists plate_appearances (
  id uuid primary key default gen_random_uuid(),
  game_id uuid not null references games(id) on delete cascade,
  inning int, half text, team_id uuid, batter_id uuid,
  batting_order_slot int, outcome text, fielding_notation text,
  rbi int, outs_recorded int, errors jsonb, base_advancement jsonb,
  count_at_end jsonb, seq int
);

create index if not exists idx_game_events_game on game_events(game_id, seq);
create index if not exists idx_games_owner on games(owner_id);

-- RLS policies (DEFINED for Phase 3; app enforces owner scoping in slice one).
-- alter table games enable row level security;
-- create policy games_owner on games using (owner_id = auth.uid());
-- (repeat per owned table when RLS is turned on.)
```

- [ ] **Step 2: Write `data-raw/README.md`**

```markdown
# Applying the Bookworm schema

1. Open your Supabase project → SQL Editor.
2. Paste the contents of `supabase_schema.sql` and run it.
3. In `.Renviron` (copy from `.Renviron.template`), set the SUPABASE_* values.
4. RLS is intentionally left disabled in slice one; the app enforces
   `owner_id = <signed-in user>` scoping. Enable the commented policies in Phase 3.
```

- [ ] **Step 3: Commit**

```bash
git add data-raw/supabase_schema.sql data-raw/README.md
git commit -m "feat: supabase schema DDL and apply instructions"
```

---

## Task 12: Supabase client (auth REST + Postgres connection)

**Files:**
- Create: `R/supabase_client.R`
- Test: `tests/test_supabase_client.R` (offline/unit tests only — no live network)

**Interfaces:**
- Produces:
  - `supabase_configured()` → logical (all SUPABASE_* env vars present).
  - `supabase_connect()` → a `DBI` connection (RPostgres) or `stop()` with a clear message.
  - `gotrue_sign_in(email, password)` → `list(ok, user_id, access_token, error)`.
  - `gotrue_sign_up(email, password)` → same shape.
  - `.gotrue_parse(resp_body)` → internal parser (pure; unit-tested).

- [ ] **Step 1: Write `tests/test_supabase_client.R`** (test only the pure parser + config gate)

```r
library(testthat)
source(file.path("R", "app_config.R"))
source(file.path("R", "supabase_client.R"))

test_that("gotrue parser extracts user id and token", {
  body <- list(access_token = "tok", user = list(id = "u-123"))
  parsed <- .gotrue_parse(body)
  expect_true(parsed$ok)
  expect_equal(parsed$user_id, "u-123")
  expect_equal(parsed$access_token, "tok")
})

test_that("gotrue parser reports errors", {
  parsed <- .gotrue_parse(list(error_description = "bad creds"))
  expect_false(parsed$ok)
  expect_match(parsed$error, "bad creds")
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `Rscript tests/test_supabase_client.R`

- [ ] **Step 3: Implement `R/supabase_client.R`**

```r
supabase_configured <- function() {
  vars <- c("SUPABASE_DB_HOST","SUPABASE_DB_PORT","SUPABASE_DB_NAME",
            "SUPABASE_DB_USER","SUPABASE_DB_PASSWORD","SUPABASE_URL","SUPABASE_ANON_KEY")
  all(nzchar(Sys.getenv(vars)))
}

supabase_connect <- function() {
  if (!requireNamespace("RPostgres", quietly = TRUE))
    stop("RPostgres is required for Supabase persistence.")
  DBI::dbConnect(RPostgres::Postgres(),
    host = Sys.getenv("SUPABASE_DB_HOST"),
    port = as.integer(Sys.getenv("SUPABASE_DB_PORT")),
    dbname = Sys.getenv("SUPABASE_DB_NAME"),
    user = Sys.getenv("SUPABASE_DB_USER"),
    password = Sys.getenv("SUPABASE_DB_PASSWORD"),
    sslmode = "require")
}

.gotrue_parse <- function(body) {
  if (!is.null(body$access_token) && !is.null(body$user)) {
    return(list(ok = TRUE, user_id = body$user$id,
                access_token = body$access_token, error = NA_character_))
  }
  msg <- body$error_description %||% body$msg %||% body$error %||% "authentication failed"
  list(ok = FALSE, user_id = NA_character_, access_token = NA_character_, error = msg)
}

.gotrue_request <- function(path, email, password) {
  url <- paste0(Sys.getenv("SUPABASE_URL"), "/auth/v1/", path)
  resp <- httr2::request(url) |>
    httr2::req_headers(apikey = Sys.getenv("SUPABASE_ANON_KEY"),
                       "Content-Type" = "application/json") |>
    httr2::req_body_json(list(email = email, password = password)) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  .gotrue_parse(httr2::resp_body_json(resp))
}

gotrue_sign_in <- function(email, password) .gotrue_request("token?grant_type=password", email, password)
gotrue_sign_up <- function(email, password) .gotrue_request("signup", email, password)
```

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_supabase_client.R`

- [ ] **Step 5: Commit**

```bash
git add R/supabase_client.R tests/test_supabase_client.R
git commit -m "feat: supabase client — postgres connect + GoTrue auth REST"
```

---

## Task 13: Storage interface (guest + supabase backends)

**Files:**
- Create: `R/storage.R`
- Test: `tests/test_storage_guest.R`

**Interfaces:**
- Consumes: `supabase_connect()`, `DB_TABLES`, `new_event()`, `fold_events()`, `game_to_json()`.
- Produces a storage object created by `make_storage(backend = c("guest","supabase"), con = NULL, user_id = NULL)`, with methods:
  - `$append_event(game_id, evt)` → evt with assigned integer `seq`.
  - `$load_events(game_id)` → list of events.
  - `$create_game(meta)` → game_id.
  - `$list_games()` → data.frame(game_id, name, status, updated_at).
  - `$save_snapshot(game_id, state)` → invisible.
  The **guest** backend stores everything in an environment (in-memory); **supabase** backend uses SQL. Slice-one tests cover the guest backend fully; the supabase backend is exercised manually in Task 17 integration.

- [ ] **Step 1: Write `tests/test_storage_guest.R`**

```r
library(testthat)
for (f in c("app_config.R","game_events.R","rules_engine.R","game_reducer.R","json_io.R","storage.R"))
  source(file.path("R", f))

test_that("guest storage assigns increasing seq and reloads events", {
  st <- make_storage("guest")
  gid <- st$create_game(list(name = "Test"))
  e1 <- st$append_event(gid, new_event("count_override", list(balls=0L,strikes=0L)))
  e2 <- st$append_event(gid, new_event("count_override", list(balls=1L,strikes=0L)))
  expect_equal(e1$seq, 1L); expect_equal(e2$seq, 2L)
  evs <- st$load_events(gid)
  expect_equal(length(evs), 2L)
})

test_that("guest storage lists games", {
  st <- make_storage("guest")
  st$create_game(list(name = "G1"))
  expect_equal(nrow(st$list_games()), 1L)
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `Rscript tests/test_storage_guest.R`

- [ ] **Step 3: Implement `R/storage.R`**

```r
make_storage <- function(backend = c("guest","supabase"), con = NULL, user_id = NULL) {
  backend <- match.arg(backend)
  if (backend == "guest") return(.guest_storage())
  .supabase_storage(con = con, user_id = user_id)
}

.guest_storage <- function() {
  env <- new.env(parent = emptyenv())
  env$games <- list(); env$events <- list()
  list(
    create_game = function(meta = list()) {
      gid <- uuid::UUIDgenerate()
      env$games[[gid]] <- list(game_id = gid, name = meta$name %||% "Untitled",
        status = "in_progress", updated_at = NA_character_)
      env$events[[gid]] <- list(); gid
    },
    append_event = function(game_id, evt) {
      cur <- env$events[[game_id]] %||% list()
      evt$seq <- length(cur) + 1L
      env$events[[game_id]] <- c(cur, list(evt)); evt
    },
    load_events = function(game_id) env$events[[game_id]] %||% list(),
    save_snapshot = function(game_id, state) invisible(NULL),
    list_games = function() {
      if (length(env$games) == 0)
        return(data.frame(game_id=character(), name=character(),
                          status=character(), updated_at=character()))
      do.call(rbind, lapply(env$games, function(g) as.data.frame(g, stringsAsFactors = FALSE)))
    }
  )
}

.supabase_storage <- function(con, user_id) {
  stopifnot(!is.null(con), !is.null(user_id))
  list(
    create_game = function(meta = list()) {
      gid <- uuid::UUIDgenerate()
      DBI::dbExecute(con,
        "insert into games (id, owner_id, played_on, location, status) values ($1,$2,$3,$4,'in_progress')",
        params = list(gid, user_id, meta$played_on %||% NA, meta$location %||% NA))
      gid
    },
    append_event = function(game_id, evt) {
      nxt <- DBI::dbGetQuery(con,
        "select coalesce(max(seq),0)+1 as n from game_events where game_id=$1",
        params = list(game_id))$n[[1]]
      evt$seq <- as.integer(nxt)
      DBI::dbExecute(con,
        "insert into game_events (game_id, seq, type, payload) values ($1,$2,$3,$4)",
        params = list(game_id, evt$seq, evt$type,
                      jsonlite::toJSON(evt$payload, auto_unbox = TRUE, null="null", na="null")))
      DBI::dbExecute(con, "update games set updated_at=now() where id=$1", params=list(game_id))
      evt
    },
    load_events = function(game_id) {
      df <- DBI::dbGetQuery(con,
        "select seq, type, payload from game_events where game_id=$1 order by seq",
        params = list(game_id))
      lapply(seq_len(nrow(df)), function(i)
        new_event(df$type[i], jsonlite::fromJSON(df$payload[i], simplifyVector = FALSE),
                  seq = as.integer(df$seq[i])))
    },
    save_snapshot = function(game_id, state) {
      DBI::dbExecute(con, "update games set state_snapshot=$1, status=$2, updated_at=now() where id=$3",
        params = list(jsonlite::toJSON(state, auto_unbox=TRUE, null="null", na="null"),
                      state$status %||% "in_progress", game_id))
      invisible(NULL)
    },
    list_games = function() {
      DBI::dbGetQuery(con,
        "select id as game_id, coalesce(location,'Game') as name, status, updated_at::text
           from games where owner_id=$1 order by updated_at desc", params = list(user_id))
    }
  )
}
```

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_storage_guest.R`

- [ ] **Step 5: Commit**

```bash
git add R/storage.R tests/test_storage_guest.R
git commit -m "feat: storage interface with guest and supabase backends"
```

---

## Task 14: Auth module

**Files:**
- Create: `R/auth_module.R`
- Test: `tests/test_auth_module.R` (module server logic via `shiny::testServer`)

**Interfaces:**
- Consumes: `gotrue_sign_in()`, `gotrue_sign_up()`.
- Produces:
  - `auth_ui(id)` → UI with email/password inputs, Sign in / Sign up / Continue as guest buttons, and an error output.
  - `auth_server(id, sign_in = gotrue_sign_in, sign_up = gotrue_sign_up)` → returns a `reactive` of the session identity: `list(mode = "guest"|"user", user_id = NA|<id>, access_token = NA|<tok>)`. The `sign_in`/`sign_up` function args are injected so tests can pass fakes.

- [ ] **Step 1: Write `tests/test_auth_module.R`**

```r
library(testthat); library(shiny)
source(file.path("R","app_config.R")); source(file.path("R","auth_module.R"))

test_that("successful sign-in yields a user identity", {
  fake_ok <- function(email, password) list(ok=TRUE, user_id="u1", access_token="t1", error=NA)
  testServer(auth_server, args = list(sign_in = fake_ok), {
    session$setInputs(email = "a@b.com", password = "pw", do_sign_in = 1)
    expect_equal(identity()$mode, "user")
    expect_equal(identity()$user_id, "u1")
  })
})

test_that("continue as guest yields guest identity", {
  testServer(auth_server, {
    session$setInputs(do_guest = 1)
    expect_equal(identity()$mode, "guest")
  })
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `Rscript tests/test_auth_module.R`

- [ ] **Step 3: Implement `R/auth_module.R`**

```r
auth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Sign in to save your games"),
    textInput(ns("email"), "Email"),
    passwordInput(ns("password"), "Password"),
    div(class = "d-flex gap-2 flex-wrap",
      actionButton(ns("do_sign_in"), "Sign in", class = "btn-primary"),
      actionButton(ns("do_sign_up"), "Create account", class = "btn-outline-secondary"),
      actionButton(ns("do_guest"), "Continue as guest", class = "btn-link")),
    div(class = "text-danger small mt-2", textOutput(ns("err")))
  )
}

auth_server <- function(id, sign_in = gotrue_sign_in, sign_up = gotrue_sign_up) {
  moduleServer(id, function(input, output, session) {
    identity <- reactiveVal(list(mode = NA_character_, user_id = NA_character_, access_token = NA_character_))
    err <- reactiveVal("")

    handle <- function(fn) {
      res <- fn(input$email, input$password)
      if (isTRUE(res$ok)) {
        identity(list(mode = "user", user_id = res$user_id, access_token = res$access_token))
        err("")
      } else err(res$error %||% "Authentication failed")
    }
    observeEvent(input$do_sign_in, handle(sign_in))
    observeEvent(input$do_sign_up, handle(sign_up))
    observeEvent(input$do_guest,
      identity(list(mode = "guest", user_id = NA_character_, access_token = NA_character_)))

    output$err <- renderText(err())
    identity
  })
}
```

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_auth_module.R`

- [ ] **Step 5: Commit**

```bash
git add R/auth_module.R tests/test_auth_module.R
git commit -m "feat: auth module (sign in/up, guest) with injectable auth fns"
```

---

## Task 15: Setup module (league / ruleset / teams / lineups)

**Files:**
- Create: `R/setup_module.R`
- Test: `tests/test_setup_module.R`

**Interfaces:**
- Consumes: `default_ruleset_config()`, `coerce_ruleset_config()`, `validate_ruleset_config()`, `make_player()`, `new_event()`.
- Produces:
  - `setup_ui(id)` → mobile-first form: ruleset controls (starting count, foul-out, gender rule, min females, innings, run cap, mercy), two team name fields, and a lineup editor (add player rows: name, gender, jersey, order, position).
  - `setup_server(id)` → returns `reactive` producing a `"game_start"` event via `build_game_start_event(ruleset, home, away, first_bat)`.
  - Pure helper `build_game_start_event(ruleset, home, away, first_bat = "away")` → validated `game_start` event (this is the unit-tested part).

- [ ] **Step 1: Write `tests/test_setup_module.R`**

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","setup_module.R"))
  source(file.path("R", f))

test_that("build_game_start_event assembles a valid event", {
  home <- list(team_id="H", name="Home",
    lineup = list(make_player("h1","H1","M",1L,1L,6L)))
  away <- list(team_id="A", name="Away",
    lineup = list(make_player("a1","A1","F",1L,1L,4L)))
  evt <- build_game_start_event(default_ruleset_config(), home, away, "away")
  expect_equal(evt$type, "game_start")
  expect_equal(evt$payload$first_bat, "away")
  expect_true(validate_event(evt)$ok)
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `Rscript tests/test_setup_module.R`

- [ ] **Step 3: Implement `R/setup_module.R`** (pure helper first, then UI/server)

```r
build_game_start_event <- function(ruleset, home, away, first_bat = "away") {
  ruleset <- coerce_ruleset_config(ruleset)
  v <- validate_ruleset_config(ruleset)
  if (!v$ok) stop(paste("Invalid ruleset:", paste(v$errors, collapse = "; ")))
  new_event("game_start", list(ruleset = ruleset, first_bat = first_bat,
    home = home, away = away), seq = 1L)
}

setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("New game"),
    layout_columns(col_widths = c(6,6),
      numericInput(ns("start_balls"), "Starting balls", 1, 0, 3),
      numericInput(ns("start_strikes"), "Starting strikes", 1, 0, 2)),
    selectInput(ns("foul_out"), "Foul with 2 strikes",
      c("Out" = "out", "One courtesy foul" = "one_courtesy_foul")),
    selectInput(ns("gender_rule"), "Batting gender rule",
      c("None" = "none", "No two males in a row" = "no_two_males_consecutive",
        "Every other" = "every_other", "At least one F every N" = "every_n")),
    numericInput(ns("gender_n"), "N (for 'every N')", 2, 2, 12),
    numericInput(ns("min_females"), "Min females in field", 0, 0, 10),
    layout_columns(col_widths = c(4,4,4),
      numericInput(ns("innings"), "Innings", 7, 1, 12),
      numericInput(ns("run_cap"), "Run cap/inning (0 = none)", 0, 0, 30),
      numericInput(ns("mercy_diff"), "Mercy differential (0 = none)", 0, 0, 50)),
    textInput(ns("away_name"), "Away team", "Away"),
    textInput(ns("home_name"), "Home team", "Home"),
    tags$p(class="text-muted small",
      "Add lineups on the next screen; a quick default lineup is created for now."),
    actionButton(ns("start"), "Start game", class = "btn-primary bw-outcome-btn")
  )
}

setup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    game_start <- reactiveVal(NULL)
    observeEvent(input$start, {
      cfg <- coerce_ruleset_config(list(
        starting_count = list(balls = input$start_balls, strikes = input$start_strikes),
        foul_out_rule = input$foul_out,
        batting_gender_rule = list(type = input$gender_rule, n = input$gender_n),
        fielding = list(min_females = input$min_females, position_requirements = list()),
        innings = input$innings,
        run_cap_per_inning = if (input$run_cap > 0) input$run_cap else NA_integer_,
        mercy_rule = list(differential = if (input$mercy_diff > 0) input$mercy_diff else NA_integer_,
                          after_inning = 1L)))
      mk <- function(prefix)
        lapply(1:4, function(i) make_player(uuid::UUIDgenerate(),
          paste(prefix, i), c("M","F","M","F")[i], i, i, i))
      home <- list(team_id = uuid::UUIDgenerate(), name = input$home_name, lineup = mk("Home"))
      away <- list(team_id = uuid::UUIDgenerate(), name = input$away_name, lineup = mk("Away"))
      game_start(build_game_start_event(cfg, home, away, "away"))
    })
    game_start
  })
}
```

(Full roster editing UI is Phase 3; slice one seeds a default 4-batter lineup so the tracking loop is exercisable. This is documented in the spec's non-goals.)

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_setup_module.R`

- [ ] **Step 5: Commit**

```bash
git add R/setup_module.R tests/test_setup_module.R
git commit -m "feat: setup module — ruleset form and game_start event builder"
```

---

## Task 16: Tracking module (mobile-first live scoring)

**Files:**
- Create: `R/tracking_module.R`
- Test: `tests/test_tracking_module.R`

**Interfaces:**
- Consumes: `fold_events()`, `suggest_advances()`, `new_event()`, `render_scorebook_svg()`, `line_score()`, `batting_lines()`, a storage object.
- Produces:
  - `tracking_ui(id)` → header (inning/half/outs/count + mini-diamond via `uiOutput`), outcome button grid, undo button, substitution button, and tabs for Scorebook and Box score.
  - `tracking_server(id, storage, game_id, game_start_event)` → drives event append + re-fold; exposes `state` reactive.
  - Pure helper `record_outcome_event(state, outcome, team)` → a `plate_appearance` event using `suggest_advances` defaults and derived `reached`/`outs_on_play`/`rbi` (this is unit-tested).

- [ ] **Step 1: Write `tests/test_tracking_module.R`**

```r
library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R","tracking_module.R"))
  source(file.path("R", f))
mk_lineup <- function(prefix) lapply(1:4, function(i)
  make_player(paste0(prefix,i), paste(prefix,i), c("M","F","M","F")[i], i, i, i))
start <- new_event("game_start", list(ruleset=default_ruleset_config(), first_bat="away",
  home=list(team_id="H",name="Home",lineup=mk_lineup("h")),
  away=list(team_id="A",name="Away",lineup=mk_lineup("a"))), seq=1L)

test_that("record_outcome_event for 1B puts batter on first with reached=1", {
  s <- fold_events(list(start))
  e <- record_outcome_event(s, "1B", "away")
  expect_equal(e$payload$reached, 1L)
  expect_equal(e$payload$outs_on_play, 0L)
  s2 <- fold_events(list(start, e))
  expect_equal(s2$bases$first, "a1")
})

test_that("record_outcome_event for K records one out", {
  s <- fold_events(list(start))
  e <- record_outcome_event(s, "K", "away")
  expect_equal(e$payload$outs_on_play, 1L)
  expect_true(is.na(e$payload$reached))
})
```

- [ ] **Step 2: Run test, expect FAIL**

Run: `Rscript tests/test_tracking_module.R`

- [ ] **Step 3: Implement `R/tracking_module.R`**

```r
.OUT_OUTCOMES <- c("K","KL","GO","FO","LO","PO")

record_outcome_event <- function(state, outcome, team) {
  reached <- switch(outcome, "1B"=1L,"2B"=2L,"3B"=3L,"HR"=4L,
                    "BB"=1L,"IBB"=1L,"HBP"=1L,"FC"=1L,"E"=1L, NA_integer_)
  outs_on_play <- if (outcome %in% .OUT_OUTCOMES) 1L else 0L
  advances <- suggest_advances(state, outcome)
  # suggest_advances already includes the batter's own advance (scored on a HR),
  # so RBIs are simply the count of scored advances — do NOT add a separate
  # reached==4 bonus or the batter's HR run would be double-counted.
  rbi <- sum(vapply(advances, function(a) isTRUE(a$scored), logical(1)))
  new_event("plate_appearance", list(team = team,
    batter_id = state$current_batter$player_id, outcome = outcome,
    reached = reached, rbi = as.integer(rbi), outs_on_play = outs_on_play,
    advances = advances))
}

tracking_ui <- function(id) {
  ns <- NS(id)
  outcomes <- c("1B","2B","3B","HR","BB","K","GO","FO","FC","E")
  btns <- lapply(outcomes, function(o)
    actionButton(ns(paste0("o_", o)), o, class = "btn-outline-primary bw-outcome-btn"))
  tagList(
    uiOutput(ns("situation")),
    div(class = "bw-outcome-grid d-grid",
        style = "grid-template-columns: repeat(5,1fr); gap:.5rem;", !!!btns),
    div(class = "d-flex gap-2 mt-2",
        actionButton(ns("undo"), "Undo", class = "btn-warning"),
        actionButton(ns("sub"), "Substitution", class = "btn-outline-secondary")),
    navset_tab(
      nav_panel("Scorebook", uiOutput(ns("scorebook"))),
      nav_panel("Box score", tableOutput(ns("box_away")), tableOutput(ns("box_home"))))
  )
}

tracking_server <- function(id, storage, game_id, game_start_event) {
  moduleServer(id, function(input, output, session) {
    events <- reactiveVal(NULL)
    isolate({
      appended <- storage$append_event(game_id, game_start_event)
      events(storage$load_events(game_id))
    })
    state <- reactive(fold_events(events()))

    record <- function(outcome) {
      s <- isolate(state())
      if (identical(s$status, "final")) return(invisible())
      evt <- record_outcome_event(s, outcome, s$batting_team)
      appended <- storage$append_event(game_id, evt)
      # The local events() list is the in-session source of truth. Append to it
      # directly (do NOT reload from storage) so an undo — which truncates events()
      # — is not resurrected on the next append. (Storage rows for undone events are
      # not pruned in slice one; that only affects a full reload/resume, per README.)
      events(c(isolate(events()), list(appended)))
      storage$save_snapshot(game_id, isolate(state()))
    }
    outcomes <- c("1B","2B","3B","HR","BB","K","GO","FO","FC","E")
    lapply(outcomes, function(o)
      observeEvent(input[[paste0("o_", o)]], record(o), ignoreInit = TRUE))

    observeEvent(input$undo, {
      ev <- events()
      if (length(ev) > 1) {   # never drop game_start
        # Rebuild by re-appending all but the last into a fresh guest-ish reload:
        # simplest correct approach — reload, drop last, persist via snapshot only.
        ev <- ev[-length(ev)]
        events(ev)
        storage$save_snapshot(game_id, fold_events(ev))
      }
    }, ignoreInit = TRUE)

    output$situation <- renderUI({
      s <- state()
      tags$div(class = "d-flex justify-content-between align-items-center",
        tags$div(sprintf("Inning %d %s • %d out • %d-%d",
          s$inning, s$half, s$outs, s$count$balls, s$count$strikes)),
        tags$div(sprintf("%s: away %d – home %d",
          if (identical(s$status,"final")) "FINAL" else "Score", s$score$away, s$score$home)),
        if (!is.null(s$current_batter)) tags$strong(s$current_batter$name))
    })
    output$scorebook <- renderUI(render_scorebook_svg(state(), state()$batting_team))
    output$box_away <- renderTable(batting_lines(state(), "away"))
    output$box_home <- renderTable(batting_lines(state(), "home"))

    state
  })
}
```

Note on Undo + Supabase: dropping the last event in the reactive is sufficient for slice one (the fold + snapshot stays correct). Deleting the persisted `game_events` row is a Phase-3 refinement; document this limitation in `README.md` (Task 18).

- [ ] **Step 4: Run test, expect PASS**

Run: `Rscript tests/test_tracking_module.R`

- [ ] **Step 5: Commit**

```bash
git add R/tracking_module.R tests/test_tracking_module.R
git commit -m "feat: mobile-first tracking module with outcome buttons and undo"
```

---

## Task 17: App assembly & navigation

**Files:**
- Modify: `app.R`, `global.R`
- Create: `R/session_flow.R` (helper: choose storage backend from identity)

**Interfaces:**
- Consumes: `auth_ui/server`, `setup_ui/server`, `tracking_ui/server`, `make_storage`, `supabase_configured`, `supabase_connect`.
- Produces: `storage_for_identity(identity)` → a storage object (guest or supabase). App wires screens: **auth → setup → tracking**, with a persistent guest banner.

- [ ] **Step 1: Write `R/session_flow.R`**

```r
storage_for_identity <- function(identity) {
  if (identical(identity$mode, "user") && supabase_configured()) {
    con <- supabase_connect()
    return(list(storage = make_storage("supabase", con = con, user_id = identity$user_id),
                con = con))
  }
  list(storage = make_storage("guest"), con = NULL)
}
```

- [ ] **Step 2: Rewrite `app.R` to wire the flow**

```r
source("global.R")

ui <- page_fillable(
  title = APP_CONFIG$app_name, theme = app_theme, padding = 0,
  tags$head(tags$link(rel = "stylesheet", href = "css/app.css")),
  uiOutput("guest_banner"),
  navset_hidden(id = "screen",
    nav_panel_hidden("auth", div(class="p-3", auth_ui("auth"))),
    nav_panel_hidden("setup", div(class="p-3", setup_ui("setup"))),
    nav_panel_hidden("track", div(class="p-2", tracking_ui("track")))
  )
)

server <- function(input, output, session) {
  identity <- auth_server("auth")
  store <- reactiveVal(NULL)
  game_start <- setup_server("setup")

  observeEvent(identity(), {
    req(!is.na(identity()$mode))
    sf <- storage_for_identity(identity())
    store(sf$storage)
    if (!is.null(sf$con)) onStop(function() DBI::dbDisconnect(sf$con))
    nav_hide("screen"); nav_select("screen", "setup")
  }, ignoreInit = TRUE)

  output$guest_banner <- renderUI({
    req(!is.null(identity()))
    if (identical(identity()$mode, "guest"))
      div(class = "alert alert-warning m-2 py-2 small",
          "Guest mode: sign in to save. Refreshing will lose this game.")
  })

  observeEvent(game_start(), {
    req(store(), game_start())
    gid <- store()$create_game(list(name = "Game"))
    tracking_server("track", store(), gid, game_start())
    nav_select("screen", "track")
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
```

- [ ] **Step 3: Boot & manual smoke test (guest path, no Supabase needed)**

Run: `Rscript -e "shiny::runApp('.', port = 8100, launch.browser = TRUE)"`
Verify by hand:
1. Auth screen → click **Continue as guest** → guest banner appears, setup screen shows.
2. Click **Start game** → tracking screen; situation panel shows "Inning 1 top • 0 out • 1-1".
3. Tap `1B` → batter advances, situation updates, Scorebook tab shows a diamond.
4. Tap `K` three times across batters → half flips to "bottom".
5. **Undo** → last event reverts.
Expected: no console errors; all five behaviors hold.

- [ ] **Step 4: Commit**

```bash
git add app.R global.R R/session_flow.R
git commit -m "feat: wire auth -> setup -> tracking flow with guest banner"
```

---

## Task 18: Deployment manifest, README, and full test sweep

**Files:**
- Create: `README.md`, `manifest.json` (generated), `run_tests.R`

**Interfaces:** none (project docs + CI convenience).

- [ ] **Step 1: Write `run_tests.R`** (runs every `tests/test_*.R`)

```r
files <- list.files("tests", pattern = "^test_.*\\.R$", full.names = TRUE)
fail <- FALSE
for (f in files) {
  cat("\n== ", f, " ==\n"); r <- tryCatch({ source(f); TRUE },
    error = function(e) { message(conditionMessage(e)); FALSE })
  if (!r) fail <- TRUE
}
if (fail) quit(status = 1)
```

- [ ] **Step 2: Run the full sweep, expect all pass**

Run: `Rscript run_tests.R`
Expected: every test file prints and no `[ FAIL > 0 ]`; exit status 0.

- [ ] **Step 3: Write `README.md`**

```markdown
# Bookworm

Softball/baseball scorebook tracking, built with R Shiny for Posit Connect Cloud.

## Run locally
1. `cp .Renviron.template .Renviron` and fill in Supabase values (or skip — guest mode works without them).
2. Apply `data-raw/supabase_schema.sql` in Supabase (see `data-raw/README.md`).
3. `Rscript -e "shiny::runApp('.')"`

## Architecture
Event-sourced core: a game is an append-only list of events; a pure reducer
(`R/game_reducer.R`) folds them into state. Scorebook (`R/scorebook_render.R`),
box score (`R/boxscore.R`), and stats are derived views. Storage
(`R/storage.R`) has guest (in-memory) and Supabase backends.

## Tests
`Rscript run_tests.R`

## Known slice-one limitations
- Undo reverts in-session; persisted event rows are pruned in a later phase.
- Lineups are seeded with a default 4-batter order; full roster editing is Phase 3.
- Row-Level Security is defined but not enforced (app-level owner scoping).

## Roadmap
- Phase 2: vision-LLM photo import.
- Phase 3: team management, sharing, standings, RLS.
```

- [ ] **Step 4: Generate the Posit Connect manifest**

Run: `Rscript -e "rsconnect::writeManifest()"`
Expected: `manifest.json` created listing `app.R` and dependencies. (If `rsconnect` is absent, install it: `Rscript -e "install.packages('rsconnect')"`.)

- [ ] **Step 5: Commit**

```bash
git add README.md manifest.json run_tests.R
git commit -m "chore: test runner, README, and Posit Connect manifest"
```

---

## Self-Review notes (for the executor)

- **Spec coverage:** rules engine (T2/T6), event-sourced core (T3–T7), scorebook (T10), box score (T8), Supabase persistence + schema (T11–T13), auth + guest (T14, T17), substitutions (T7), JSON export/import (T9), mobile-first tracking (T16). All spec sections map to a task.
- **Interface consistency:** `fold_events`, `apply_event`, `suggest_advances`, `record_outcome_event`, `make_storage(...)$append_event/load_events/create_game/save_snapshot/list_games`, `build_game_start_event`, `gotrue_sign_in/up`, `.gotrue_parse` are named identically wherever referenced across tasks.
- **Deferred-with-note (not placeholders):** persisted-event pruning on undo, full roster editor, RLS enforcement, and photo import are explicitly out of slice-one scope per the spec's non-goals and are called out in `README.md`.
