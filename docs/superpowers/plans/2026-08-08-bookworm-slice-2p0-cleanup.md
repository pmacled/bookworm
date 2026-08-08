# Bookworm Slice 2.0 — Cleanup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the leftover agriculture-app branding with a scorebook theme, document every outcome code, make the box score readable and sortable, and stop the app from crashing on authentication failures.

**Architecture:** All changes are additive or local to a single file. No event-schema or reducer changes. `_brand.yml` remains the single source of colour truth, parsed once by `R/brand_colors.R` into `BRAND_COLORS`. Outcome-code metadata moves into `APP_CONFIG$outcome_meta` and `outcome_codes` is derived from it so the two cannot drift.

**Tech Stack:** R 4.5.3, Shiny 1.13.0, bslib 0.10.0, DT 0.34.0, testthat 3.3.2, yaml 2.3.12.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-08-08-bookworm-slice-two-design.md`, section "Slice 2.0".
- Run every command from the **project root**. `R/brand_colors.R` reads `_brand.yml` by relative path and will fail elsewhere.
- Rscript is not on PATH. Use the absolute path: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe"` (Bash tool) or `& 'C:\Program Files\R\R-4.5.3\bin\Rscript.exe'` (PowerShell).
- Full suite: `Rscript run_tests.R`. It must exit 0 at the end of **every task**, not just every slice.
- Tests are plain `testthat` files sourced in a loop by `run_tests.R`. Follow the existing convention: `library(testthat)` then `source(file.path("R", "<file>.R"))` for each dependency, then `test_that()` blocks. There is no `testthat::test_dir()` setup and no `helper-*.R`.
- Do not modify any file owned by slice 2.1 (`R/rules_engine.R`, `R/rule_presets.R`, `R/setup_module.R`), which runs in parallel with this slice.
- `R/app_config.R` is touched by this slice only. Slice 2.1 consumes `outcome_meta` but does not edit it.
- Colour values are exact. Copy the hex codes verbatim from the tables below.

---

### Task 1: "Ink on Paper" palette

Replaces the `field-green`/`clay-orange` palette and deletes the agriculture-app leftovers (`usda_nass_navy`, `weatherlink-navy`, `accent-teal`). A grep confirmed `BRAND_COLORS$accent` and `BRAND_COLORS$usda_nass_navy` have **zero** consumers, so removing them is safe.

**Files:**
- Modify: `_brand.yml` (whole file)
- Modify: `R/brand_colors.R:1-42`
- Modify: `R/scorebook_render.R:42` (uses `BRAND_COLORS$secondary` for the grid; switch to the new `rule_line`)
- Test: `tests/test_brand.R` (create)

**Interfaces:**
- Consumes: nothing.
- Produces: `BRAND_COLORS`, a named list with exactly these keys, each a `#RRGGBB` string —
  `primary`, `primary_light`, `secondary`, `success`, `warning`, `warning_light`,
  `danger`, `danger_light`, `foreground`, `background`, `surface`, `light`, `dark`,
  `rule_line`. Slice 2.3's scorebook renderer relies on `primary_light`, `rule_line`,
  `dark`, `secondary`, and `danger`.

- [ ] **Step 1: Write the failing test**

Create `tests/test_brand.R`:

```r
library(testthat)
# brand_colors.R reads _brand.yml by relative path; run from the project root.
source(file.path("R", "app_config.R"))
source(file.path("R", "brand_colors.R"))

# WCAG 2.1 relative luminance and contrast ratio.
.hex_rgb <- function(hex) strtoi(substring(sub("^#", "", hex), c(1, 3, 5), c(2, 4, 6)), 16L)
.rel_lum <- function(hex) {
  srgb <- .hex_rgb(hex) / 255
  lin <- ifelse(srgb <= 0.03928, srgb / 12.92, ((srgb + 0.055) / 1.055)^2.4)
  sum(lin * c(0.2126, 0.7152, 0.0722))
}
contrast_ratio <- function(a, b) {
  l <- sort(c(.rel_lum(a), .rel_lum(b)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}

test_that("every required brand colour resolves to a hex string", {
  required <- c("primary", "primary_light", "secondary", "success",
                "warning", "warning_light", "danger", "danger_light",
                "foreground", "background", "surface", "light", "dark", "rule_line")
  for (key in required) {
    val <- BRAND_COLORS[[key]]
    expect_true(!is.null(val), info = paste("missing brand colour:", key))
    expect_match(val, "^#[0-9A-Fa-f]{6}$", info = paste("not a hex colour:", key))
  }
})

test_that("agriculture-app leftovers are gone", {
  expect_null(BRAND_COLORS$usda_nass_navy)
  expect_null(BRAND_COLORS$accent)
})

test_that("body text meets WCAG AA on both surfaces", {
  expect_gte(contrast_ratio(BRAND_COLORS$foreground, BRAND_COLORS$background), 4.5)
  expect_gte(contrast_ratio(BRAND_COLORS$secondary,  BRAND_COLORS$surface),    4.5)
  expect_gte(contrast_ratio("#FFFFFF",               BRAND_COLORS$primary),    4.5)
  expect_gte(contrast_ratio(BRAND_COLORS$danger,     BRAND_COLORS$background), 4.5)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_brand.R")'`

Expected: FAIL — `missing brand colour: primary_light` (the current `BRAND_COLORS` has no `surface` or `rule_line`, and `usda_nass_navy` is present).

- [ ] **Step 3: Rewrite `_brand.yml`**

Replace the whole file:

```yaml
meta:
  name: Bookworm
color:
  palette:
    paper: "#FDFCF7"
    paper-shade: "#F4F1E8"
    ink: "#1A2233"
    ink-muted: "#5B6472"
    ink-blue: "#28406B"
    ink-blue-light: "#E4E9F2"
    pencil-red: "#B02A2A"
    pencil-red-light: "#F7E4E4"
    pencil-amber: "#B8791F"
    pencil-amber-light: "#FBF0DC"
    field-green: "#2E7D32"
    rule-blue: "#C6D2E4"
  foreground: ink
  background: paper
  primary: ink-blue
  secondary: ink-muted
  success: field-green
  warning: pencil-amber
  danger: pencil-red
  light: paper-shade
  dark: ink
typography:
  base: system-ui
  headings:
    family: system-ui
    weight: 600
defaults:
  bootstrap:
    enable-rounded: true
    border-radius: "0.375rem"
```

Note the `-light` suffix convention: `.resolve_brand_light("primary")` looks up
`ink-blue-light` because `primary` aliases `ink-blue`. That is why `ink-blue-light`,
`pencil-red-light`, and `pencil-amber-light` are named the way they are — do not rename them.

- [ ] **Step 4: Rewrite `R/brand_colors.R`**

```r
# Brand Colors ----
# Parses _brand.yml and exposes BRAND_COLORS for use across R/ files.
# Must be sourced before any file that references BRAND_COLORS (scorebook_render.R).
# global.R sources brand_colors.R and app_config.R first to guarantee this.

.brand_raw <- yaml::read_yaml("_brand.yml")
.brand_palette <- .brand_raw$color$palette

# Resolves a semantic role (e.g. "primary") through its palette alias to a hex value.
.resolve_brand_color <- function(name) {
  val <- .brand_raw$color[[name]]
  if (is.null(val)) return(NULL)
  .brand_palette[[val]] %||% val
}

# Resolves the "-light" companion of a semantic role, e.g. primary -> ink-blue-light.
.resolve_brand_light <- function(name) {
  alias <- .brand_raw$color[[name]]
  if (is.null(alias)) return(NULL)
  .brand_palette[[paste0(alias, "-light")]]
}

BRAND_COLORS <- list(
  primary       = .resolve_brand_color("primary"),
  primary_light = .resolve_brand_light("primary"),
  secondary     = .resolve_brand_color("secondary"),
  success       = .resolve_brand_color("success"),
  warning       = .resolve_brand_color("warning"),
  warning_light = .resolve_brand_light("warning"),
  danger        = .resolve_brand_color("danger"),
  danger_light  = .resolve_brand_light("danger"),
  foreground    = .resolve_brand_color("foreground"),
  background    = .resolve_brand_color("background"),
  light         = .resolve_brand_color("light"),
  dark          = .resolve_brand_color("dark"),
  # Palette-only entries with no Bootstrap role of their own.
  surface       = .brand_palette[["paper-shade"]],
  rule_line     = .brand_palette[["rule-blue"]]
)

rm(.brand_raw, .brand_palette, .resolve_brand_color, .resolve_brand_light)
```

- [ ] **Step 5: Point the scorebook grid at `rule_line`**

In `R/scorebook_render.R`, the outer grid rectangle currently uses `BRAND_COLORS$secondary`
(a text colour). Change line 42's `BRAND_COLORS$secondary` to `BRAND_COLORS$rule_line`.
Leave everything else in that file alone — slice 2.3 rewrites it.

- [ ] **Step 6: Run the brand test and then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_brand.R")'`
Expected: PASS, 3 tests.

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: exit 0. `test_scorebook_render.R` still passes — it asserts on `polygon` and text
content, not colours.

- [ ] **Step 7: Commit**

```bash
git add _brand.yml R/brand_colors.R R/scorebook_render.R tests/test_brand.R
git commit -m "feat: Ink on Paper theme; drop agriculture-app palette leftovers"
```

---

### Task 2: Outcome-code glossary data

**Files:**
- Modify: `R/app_config.R:15-19`
- Test: `tests/test_app_config.R` (extend)

**Interfaces:**
- Consumes: nothing.
- Produces: `APP_CONFIG$outcome_meta` — a named list keyed by outcome code, each entry
  `list(label = <chr>, description = <chr>, category = <chr>)` with category in
  `c("hit", "on_base", "out", "other")`. `APP_CONFIG$outcome_codes` becomes
  `names(APP_CONFIG$outcome_meta)`. Tasks 3 and 4 read `outcome_meta`. Slice 2.1's
  home-run rule reads the `HR` and `ITPHR` codes.

- [ ] **Step 1: Write the failing test**

Append to `tests/test_app_config.R`:

```r
test_that("outcome_codes is derived from outcome_meta", {
  expect_identical(APP_CONFIG$outcome_codes, names(APP_CONFIG$outcome_meta))
  expect_length(APP_CONFIG$outcome_codes, 18L)
  expect_true("ITPHR" %in% APP_CONFIG$outcome_codes)
})

test_that("every outcome has a label, description, and valid category", {
  valid <- c("hit", "on_base", "out", "other")
  for (code in names(APP_CONFIG$outcome_meta)) {
    m <- APP_CONFIG$outcome_meta[[code]]
    expect_true(nzchar(m$label %||% ""),       info = paste(code, "needs a label"))
    expect_true(nzchar(m$description %||% ""), info = paste(code, "needs a description"))
    expect_true(m$category %in% valid,         info = paste(code, "has category", m$category))
  }
})

test_that("the out categories match the reducer's out list", {
  outs <- names(Filter(function(m) identical(m$category, "out"), APP_CONFIG$outcome_meta))
  expect_setequal(outs, c("K", "KL", "GO", "FO", "LO", "PO"))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_app_config.R")'`
Expected: FAIL — `APP_CONFIG$outcome_meta` is NULL, so `names()` returns NULL.

- [ ] **Step 3: Add `outcome_meta` to `R/app_config.R`**

Replace the `outcome_codes = c(...)` entry in `APP_CONFIG` with an `outcome_meta` list,
then derive `outcome_codes` after the list is constructed:

```r
  outcome_meta = list(
    "1B"    = list(label = "Single", category = "hit",
                   description = "Batter reaches first base safely on a batted ball."),
    "2B"    = list(label = "Double", category = "hit",
                   description = "Batter reaches second base safely on a batted ball."),
    "3B"    = list(label = "Triple", category = "hit",
                   description = "Batter reaches third base safely on a batted ball."),
    "HR"    = list(label = "Home run (over the fence)", category = "hit",
                   description = "Ball leaves the park in fair territory. Counts toward a league home-run limit."),
    "ITPHR" = list(label = "Inside-the-park home run", category = "hit",
                   description = "Batter circles the bases on a ball that stays in play. Normally exempt from a home-run limit."),
    "BB"    = list(label = "Walk", category = "on_base",
                   description = "Four balls; batter is awarded first base. Not an at-bat."),
    "IBB"   = list(label = "Intentional walk", category = "on_base",
                   description = "Defence deliberately awards first base. Not an at-bat."),
    "HBP"   = list(label = "Hit by pitch", category = "on_base",
                   description = "Pitch strikes the batter; awarded first base. Not an at-bat."),
    "FC"    = list(label = "Fielder's choice", category = "on_base",
                   description = "Batter reaches because the defence chose to retire another runner."),
    "E"     = list(label = "Reached on error", category = "on_base",
                   description = "Batter reaches because of a defensive misplay. Counts as an at-bat, not a hit."),
    "K"     = list(label = "Strikeout swinging", category = "out",
                   description = "Third strike with the batter swinging."),
    "KL"    = list(label = "Strikeout looking", category = "out",
                   description = "Third strike called with the batter not swinging."),
    "GO"    = list(label = "Ground out", category = "out",
                   description = "Ground ball fielded and thrown out."),
    "FO"    = list(label = "Fly out", category = "out",
                   description = "Fly ball caught in the air."),
    "LO"    = list(label = "Line out", category = "out",
                   description = "Line drive caught in the air."),
    "PO"    = list(label = "Pop out", category = "out",
                   description = "Short, high pop-up caught in the air."),
    "SF"    = list(label = "Sacrifice fly", category = "other",
                   description = "Fly out that scores a runner. Not an at-bat; the batter is credited an RBI."),
    "SAC"   = list(label = "Sacrifice bunt", category = "other",
                   description = "Bunt out that advances a runner. Not an at-bat.")
  )
)

# Derived so the code list and the glossary can never drift apart.
APP_CONFIG$outcome_codes <- names(APP_CONFIG$outcome_meta)
```

Note the closing `)` above closes the `APP_CONFIG <- list(` call; `outcome_meta` is the
last entry. The `outcome_codes` assignment comes after.

- [ ] **Step 4: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_app_config.R")'`
Expected: PASS.

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: exit 0. `validate_event()` reads `APP_CONFIG$outcome_codes`, which now contains a
superset of the old codes, so no existing test regresses.

- [ ] **Step 5: Commit**

```bash
git add R/app_config.R tests/test_app_config.R
git commit -m "feat: outcome_meta glossary; add ITPHR; derive outcome_codes"
```

---

### Task 3: Help panel and outcome-button popovers

> **Partially superseded — historical record.** This task shipped as written, but the
> final whole-branch review found `bslib::popover()` resolves its trigger to `"click"` on
> a `<button>`, so tapping an outcome both recorded the play and popped the description
> over the grid. The owner ruled the per-button popovers out; `outcome_button()` and its
> tests were deleted in the final fix wave, and the action panel builds plain
> `actionButton`s. The `outcome_help_ui()` Help panel below is unchanged and still current.

**Files:**
- Modify: `R/tracking_module.R:36-48` (`tracking_ui`), `R/tracking_module.R:91-106` (`output$action_panel`)
- Create: `R/outcome_help.R`
- Test: `tests/test_outcome_help.R` (create)

**Interfaces:**
- Consumes: `APP_CONFIG$outcome_meta` (Task 2).
- Produces:
  - `outcome_help_ui()` → a `htmltools::tagList` listing every code grouped by category.
    No arguments; reads `APP_CONFIG` directly.
  - `outcome_button(ns_id, code)` → an `actionButton` wrapped in a `bslib::popover`.
    `ns_id` is the already-namespaced input id string; `code` is the outcome code.

Putting these in their own file keeps `tracking_module.R` from growing further — it is
already the busiest UI file and slices 2.2 and 2.3 both add to it.

- [ ] **Step 1: Write the failing test**

Create `tests/test_outcome_help.R`:

```r
library(testthat)
suppressMessages({ library(shiny); library(bslib); library(htmltools) })
source(file.path("R", "app_config.R"))
source(file.path("R", "outcome_help.R"))

test_that("the help panel names every outcome code and its label", {
  html <- as.character(outcome_help_ui())
  for (code in APP_CONFIG$outcome_codes) {
    expect_true(grepl(code, html, fixed = TRUE), info = paste("missing code:", code))
    expect_true(grepl(APP_CONFIG$outcome_meta[[code]]$label, html, fixed = TRUE),
                info = paste("missing label for:", code))
  }
})

test_that("the help panel groups by category", {
  html <- as.character(outcome_help_ui())
  for (heading in c("Hits", "Reached base", "Outs", "Other"))
    expect_true(grepl(heading, html, fixed = TRUE), info = paste("missing heading:", heading))
})

test_that("an outcome button keeps its id and carries its description", {
  html <- as.character(outcome_button("track-o_1B", "1B"))
  expect_true(grepl('id="track-o_1B"', html, fixed = TRUE))
  expect_true(grepl("Batter reaches first base safely", html, fixed = TRUE))
})

test_that("outcome_button rejects an unknown code", {
  expect_error(outcome_button("track-o_ZZ", "ZZ"), "unknown outcome")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_outcome_help.R")'`
Expected: FAIL — cannot open file `R/outcome_help.R`.

- [ ] **Step 3: Create `R/outcome_help.R`**

```r
# Outcome-code help: the glossary panel and the per-button popovers.
# Both read APP_CONFIG$outcome_meta so the vocabulary has one source of truth.

.OUTCOME_CATEGORY_LABELS <- c(
  hit = "Hits", on_base = "Reached base", out = "Outs", other = "Other"
)

outcome_help_ui <- function() {
  sections <- lapply(names(.OUTCOME_CATEGORY_LABELS), function(cat) {
    codes <- names(Filter(function(m) identical(m$category, cat), APP_CONFIG$outcome_meta))
    if (!length(codes)) return(NULL)
    rows <- lapply(codes, function(code) {
      m <- APP_CONFIG$outcome_meta[[code]]
      tags$tr(
        tags$th(scope = "row", class = "bw-help-code", code),
        tags$td(class = "bw-help-label", m$label),
        tags$td(class = "bw-help-desc text-muted small", m$description))
    })
    tagList(
      tags$h5(class = "mt-3", .OUTCOME_CATEGORY_LABELS[[cat]]),
      tags$table(class = "table table-sm bw-help-table", tags$tbody(!!!rows)))
  })
  tagList(
    tags$div(class = "p-3",
      tags$p(class = "text-muted small",
        "Codes you can record for a plate appearance. Tap and hold an outcome button during a game for the same description."),
      !!!Filter(Negate(is.null), sections)))
}

# Wraps an outcome actionButton in a popover carrying its label and description.
# `ns_id` must already be namespaced; the button's id is unchanged so the existing
# observeEvent(input[[paste0("o_", code)]]) wiring keeps working.
outcome_button <- function(ns_id, code) {
  m <- APP_CONFIG$outcome_meta[[code]]
  if (is.null(m)) stop(sprintf("unknown outcome code: %s", code))
  bslib::popover(
    actionButton(ns_id, code, class = "btn-outline-primary bw-outcome-btn"),
    tags$strong(m$label), tags$br(), m$description,
    title = code, placement = "top")
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_outcome_help.R")'`
Expected: PASS, 4 tests.

- [ ] **Step 5: Wire the Help tab and swap in `outcome_button`**

In `R/tracking_module.R`, change the `navset_tab` inside `tracking_ui()` to add a Help panel:

```r
    navset_tab(
      nav_panel("Scorebook", uiOutput(ns("scorebook"))),
      nav_panel("Box score", tableOutput(ns("box_away")), tableOutput(ns("box_home"))),
      nav_panel("Help", outcome_help_ui()))
```

(Task 5 replaces the two `tableOutput`s. Leave them for now.)

In `output$action_panel`, replace the `lapply` that builds plain `actionButton`s:

```r
        btns <- lapply(outcomes, function(o)
          outcome_button(session$ns(paste0("o_", o)), o))
```

Do **not** change the `outcomes` vector or the `observeEvent` registrations — the button
ids are identical, so the existing handlers still fire.

- [ ] **Step 6: Run the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: exit 0. `tests/test_tracking_module.R` exercises `record_outcome_event`, not the
UI, so it is unaffected.

- [ ] **Step 7: Commit**

```bash
git add R/outcome_help.R R/tracking_module.R tests/test_outcome_help.R
git commit -m "feat: outcome-code help panel and per-button popovers"
```

---

### Task 4: Box score without player IDs, sortable

**Files:**
- Modify: `R/boxscore.R:4-27` (`batting_lines`)
- Modify: `R/tracking_module.R` (`tracking_ui` box-score panel, `output$box_away`/`box_home`)
- Modify: `global.R` (add `library(DT)`)
- Modify: `manifest.json` (regenerate)
- Test: `tests/test_boxscore.R` (extend)

**Interfaces:**
- Consumes: `state` from `fold_events()`.
- Produces: `batting_lines(state, team)` → a data frame with columns
  `Order, Player, AB, R, H, RBI, BB, K` **and no `player_id`**. Slice 2.3's summary card
  filters this by player name to build the batter's in-game line, so the `Player` column
  name is load-bearing.

- [ ] **Step 1: Write the failing test**

Append to `tests/test_boxscore.R`:

```r
test_that("batting_lines exposes no player_id and is ordered by batting slot", {
  lu <- list(make_player("a1", "Ann", "F", 7L, 2L, "SS"),
             make_player("a2", "Bo",  "M", 3L, 1L, "P"))
  st <- initial_game_state()
  st$lineups$away <- lu
  df <- batting_lines(st, "away")
  expect_false("player_id" %in% names(df))
  expect_identical(names(df), c("Order", "Player", "AB", "R", "H", "RBI", "BB", "K"))
  expect_equal(df$Order, c(1L, 2L))
  expect_equal(df$Player, c("Bo", "Ann"))
})

test_that("a player with no order_slot still appears, after the batters", {
  lu <- list(make_player("a1", "Ann", "F", 7L, 1L, "SS"),
             make_player("a2", "Sub", "M", 3L, NA_integer_, NA_character_))
  st <- initial_game_state()
  st$lineups$away <- lu
  df <- batting_lines(st, "away")
  expect_equal(nrow(df), 2L)
  expect_equal(df$Player[2], "Sub")
  expect_true(is.na(df$Order[2]))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_boxscore.R")'`
Expected: FAIL — `player_id` is present and the column names are lowercase.

- [ ] **Step 3: Rewrite the tail of `batting_lines()`**

Keep the accumulation loop exactly as it is. Replace only the `data.frame(...)` return:

```r
  ord <- vapply(lineup, function(p) p$order_slot %||% NA_integer_, integer(1))
  df <- data.frame(
    Order = ord, Player = names_,
    AB = unlist(AB[ids]), R = unlist(R[ids]), H = unlist(H[ids]),
    RBI = unlist(RBI[ids]), BB = unlist(BB[ids]), K = unlist(K[ids]),
    row.names = NULL, stringsAsFactors = FALSE)
  # Batters in order, then anyone without a slot (defensive subs, unentered players).
  df[order(is.na(df$Order), df$Order), , drop = FALSE]
```

`row.names = NULL` plus the reorder leaves stale row names; add `rownames(df) <- NULL`
before returning if `DT` shows them.

Note: `make_player()` coerces a missing `order_slot` to `NA_integer_`, so `%||%` alone is
not enough — `%||%` only catches NULL. The `vapply` above is correct because
`p$order_slot` is always a length-1 integer after `make_player()`.

- [ ] **Step 4: Run the box-score test**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_boxscore.R")'`
Expected: PASS.

- [ ] **Step 5: Render with DT and label with team names**

Add `library(DT)` to `global.R`, in the library block after `library(bslib)`.

In `R/tracking_module.R`, replace the box-score nav panel:

```r
      nav_panel("Box score",
        div(class = "p-2",
          uiOutput(ns("box_away_hdr")), DT::DTOutput(ns("box_away")),
          uiOutput(ns("box_home_hdr")), DT::DTOutput(ns("box_home")))),
```

and replace the two `renderTable` calls in `tracking_server`:

```r
    .box_dt <- function(team) DT::renderDT({
      DT::datatable(batting_lines(state(), team),
        rownames = FALSE, class = "compact stripe",
        options = list(dom = "t", paging = FALSE, ordering = TRUE,
                       order = list(list(0, "asc"))))
    })
    output$box_away <- .box_dt("away")
    output$box_home <- .box_dt("home")

    .team_name <- function(team) state()$teams[[team]]$name %||% team
    output$box_away_hdr <- renderUI(tags$h5(class = "mt-2", .team_name("away")))
    output$box_home_hdr <- renderUI(tags$h5(class = "mt-3", .team_name("home")))
```

`dom = "t"` renders the table only — no search box, no paging controls, no info line.
`order = list(list(0, "asc"))` sorts by the `Order` column, which is column index 0 once
`rownames = FALSE` is set.

- [ ] **Step 6: Regenerate the deployment manifest**

DT is installed but absent from `manifest.json`; without this the Posit Connect Cloud
deploy fails at package-restore time.

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'rsconnect::writeManifest()'`

If `rsconnect` is not installed, install it first:
`"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'install.packages("rsconnect", repos="https://cloud.r-project.org")'`

Verify DT made it in:
`"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'cat("DT" %in% names(jsonlite::fromJSON("manifest.json")$packages), "\n")'`
Expected: `TRUE`

- [ ] **Step 7: Run the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: exit 0.

- [ ] **Step 8: Commit**

```bash
git add R/boxscore.R R/tracking_module.R global.R manifest.json tests/test_boxscore.R
git commit -m "feat: sortable box score without player IDs; add DT to manifest"
```

---

### Task 5: Authentication failures do not crash the session

Fixes the two throw paths in `R/supabase_client.R` and the unhelpful error text in
`R/auth_module.R`. Task 6 handles the third path (`storage_for_identity`).

**Files:**
- Modify: `R/supabase_client.R:23-40` (`.gotrue_request`)
- Modify: `R/auth_module.R:1-35`
- Test: `tests/test_supabase_client.R` (extend), `tests/test_auth_module.R` (extend)

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `.gotrue_request(path, email, password)` → **always** returns
    `list(ok = <lgl>, user_id = <chr>, access_token = <chr>, error = <chr>)`. It never
    throws, for any input or network condition.
  - `friendly_auth_error(msg)` → `<chr>`, exported from `R/supabase_client.R`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/test_supabase_client.R`:

```r
test_that("a connectivity failure returns an error result instead of throwing", {
  # SUPABASE_URL unset => the request targets "/auth/v1/token?..." with no host.
  withr_vars <- c(SUPABASE_URL = "", SUPABASE_ANON_KEY = "")
  old <- Sys.getenv(names(withr_vars), unset = NA)
  do.call(Sys.setenv, as.list(withr_vars))
  on.exit({
    for (n in names(old)) if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, setNames(list(old[[n]]), n))
  }, add = TRUE)

  res <- gotrue_sign_in("nobody@example.com", "hunter2")
  expect_false(res$ok)
  expect_true(nzchar(res$error))
  expect_true(is.na(res$user_id))
})

test_that("friendly_auth_error rewrites known GoTrue messages", {
  expect_match(friendly_auth_error("Invalid login credentials"), "email or password")
  expect_match(friendly_auth_error("User already registered"), "already")
  # Unknown messages pass through unchanged.
  expect_equal(friendly_auth_error("teapot"), "teapot")
  # Empty or missing collapses to a generic message.
  expect_true(nzchar(friendly_auth_error("")))
  expect_true(nzchar(friendly_auth_error(NA_character_)))
})
```

Append to `tests/test_auth_module.R`:

```r
test_that("a sign-in function that throws surfaces a message instead of crashing", {
  boom <- function(email, password) stop("connection refused")
  testServer(auth_server, args = list(sign_in = boom, sign_up = boom), {
    session$setInputs(email = "a@b.c", password = "x", do_sign_in = 1)
    expect_true(nzchar(output$err))
    expect_true(is.na(identity()$mode))   # still unauthenticated
  })
})

test_that("guest mode is unaffected by a broken sign-in backend", {
  boom <- function(email, password) stop("connection refused")
  testServer(auth_server, args = list(sign_in = boom, sign_up = boom), {
    session$setInputs(do_guest = 1)
    expect_equal(identity()$mode, "guest")
  })
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_supabase_client.R")'`
Expected: FAIL — `could not resolve host` propagates out of `req_perform()`.

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_auth_module.R")'`
Expected: FAIL — `handle()` calls `fn()` unguarded, so `stop("connection refused")` escapes.

- [ ] **Step 3: Make `.gotrue_request` total**

In `R/supabase_client.R`, replace `.gotrue_request` and add `friendly_auth_error`:

```r
.auth_error <- function(msg)
  list(ok = FALSE, user_id = NA_character_, access_token = NA_character_,
       error = friendly_auth_error(msg))

# GoTrue's raw messages are terse and sometimes cryptic. Map the ones we know;
# pass anything else through so a real backend message is never swallowed.
friendly_auth_error <- function(msg) {
  if (is.null(msg) || length(msg) != 1 || is.na(msg) || !nzchar(msg))
    return("Sign-in failed. Please try again.")
  known <- c(
    "Invalid login credentials" = "That email or password is not correct.",
    "Email not confirmed"       = "Check your inbox and confirm your email address first.",
    "User already registered"   = "An account with that email already exists — try signing in.",
    "Password should be at least 6 characters" =
      "Passwords must be at least 6 characters long.")
  if (msg %in% names(known)) return(unname(known[[msg]]))
  msg
}

.gotrue_request <- function(path, email, password) {
  base <- Sys.getenv("SUPABASE_URL")
  if (!nzchar(base))
    return(.auth_error("Saving is not configured on this deployment."))
  # req_error(is_error = FALSE) suppresses HTTP *status* errors but not transport
  # errors (DNS, refused connection, TLS), which is why the whole call is wrapped.
  tryCatch({
    resp <- httr2::request(paste0(base, "/auth/v1/", path)) |>
      httr2::req_headers(apikey = Sys.getenv("SUPABASE_ANON_KEY"),
                         "Content-Type" = "application/json") |>
      httr2::req_body_json(list(email = email, password = password)) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    .gotrue_parse(httr2::resp_body_json(resp))
  }, error = function(e) .auth_error("Could not reach the sign-in service."))
}
```

Also route `.gotrue_parse`'s failure branch through the new helper: change its final line
from the inline `list(...)` to `.auth_error(msg)`.

- [ ] **Step 4: Guard `handle()` and gate the form when unconfigured**

In `R/auth_module.R`, replace `handle()`:

```r
    handle <- function(fn) {
      res <- tryCatch(fn(input$email, input$password),
                      error = function(e) list(ok = FALSE,
                        error = "Could not reach the sign-in service."))
      if (isTRUE(res$ok)) {
        identity(list(mode = "user", user_id = res$user_id, access_token = res$access_token))
        err("")
      } else err(res$error %||% "Authentication failed")
    }
```

and replace `auth_ui()` so an unconfigured deployment says so instead of offering a form
that cannot succeed:

```r
auth_ui <- function(id) {
  ns <- NS(id)
  configured <- supabase_configured()
  tagList(
    tags$h3(if (configured) "Sign in to save your games" else "Bookworm"),
    if (!configured)
      div(class = "alert alert-warning py-2 small",
        "Saving is not configured on this deployment. You can score a game as a guest, ",
        "but it will be lost when you refresh."),
    textInput(ns("email"), "Email"),
    passwordInput(ns("password"), "Password"),
    div(class = "d-flex gap-2 flex-wrap",
      actionButton(ns("do_sign_in"), "Sign in",
        class = if (configured) "btn-primary" else "btn-outline-secondary disabled"),
      actionButton(ns("do_sign_up"), "Create account",
        class = if (configured) "btn-outline-secondary" else "btn-outline-secondary disabled"),
      actionButton(ns("do_guest"), "Continue as guest",
        class = if (configured) "btn-link" else "btn-primary")),
    div(class = "text-danger small mt-2", textOutput(ns("err"))))
}
```

`supabase_configured()` lives in `R/session_flow.R`, which `global.R` sources; alphabetical
load order puts `auth_module.R` before it, but that is fine because the call happens at UI
*render* time, not at source time.

- [ ] **Step 5: Run both tests, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_supabase_client.R")'`
Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_auth_module.R")'`
Expected: PASS.

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: exit 0.

- [ ] **Step 6: Commit**

```bash
git add R/supabase_client.R R/auth_module.R tests/test_supabase_client.R tests/test_auth_module.R
git commit -m "fix: authentication failures return messages instead of crashing"
```

---

### Task 6: Storage falls back to guest when the database is unreachable

The third crash path: a successful sign-in against an unreachable Postgres kills the
session, because `bookworm_server`'s `observeEvent(identity(), ...)` calls
`storage_for_identity()` → `supabase_connect()` → `DBI::dbConnect()` with no handler.

**Files:**
- Modify: `R/session_flow.R:1-8` (`storage_for_identity`)
- Modify: `R/app_main.R:18-46` (banner)
- Test: `tests/test_session_flow.R` (extend)

**Interfaces:**
- Consumes: `make_storage()` from `R/storage.R`.
- Produces: `storage_for_identity(identity)` → `list(storage =, con =, degraded = <lgl>, reason = <chr>)`.
  `degraded` is `TRUE` when a signed-in user was silently downgraded to guest storage.
  `R/app_main.R` reads `degraded` and `reason` to render the banner.

- [ ] **Step 1: Write the failing test**

Append to `tests/test_session_flow.R`:

```r
test_that("guest identity yields guest storage and is not degraded", {
  sf <- storage_for_identity(list(mode = "guest", user_id = NA_character_))
  expect_true(is.function(sf$storage$create_game))
  expect_null(sf$con)
  expect_false(sf$degraded)
})

test_that("a failing database connection falls back to guest storage", {
  # supabase_connect is looked up in the calling environment, so a local shadow works.
  sf <- storage_for_identity(
    list(mode = "user", user_id = "u1"),
    configured = function() TRUE,
    connect = function() stop("could not connect to server"))
  expect_true(is.function(sf$storage$append_event))
  expect_null(sf$con)
  expect_true(sf$degraded)
  expect_true(nzchar(sf$reason))
})

test_that("a working database connection is used and is not degraded", {
  fake_con <- structure(list(), class = "FakeConn")
  sf <- storage_for_identity(
    list(mode = "user", user_id = "u1"),
    configured = function() TRUE,
    connect = function() fake_con)
  expect_identical(sf$con, fake_con)
  expect_false(sf$degraded)
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_session_flow.R")'`
Expected: FAIL — `unused arguments (configured = ..., connect = ...)`.

- [ ] **Step 3: Rewrite `storage_for_identity`**

Replace the function in `R/session_flow.R`. The injected `configured`/`connect` arguments
follow the pattern `auth_server()` already uses for `sign_in`/`sign_up`, so the failure path
is testable without a database.

```r
storage_for_identity <- function(identity,
                                 configured = supabase_configured,
                                 connect = supabase_connect) {
  guest <- function(degraded = FALSE, reason = "")
    list(storage = make_storage("guest"), con = NULL,
         degraded = degraded, reason = reason)

  if (!identical(identity$mode, "user")) return(guest())
  if (!configured())
    return(guest(TRUE, "Saving is not configured on this deployment."))

  con <- tryCatch(connect(), error = function(e) e)
  if (inherits(con, "error"))
    return(guest(TRUE, "Could not reach the database. This game will not be saved."))

  list(storage = make_storage("supabase", con = con, user_id = identity$user_id),
       con = con, degraded = FALSE, reason = "")
}
```

- [ ] **Step 4: Surface the degraded state in the banner**

In `R/app_main.R`, capture the flag in the identity observer and widen the banner. Replace
the observer and `output$guest_banner`:

```r
  degraded <- reactiveVal(NULL)

  observeEvent(identity(), {
    req(!is.na(identity()$mode))
    sf <- storage_for_identity(identity())
    store(sf$storage)
    degraded(if (isTRUE(sf$degraded)) sf$reason else NULL)
    if (!is.null(sf$con)) onStop(function() DBI::dbDisconnect(sf$con))
    nav_select("screen", "setup")
  }, ignoreInit = TRUE)

  output$guest_banner <- renderUI({
    req(!is.null(identity()))
    if (!is.null(degraded()))
      div(class = "alert alert-danger m-2 py-2 small", degraded())
    else if (identical(identity()$mode, "guest"))
      div(class = "alert alert-warning m-2 py-2 small",
        "Guest mode: sign in to save. Refreshing will lose this game.")
  })
```

- [ ] **Step 5: Run the test, then the full suite**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" -e 'library(testthat); source("tests/test_session_flow.R")'`
Expected: PASS.

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: exit 0. `tests/test_app_flow.R` exercises the guest path through
`bookworm_server`; `sf$degraded` is `FALSE` there, so the banner logic is unchanged for it.

- [ ] **Step 6: Commit**

```bash
git add R/session_flow.R R/app_main.R tests/test_session_flow.R
git commit -m "fix: fall back to guest storage and warn when the database is unreachable"
```

---

### Task 7: README

**Files:**
- Modify: `README.md`

**Interfaces:** none.

- [ ] **Step 1: Add the AI roadmap and refresh the surrounding sections**

Replace the `## Roadmap` section and update `## Known limitations` and `## Rules supported`
to match what slice 2 ships. The AI section is the point of this task — it is the record of
the two planned capabilities:

```markdown
## AI roadmap

Two planned capabilities, neither implemented yet:

- **Scorebook photo import.** Upload photos of a paper scorebook page. A vision model
  reads the grid — lineups, per-inning cells, diamond fills, out numbers — and emits a
  populated Bookworm game. The result is presented for review and correction before any
  events are committed, because a misread cell is worse than no cell.
- **Ruleset from a rules document.** Upload a league's rules PDF or text. A model extracts
  the parameters into a Bookworm ruleset and presents it as a diff against the closest
  built-in preset, so the user confirms only what actually differs.

## Roadmap

- Phase 3: team management, sharing, standings, RLS.
```

Also add to `## Known limitations`:

```markdown
- Sign-in requires Supabase configuration; without it the app runs guest-only and says so.
- A run cap now ends the half-inning when reached (configurable via `run_cap$cap_ends_half`).
```

Leave the second bullet out if slice 2.1 has not merged yet — it describes behaviour this
slice does not ship.

- [ ] **Step 2: Verify the file renders**

Run: `"/c/Program Files/R/R-4.5.3/bin/Rscript.exe" run_tests.R`
Expected: exit 0 (no tests read the README; this is the regression gate).

- [ ] **Step 3: Commit**

```bash
git add README.md
git commit -m "docs: record the AI roadmap and refresh limitations"
```

---

## Definition of done for slice 2.0

- `Rscript run_tests.R` exits 0.
- `BRAND_COLORS` has no agriculture-app keys; `grep -rn "nass\|weatherlink\|map_helpers" R/ _brand.yml` returns nothing.
- Every outcome code has a description reachable from the Help tab and from its button.
- The box score shows no `player_id` and sorts on every column.
- Signing in with bad credentials, with no `SUPABASE_URL`, and with an unreachable database
  each produce a message and leave the app usable.
- `manifest.json` lists DT.
