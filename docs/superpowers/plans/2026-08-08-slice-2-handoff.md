# Slice 2 handoff — state after 2.0 and 2.1, notes for 2.2 and 2.3

Written 2026-08-08, at the point where slices 2.0 and 2.1 are merged to `main` and
slices 2.2 and 2.3 are planned but not started. Its purpose is to carry forward what
lived only in the per-slice SDD ledgers, which were deleted at merge time.

## Where everything is

| Document | Path |
|---|---|
| Spec (all four slices) | `docs/superpowers/specs/2026-08-08-bookworm-slice-two-design.md` |
| Plan — 2.0 Cleanup (**done**) | `docs/superpowers/plans/2026-08-08-bookworm-slice-2p0-cleanup.md` |
| Plan — 2.1 Rules & setup (**done**) | `docs/superpowers/plans/2026-08-08-bookworm-slice-2p1-rules-setup.md` |
| Plan — 2.2 Play entry & subs (**next**) | `docs/superpowers/plans/2026-08-08-bookworm-slice-2p2-play-entry.md` |
| Plan — 2.3 Scorebook & presentation | `docs/superpowers/plans/2026-08-08-bookworm-slice-2p3-scorebook.md` |

Merge commits: slice 2.0 = `57798e7`, slice 2.1 = `35d0025`.

Baseline on `main`: 20 files in `R/`, 29 test files, 226 `test_that` blocks.
`Rscript run_tests.R` exits 0. `source("global.R")` loads and `bookworm_ui()` builds.

## What 2.2 and 2.3 are

**2.2 — Play entry and substitutions** (5 tasks). The app currently *infers* where runners
go after each play, and the inference is often wrong with no way to correct it. 2.2 replaces
that with an explicit disposition modal: tap an outcome with runners on base and you get a
row per runner plus the batter, pre-filled from the old inference, each offering `1 / 2 / 3 /
H / OUT`. Bases empty commits in one tap. It also makes the Substitution button work (it is
currently inert), and adds the runner-origin bookkeeping 2.3 needs. Task 5 adds the mid-game
lineup editor that emits the `lineup_set` event 2.1 built.

**2.3 — Scorebook and presentation** (5 tasks). The scorebook is unreadable today: runner
position is invisible, non-home-run runs never appear, and batting around draws cells on top
of each other. 2.3 adds `R/runner_paths.R`, which reconstructs each batter's full trip from
the `origin_index` bookkeeping 2.2 lays down, then rewrites the cell and grid renderers
around it. Plus the situation summary card and the tracking-view information architecture.

2.2 must land before 2.3 — 2.3's whole premise is the data 2.2 records.

## Corrections already applied to the 2.2 and 2.3 plans

Both plans were written before 2.0 and 2.1 ran, and both were patched afterwards. The
patches are in the files; this is the index so you know they exist.

**2.2's plan:**
- `outcome_button()` **does not exist**. An early draft depended on it. 2.0's final review
  found `bslib::popover()` resolves its trigger to `"click"` on a `<button>`, so the popover
  fired on the same tap that recorded the play; the owner ruled per-button popovers out and
  the function was deleted. Outcome buttons are plain `actionButton`s.
- **All five rule evaluators now share one item shape**: `list(severity, code, message)`.
  `evaluate_pinch_runner()` returns `list(ok =, items =)` — **not** `errors`, which the plan
  originally consumed by name at its `build_substitution_event()` step. The `code` field is
  what lets these messages use the existing toast dedupe from commit `7efcf48`.
- **Mercy evaluates only at a half-inning boundary** (`after_inning` means *completed*
  innings), and `state$status` is **derived, not latched**. 2.2 wires Undo; do not
  reintroduce a latch, or a mercy-finaled game becomes unreopenable again.

**2.3's plan:**
- The step adding `"ITPHR"` to `.HIT` is **already done** — 2.0's final fix wave did it, along
  with an invariant test in `tests/test_app_config.R` binding the glossary's `hit` category to
  `.HIT`. Verify it is still there; do not re-apply.

## Carry-forward items — these were only in the deleted ledgers

### Load-bearing for 2.2

**`order_slot = NA_integer_` does not survive a JSON round-trip.** `R/json_io.R` writes with
`na = "null"` and reads with `simplifyVector = FALSE`, so a persisted `NA_integer_` comes back
as `NULL`. Then `!is.na(p$order_slot)` returns `logical(0)`, which misaligns `Filter()` and
errors in `vapply` inside `R/game_reducer.R` (around the `.set_current_batter` batter filter).
This is pre-existing — it already affects `game_start` payloads — but **2.1's `lineup_set`
gave it a new entry point, and 2.2's lineup modal emits exactly those persisted events**.
`validate_event()` only checks `is.list(lineup)`. Address it in 2.2 rather than discovering it
when a saved game refuses to load.

**Regenerate `manifest.json` whenever `R/` gains a file.** `global.R` sources `R/` via
`list.files()` *from the deployed bundle*, so a file absent from the manifest is absent from
the deploy and every function it defines is undefined at startup. This has bitten twice
already: the pre-slice-2 manifest was missing `R/app_main.R`, and 2.1 initially shipped
without its four new rule files. An `.rscignore` now exists (`.superpowers`, `.claude`,
`.git`, `docs`, `tests`), so `rsconnect::writeManifest()` is safe to run from anywhere.
Verify after: `R/` entry count equals `length(list.files("R"))`.

### Parked — real, adjudicated as non-blocking

- `R/lineup_validation.R` wrap-around loop is bounded by `n`, correct for
  `max_consecutive_males` and `max_consecutive_same_gender` (lookback `n`) but one too large
  for `min_females_per_n` (lookback `n − 1`). A purely forward break at slot `n` is
  re-reported as a wrap break. One redundant advisory item in a surface that never blocks the
  scorer.
- Two lines above it, `!is.na(n) && n >= 1L` throws when `n` is `NULL` rather than `NA`.
  Unreachable from the app — every caller passes a `coerce_ruleset_config()`-ed ruleset, which
  always materialises `n` via `.as_int_or_na()`. `n <- ... %||% NA_integer_` closes it.

### Backlog — deferred minors worth a sweep sometime

- `tests/test_setup_module.R` shadows `showNotification` globally. `run_tests.R` sources all
  29 files into **one R process**, so the stub is live for the three files that run after it.
  Harmless today (they assert no notifications), but any future notification assertion in
  `tests/test_tracking_module.R` would silently see the stub.
- Test helpers (`codes`, `msgs`, `pl`, `p`) are defined at global scope across files sharing
  that one process — a standing collision hazard.
- `run_tests.R` is in the manifest but `tests/` is excluded, so the shipped copy is inert.
- Three copies of "look up a player's gender in a lineup" — twice in `R/game_reducer.R`, once
  in `R/rule_pinch_runner.R`.
- The retroactive batting-order warning is ephemeral: appended after `.refresh_flags()`, so
  the next event recomputes `warnings` and drops it. Deterministic under replay, but a scorer
  who reloads never sees it.
- Genderless rulesets store `gender = "M"` rather than `NA_character_` — "unknown" recorded as
  a fact, in persisted data.
- `max_per_inning = 0` and `max_per_player_per_game = 0` still read "Only 0 allowed; 0 already
  used"; only `max_per_game = 0` got friendlier wording.
- Under `same_gender` pinch-runner eligibility, `NA` gender silently "matches" because
  `identical(NA, NA)` is TRUE. Structurally unreachable — `ruleset_is_genderless()` returns
  FALSE for that eligibility, so the Gender column is always shown.
- 2.0 leftovers: the outcome-button vector is duplicated in `R/tracking_module.R`; the
  "not configured" string is literal in three files; two different notions of "configured"
  coexist (all seven env vars vs `SUPABASE_URL` alone).

### A product decision still open

The Help tab documents 18 outcome codes but the tracking screen has 10 buttons. `ITPHR`,
`IBB`, `HBP`, `KL`, `LO`, `PO`, `SF`, and `SAC` are documented and accepted by
`validate_event()` but unreachable from the UI. 2.0's brief deliberately froze the button
list, and the Help copy was reworded to be truthful about it. Someone has to decide whether
those codes get buttons, get a secondary picker, or get scoped out. 2.2 rewrites the action
panel, which is the natural moment.

## Process notes worth repeating

Five defects in slices 2.0 and 2.1 originated in the **plans**, not the implementations:

- Three test fixtures whose input shape contradicted what they asserted (a `name`/`player_id`
  differing by a space; a `numericInput`-era `NA` fed to what had become a `textInput`).
  Twice an implementer bent production code to satisfy the fixture — in one case printing raw
  UUIDs to a scorer.
- The mercy formula `inning >= after_inning`, off by one against every real "after N innings"
  rule, dormant until 2.1 shipped presets that used it.
- An unguarded `tiers[[i]] %||% NULL`, which throws before `%||%` can help and would have
  crashed the setup screen for four of six presets.

The common cause is stating a contract in prose rather than as a literal. Two practices that
paid for themselves and are worth carrying into 2.2 and 2.3:

1. **Tell implementers that a brief test is the more likely defect** when it disagrees with
   the specified behaviour, and to fix the test and report it.
2. **Require mutation testing in reviews** — inject the bug a test claims to catch and confirm
   it fails. Three separate times a test looked targeted, named the right behaviour, and did
   not fail when that behaviour was broken. Every one of those was invisible from reading and
   obvious from running.
