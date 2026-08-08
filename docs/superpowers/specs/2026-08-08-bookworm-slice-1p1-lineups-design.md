# Bookworm — Slice 1.1 Design: Lineups, Run-Only Teams & Coed Fielding Rules

**Date:** 2026-08-08
**Status:** Approved (design); ready for implementation planning
**Author:** Nash Delcamp (with Claude)
**Builds on:** `2026-07-03-bookworm-slice-one-design.md` (slice one, merged to main)

## Overview

Slice one shipped a 4-player stub lineup with no editor, making real tracking
impractical, and it computed rule warnings but never surfaced them. Slice 1.1
delivers four connected things:

1. A real **lineup editor** per team (name, gender, optional jersey, optional
   field position).
2. **Run-only teams:** there is no "opponent" concept — two symmetric teams
   (away, home); **any team with an empty lineup is tracked by runs per
   half-inning** instead of plate appearances. Either or both may be run-only.
3. A configurable **coed fielding gender-balance engine** (totals, per-category
   minimums, battery opposite-gender, and count-specific tiers).
4. **Warning surfacing:** rule violations are shown to the scorer — as
   acknowledgement **modals** for illegal-lineup/defense violations and **toast
   notifications** for informational notices.

Plus a `batting_size` ruleset field and two small setup-form fixes.

## Goals

- Mobile-first **lineup editor** for each team, guest-mode usable. Per player:
  name, gender (M/F), optional jersey (default `00`), optional position.
- **Empty lineup ⇒ run-only** for that team (symmetric); discoverable via help text.
- `batting_size`: `unlimited` (default) | `9` | `10`, all first-class.
- Config fixes: `foul_out_rule = "unlimited"`; hide "every N" unless needed.
- **Configurable coed fielding rules** with per-category minimums and
  count-specific tiers, validated against the defense.
- **Surface warnings**: modals (require acknowledgement) for violations; toasts
  for notices. Applies to all rules, including slice one's batting-order and
  run-cap warnings that were never shown.

## Non-goals (slice 1.1)

- Roster persistence / reuse across games (Phase 3).
- Mid-game lineup editing beyond existing substitution events.
- Hand-editing every fielding tier in the mobile UI (the engine is fully
  configurable; the UI ships presets + base knobs — see §4.8).
- Changes to auth, Supabase persistence, or the scorebook SVG.

## Design

### 4.1 Two small config fixes (`R/rules_engine.R`, `R/setup_module.R`)

- `foul_out_rule` domain becomes `out | one_courtesy_foul | unlimited`;
  `validate_ruleset_config` accepts it. (`unlimited` = a two-strike foul is never
  a strikeout; the reducer doesn't auto-apply foul-outs, so this is an enum the
  count controls consult.)
- Setup form shows the `gender_n` input only when
  `batting_gender_rule$type == "every_n"` (`conditionalPanel` on the namespaced
  `gender_rule` input).

### 4.2 Ruleset field `batting_size` (`R/rules_engine.R`)

- `default_ruleset_config()`: `batting_size = NA_integer_` (unlimited).
- Coerced via `.as_int_or_na`; valid when `NA` or a positive integer.
- **Guidance only:** the entered lineup *is* the batting order (all entered
  players bat, in row order). Drives seeded rows and a non-blocking warning if a
  team's batter count differs. Reducer already wraps on actual lineup length.

### 4.3 Lineup editor (`R/setup_module.R`)

- Setup screen leads with two lineup sections (Away, Home); ruleset inputs move
  into a collapsible **"Rules"** accordion (`bslib::accordion`).
- Each section: dynamic **player rows** (Add / per-row remove); row order =
  `order_slot`. Inputs: `name` (text), `gender` (M/F segmented), `jersey`
  (numeric, optional; blank ⇒ `00`), `position` (select, optional; §4.5).
- **Help text:** *"Leave a team's lineup empty to just record that team's runs
  each inning."*
- Seed `batting_size` rows when 9/10; a small starter set when unlimited.
- `build_game_start_event` accepts variable-length **and empty** lineups on
  either/both teams (empty ⇒ run-only; never a validation error).
- Helper `collect_lineup(input, prefix, row_ids)` → list of `make_player(...)`;
  unit-testable against a fake input map.

### 4.4 Run-only half tracking (`R/game_events.R`, `R/game_reducer.R`, `R/tracking_module.R`)

- **New event `half_runs`**: `{ team, runs }`. In `EVENT_TYPES`; `validate_event`
  checks `team ∈ {home,away}`, `runs` a non-negative integer.
- **Reducer:** `apply_event` handles `half_runs` — apply run cap
  (`apply_run_cap`), add capped runs to the batting team's `score` +
  `runs_this_half`, then `advance_half()`; routed through `.refresh_flags`.
- **Tracking UI branches on the batting team:** has a lineup → normal
  count/outs/outcome flow; empty → a "Runs this inning" numeric + "End
  half-inning" button appending a `half_runs` event, with a run-only note.
  Helper `record_half_runs_event(state, runs)` (uses `state$batting_team`);
  unit-tested. Works identically for away or home.

### 4.5 Positions & categories (`R/app_config.R`, `R/game_events.R`, `R/game_reducer.R`)

- **Position labels**, grouped into three fielding categories (constants in
  `APP_CONFIG$POSITION_CATEGORY`):
  - **battery:** `P`, `C`
  - **infield:** `1B`, `2B`, `SS`, `3B`
  - **outfield:** `LF`, `LCF`, `CF`, `RCF`, `RF`, `OF`, `ROVER`
    (`OF` = generic outfield; `LCF`/`RCF` for four-OF alignments; **`ROVER` is
    an outfielder** — the short/shallow fielder).
- **`DH`** and **blank** are **non-fielders** ("bats, doesn't field" — blank is
  effectively an undesignated DH). Neither is in any fielding category.
- `make_player`'s `position` becomes an **optional label string** (or
  `NA_character_`), loosened from the slice-one integer. Slice-one integer
  callers still work (nothing compares positions numerically); the
  defensive-substitution branch stops coercing to integer.

### 4.6 Coed fielding gender-balance engine (`R/rules_engine.R`)

The ruleset's `fielding` block gains a configurable, general model (not hardcoded
to any league):

```r
fielding = list(
  min_females = 0L,        # min females among fielders (0 = off)
  max_males   = NA_integer_, # max males among fielders (NA = no cap)
  tiers = list(),          # count-specific distribution tiers (see below)
  position_requirements = list()  # reserved, still unenforced
)
```

Each **tier** is keyed by the number of females on the field and specifies the
required distribution at that count:

```r
list(females = 5L, outfield = 2L, infield = 2L, battery = "one")
# battery mode: "one" = P and C must be opposite genders (exactly one female in
#               the battery); "any" = no battery gender constraint.
```

**Evaluation** (`evaluate_fielding(cfg, defense_lineup)` → list of violations):
1. Consider only players in a **field position** (battery/infield/outfield);
   DH/blank excluded. If none have a field position, return no violations
   (can't evaluate) — no false alarms.
2. `F` = females among fielders, `M` = males among fielders.
3. Totals: violation if `F < min_females`; if `max_males` set and `M > max_males`.
4. Applicable tier = the tier with the largest `females` threshold `≤ F`
   (clamp; `F` above the top tier uses the top tier). If `tiers` is empty, skip
   tier checks.
5. Tier checks: females in outfield `≥ tier$outfield`; females in infield
   `≥ tier$infield`; battery: if `"one"`, P and C (when both are filled) must be
   opposite genders.
6. Return a structured violation per failed check (code + human message).

The example coed ruleset the user provided ships as a **"Standard coed
(10-player)" preset**: `min_females=4, max_males=6`, tiers
`{3:1/1/one, 4:1/1/one, 5:2/2/one, 6:1/1/any}` (6+ relaxes the battery to
allow females at both P and C while keeping ≥1 OF and ≥1 IF).

`fielding_warnings` is replaced/backed by `evaluate_fielding`; the default
config (`min_females=0`, no `max_males`, empty tiers) yields no warnings.

### 4.7 Warning surfacing (`R/game_reducer.R`, `R/tracking_module.R`)

- `state$warnings` becomes a **list of structured items**:
  `list(severity = "violation" | "notice", code, message)`.
  - **violation** (illegal lineup/defense): batting-order gender-rule breach
    (e.g. two males in a row when disallowed), any fielding-balance violation.
  - **notice** (informational): run cap reached, mercy threshold reached, game
    final, batting-size mismatch.
- `.refresh_flags` assembles these from the batting-order check and
  `evaluate_fielding` (on the fielding team) plus the run-cap/mercy notices.
- **Tracking module surfacing:** an observer watches `state$warnings` after each
  action and shows **new** items — **violations via `modalDialog`** (a clear
  title, the list of violations, a single "Got it" dismiss button that requires
  acknowledgement) and **notices via `showNotification`** (toast). De-dup so the
  same standing violation isn't re-modaled every pitch (track the set of
  acknowledged/among-current violation codes; only modal when the violation set
  changes).

### 4.8 Setup UI for fielding rules (`R/setup_module.R`)

Inside the "Rules" accordion, a **Fielding** subsection:
- `min_females` and `max_males` numeric inputs.
- A **preset** selector: `None` (default — no gender rules) / `Standard coed
  (10-player)` (loads the §4.6 preset tiers) / `Custom`.
- `Custom` reveals base per-category minimum inputs (outfield / infield female
  minimums) and a battery-mode select (`one` / `any`), which build a **single
  tier** applied at all counts. **Full per-count tier hand-editing is deferred**
  (the engine supports it; presets/JSON can define arbitrary tiers). This keeps
  the mobile form tractable while the validation engine stays fully general.

## Files touched

- `R/app_config.R` — position labels + `POSITION_CATEGORY` map.
- `R/rules_engine.R` — `foul_out_rule` enum; `batting_size`; `fielding` schema
  (`min_females`, `max_males`, `tiers`); `evaluate_fielding`; the "Standard coed"
  preset constant.
- `R/game_events.R` — `half_runs` type + validation; `make_player` label position.
- `R/game_reducer.R` — `half_runs` apply; structured `state$warnings`
  (severity/code/message); `.refresh_flags` assembles batting-order + fielding +
  notices; defensive-sub position label.
- `R/setup_module.R` — Rules accordion; two dynamic lineup editors; `batting_size`;
  conditional `gender_n`; `foul_out` unlimited; fielding rules subsection +
  presets; `collect_lineup`; drop the stub; help text.
- `R/tracking_module.R` — run-only branch + `record_half_runs_event`; warning
  observer (modals for violations, toasts for notices, with de-dup).
- Tests: rules-engine (`evaluate_fielding` across tiers/battery/totals; positions;
  batting_size; foul enum), events (`half_runs`, label position), reducer
  (`half_runs`, empty-lineup fold, structured warnings), setup (`collect_lineup`,
  empty/variable lineups either team), tracking (`record_half_runs_event`), and a
  `testServer` flow test for run-only + a violation surfacing.

## Testing strategy

`evaluate_fielding` is the heaviest new logic and gets thorough unit tests:
per-category female counting, each tier boundary (3/4/5/6 females), the `"one"`
vs `"any"` battery modes, `max_males`, the empty-positions no-false-alarm guard,
and the Standard-coed preset against legal and illegal defenses. Other pure
helpers unit-tested as above. A `testServer` test drives a run-only half and
asserts a fielding violation produces a modal-worthy warning item in
`state$warnings`. Full suite stays green (`Rscript run_tests.R`).

## Open items deferred to planning

- Exact `bslib` widgets for the mobile lineup editor and fielding subsection.
- Modal de-dup key: violation-set signature vs per-code acknowledgement (lean:
  re-modal only when the set of active violation codes changes).
- Confirmed: "Start game" requires no non-empty lineup (a fully run-only game is
  valid).
