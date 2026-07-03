# Bookworm — Slice One Design

**Date:** 2026-07-03
**Status:** Approved (design); ready for implementation planning
**Author:** Nash Delcamp (with Claude)

## Overview

Bookworm is an R Shiny app for keeping and importing softball (and baseball)
scorebooks, deployed on Posit Connect Cloud. It targets social/coed leagues that
layer non-standard rules on top of the base game (arbitrary starting counts,
coed batting-order and fielding requirements, per-inning run caps, mercy rules,
etc.).

**Slice one** — the scope of this spec — delivers the core loop end to end:

> Track a game through innings/outs/events under a league's custom rules, render
> it as our own traditional scorebook, and persist it to your account.

Later phases (out of scope here, but designed around): **Phase 2** photo import
via a vision LLM; **Phase 3** full team/league management, sharing, standings,
and RLS hardening; **Phase 4+** baseball-specific depth and analytics.

## Goals

- Guide a scorer through a live game on a **phone at the field**: count, outs,
  bases, current batter, big outcome buttons, one-tap undo.
- Support **configurable league rules** entered **once per league** and reused.
- Record a **guided outcome + fielding** model rich enough to render a real
  scorebook and a full box score.
- Render games as a **traditional diamond-in-cell SVG scorebook**.
- Persist to **Supabase from day one**; allow a fully functional **guest mode**
  that is ephemeral (no account = no persistence, clearly warned).
- Support **substitutions**, a **box score / line score**, and **JSON
  export/import** in slice one.

## Non-goals (slice one)

- Vision-LLM photo import (Phase 2) — but the data model and a stubbed import
  interface are designed so it slots in without a rewrite.
- Full team-management UX, invitations, sharing links, season standings,
  cross-game aggregate stats (Phase 3).
- Postgres Row-Level Security enforcement (policies are defined but app-level
  tenant scoping is authoritative in slice one).
- Baseball-specific scoring depth beyond what softball shares (Phase 4+).

## Architecture

Mirrors the sister app `vasper`:

- `app.R` + `global.R` at the root; `R/` for Shiny modules and helpers; `www/`
  for CSS/JS/icons/SVG; `_brand.yml` for theming; `.Renviron(.template)` for
  secrets; `manifest.json` for Posit Connect Cloud deployment.
- `bslib` (Bootstrap 5), **mobile-first** layout. Shiny modules per screen.
  `httr2` for Supabase Auth (GoTrue) REST calls; `DBI` + `RPostgres` for the
  Postgres data connection. `ellmer` is reserved for the Phase-2 vision tool
  (same isolated tool-dispatch pattern `vasper` uses for LLM calls).

### Event-sourced core (the central architectural decision)

A game is stored as an **append-only list of events**, not as editable
"current state" rows. Current state — who is on base, the count, outs, score,
the scorebook, the box score — is **recomputed** by folding events through a
**pure reducer function**.

Event examples: `plate_appearance_result` (batter, outcome, fielding notation,
RBIs, outs), `runner_advance`, `substitution`, `inning_end`, `count_override`.

Rationale:

- **Undo** = drop the last event(s) and re-fold.
- **Autosave** = append one row.
- **Resume** = replay events (or load the latest snapshot, then replay the tail).
- Scorebook, box score, and stats are all **views of one stream**, so they can
  never disagree.
- The reducer is a **pure function**: unit-testable with no DB and no browser.

Accepted tradeoff: every state change must be expressed as an event, and
editing history means inserting/replacing an event and re-folding rather than
mutating a field in place.

Guest mode runs the identical reducer over an in-memory event list
(`reactiveVal`); nothing is written to Supabase.

## Data model (Supabase Postgres)

- `profiles` (id = Supabase Auth user id, display_name, created_at)
- `leagues` (id, owner_id, name, sport `softball|baseball`, created_at)
- `rulesets` (id, league_id, name, **config JSONB**, created_at) — entered once
  per league, reused across games
- `teams` (id, league_id, name, created_at)
- `players` (id, team_id, name, gender, jersey_number, default_position)
- `games` (id, owner_id, league_id, ruleset_id, home_team_id, away_team_id,
  played_on, location, status `in_progress|final`, **state_snapshot JSONB**,
  created_at, updated_at)
- `game_events` (id, game_id, seq, type, **payload JSONB**, created_at) — the
  append-only source of truth (unique on `(game_id, seq)`)
- `plate_appearances` (id, game_id, inning, half `top|bottom`, team_id,
  batter_id, batting_order_slot, outcome, fielding_notation, rbi, outs_recorded,
  errors JSONB, base_advancement JSONB, count_at_end, seq) — **materialized**
  from events for fast box-score / scorebook queries

Tenancy: every owned row carries `owner_id`. App-level scoping (filter by the
signed-in user) is authoritative in slice one; **RLS policies are written into
`supabase_schema.sql` but not relied upon yet** (Phase 3 hardening).

The schema lives in `data-raw/supabase_schema.sql` and is applied to Supabase
manually/out-of-band; the app connects to the already-provisioned database.

## Rules engine

A single validated **config object** stored in `rulesets.config` (JSONB),
edited once per league:

- `starting_count`: `{ balls, strikes }` (e.g. 1-1)
- `foul_out_rule`: `out` | `one_courtesy_foul`
- `batting_gender_rule`: `none` | `no_two_males_consecutive` | `every_other` |
  `every_n` (with `n`)
- `male_walk_rule`: e.g. male BB → two bases, next female bats-or-walks
- `fielding`: `{ min_females, position_requirements }`
- `innings`, `run_cap_per_inning`, `open_last_inning` (bool)
- `mercy_rule`: `{ differential, after_inning }`
- `short_lineup_auto_out`: bool
- `courtesy_runner`: bool/policy

The reducer consults the active ruleset to: apply the starting count, drive
inning/out guidance, enforce per-inning run caps, evaluate mercy, and **flag
illegal batters/fielders** (gender-order and fielding-minimum warnings). All
guidance is **overridable** by the scorer.

## Live tracking UX (mobile-first, guided state machine)

Thumb-friendly, one primary action visible at a time.

- **Persistent header:** current batter, count, outs, and a tappable
  **mini-diamond** showing base runners.
- **Primary actions:** large outcome buttons — 1B, 2B, 3B, HR, BB, K, out, FC,
  error — plus access to fielding-position detail for notation (e.g. `6-3`,
  `F8`).
- **After an outcome:** a quick **runner-advancement confirm**, pre-filled by
  the reducer's suggested advancement; scorer adjusts and confirms.
- **Undo** is always one tap.
- **Substitutions** available mid-game (batting sub / defensive sub / courtesy
  runner), recorded as events.
- **Rule warnings** surface inline (illegal batter order, fielding minimum, run
  cap reached, mercy threshold), non-blocking.
- **Setup flow:** pick league → ruleset → home/away teams → lineups → play.

## Scorebook rendering

A **traditional diamond-in-cell SVG grid**: batters as rows, innings as
columns. Each cell contains a small diamond showing the base path taken, the
play notation, outs, and RBIs. Generated as **inline SVG in R** from the folded
state so it prints crisply and is the exact target format the Phase-2 photo
importer will map into. A responsive/scrollable variant serves phones.

## Box score & stats

Derived from the event stream via a second fold:

- **Line score:** runs per inning across the top with R / H / E totals.
- **Batting lines:** one row per player — AB, R, H, RBI, BB, K, etc.
- (Pitching lines minimal in slice one; expandable later.)

## Persistence, auth & guest mode

- **Auth (simple, in the database):** Supabase Auth (GoTrue) email + password,
  verified via `httr2` REST calls; the Shiny session holds the resulting user
  identity. Minimal UI: sign up / log in / log out.
- **Signed in:** events autosave to Supabase (`game_events` append +
  `games.state_snapshot` update); games resume from any device.
- **Guest:** the full app works in-memory only, with a persistent banner:
  "Sign in to save — refreshing will lose this game."

## JSON export/import

A game serializes to a single portable JSON file (its `game_events` + ruleset +
teams/lineups). Import reconstructs it by replaying events. Serves as: a backup,
a way for guest users to save a game, the fastest bug-repro channel (paste a
game's JSON), and the on-disk fixture format for reducer unit tests.

## Repo structure

```
bookworm/
  app.R  global.R  _brand.yml  manifest.json  .Renviron.template
  R/
    app_config.R        # constants, table names, config
    brand_colors.R      # brand palette (sourced first, vasper pattern)
    supabase_client.R   # DBI/RPostgres connection + GoTrue REST helpers
    auth_module.R       # sign up / log in / log out; session identity
    storage.R           # storage interface: supabase-backed vs in-memory guest
    rules_engine.R      # ruleset config schema + validation + rule evaluation
    game_reducer.R      # pure fold: events -> game state (the core)
    setup_module.R      # league/ruleset/teams/lineups setup flow
    tracking_module.R   # mobile-first live tracking UI + event dispatch
    scorebook_render.R  # SVG diamond-in-cell scorebook from folded state
    boxscore.R          # line score + batting lines from folded state
    json_io.R           # export/import a game as JSON
  www/  css/  js/  icons/
  data-raw/
    supabase_schema.sql # tables + (deferred) RLS policies + seed
  tests/
    testthat/           # reducer & rules-engine unit tests (pure functions)
  docs/superpowers/specs/2026-07-03-bookworm-slice-one-design.md
```

## Testing strategy

- **Reducer** (`game_reducer.R`) and **rules engine** (`rules_engine.R`) are
  pure functions unit-tested with `testthat`, driven by JSON game fixtures
  (same format as export/import). This covers the highest-risk logic without a
  DB or a browser.
- Storage and auth are behind interfaces so they can be faked in tests.
- Manual/E2E verification of the tracking UI and scorebook rendering on a phone
  viewport before considering slice one complete.

## Open items deferred to planning

- Exact `rulesets.config` JSON schema field names and validation rules.
- Exact event type catalog and payload shapes.
- Supabase project provisioning steps and secret names in `.Renviron.template`.
