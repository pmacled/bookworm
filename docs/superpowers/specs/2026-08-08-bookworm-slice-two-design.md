# Bookworm Slice 2 — Rules, Play Entry, Scorebook, and Presentation

Date: 2026-08-08
Status: approved design

## Problem

Slice 1.1 shipped a working event-sourced core, but using it surfaced twenty pieces of
feedback. They cluster into four themes:

1. The ruleset model cannot express real league rules (no home-run limit, no pinch-runner
   allowance, one-tier mercy only, "no two males" but not "no three males").
2. Play entry *infers* where runners went. The inference is often wrong and there is no way
   to correct it.
3. The scorebook is unreadable: runner position is invisible, non-home-run runs never
   appear, and batting around draws cells on top of each other.
4. The shell is rough: leftover branding from an unrelated agriculture app, a dead
   Substitution button, an ungraceful auth failure, and information scattered across the
   screen.

## Scope

Four slices. 2.0 and 2.1 are independent and run in parallel. 2.2 depends on 2.1. 2.3
depends on 2.2.

| Slice | Contents | Depends on |
|---|---|---|
| 2.0 Cleanup | theme, help/glossary, box score, auth errors, README | — |
| 2.1 Rules & setup | ruleset model, presets, setup flow, lineup table | — |
| 2.2 Play entry & subs | runner disposition modal, substitution modal | 2.1 |
| 2.3 Scorebook & presentation | runner paths, scorebook grid, summary card | 2.2 |

Out of scope: photo import, rules-document import, cross-game rosters, standings, RLS.
Those stay on the roadmap.

---

## Slice 2.0 — Cleanup

### 2.0.1 Theme: "Ink on Paper"

`_brand.yml` and `R/brand_colors.R` carry palette entries from an unrelated agriculture
app (`usda_nass_navy`, `weatherlink-navy`, `accent-teal`) and `brand_colors.R` has a stale
comment referencing a `map_helpers.R` that does not exist in this repo.

Replace the palette with a scorekeeper's palette:

| Role | Token | Value | Use |
|---|---|---|---|
| background | `paper` | `#FDFCF7` | page, cards |
| surface | `paper-shade` | `#F4F1E8` | table headers, wells |
| foreground / dark | `ink` | `#1A2233` | text, diamond strokes, grid |
| secondary | `ink-muted` | `#5B6472` | secondary text, unreached base legs |
| primary | `ink-blue` | `#28406B` | buttons, links, structure |
| primary-light | `ink-blue-light` | `#E4E9F2` | filled diamond interior |
| danger / accent | `pencil-red` | `#B02A2A` | runs, outs, RBI dots |
| danger-light | `pencil-red-light` | `#F7E4E4` | violation banners |
| warning | `pencil-amber` | `#B8791F` | notices |
| warning-light | `pencil-amber-light` | `#FBF0DC` | notice banners |
| success | `field-green` | `#2E7D32` | confirmations |
| light | `paper-shade` | — | bootstrap `light` |
| rule line | `rule-blue` | `#C6D2E4` | faint grid lines in the scorebook |

Requirements:

- Delete `usda_nass_navy` and the `accent` fallback from `BRAND_COLORS`. Grep the repo
  first; `BRAND_COLORS$accent` and `BRAND_COLORS$usda_nass_navy` must have no remaining
  consumers before removal.
- Fix the stale `map_helpers.R` comment at the top of `R/brand_colors.R`.
- Add `rule_line` and `primary_light` resolution so the scorebook renderer can use them.
- Body text on `paper` and on `paper-shade` must meet WCAG AA (4.5:1). Verify `ink` on
  `paper` and `ink-muted` on `paper-shade` numerically, not by eye.
- `www/css/app.css` keeps the existing `--tap: 3rem` touch-target sizing.

### 2.0.2 Outcome-code glossary

`APP_CONFIG$outcome_codes` has 17 codes with no descriptions anywhere. This task also adds
the 18th, `ITPHR`, so that the code list has exactly one owner: slice 2.0 defines the
vocabulary, slice 2.1 attaches rules to it. 2.0 and 2.1 run in parallel and both touch
`R/app_config.R`; keeping the code list here avoids a merge conflict over it.

Add `APP_CONFIG$outcome_meta`: a list keyed by code, each entry
`list(label = , description = , category = )` where category is one of
`"hit"`, `"on_base"`, `"out"`, `"other"`.

| Code | Label | Category |
|---|---|---|
| `1B` | Single | hit |
| `2B` | Double | hit |
| `3B` | Triple | hit |
| `HR` | Home run (over the fence) | hit |
| `ITPHR` | Inside-the-park home run | hit |
| `BB` | Walk (base on balls) | on_base |
| `IBB` | Intentional walk | on_base |
| `HBP` | Hit by pitch | on_base |
| `FC` | Fielder's choice | on_base |
| `E` | Reached on error | on_base |
| `K` | Strikeout swinging | out |
| `KL` | Strikeout looking | out |
| `GO` | Ground out | out |
| `FO` | Fly out | out |
| `LO` | Line out | out |
| `PO` | Pop out | out |
| `SF` | Sacrifice fly | other |
| `SAC` | Sacrifice bunt | other |

Surface it two ways:

- A **Help** nav panel in the tracking view listing every code grouped by category, with
  its label and description.
- ~~A popover on each outcome button in the action panel showing label + description.
  Use `bslib::popover()`; it must not interfere with the button's click handler on touch.~~

  **Reversed during slice 2.0's final review.** `bslib::popover()` resolves its trigger to
  `"click"` on a `<button>`, so the popover fires on the same tap that records the play —
  the interference this bullet tried to forbid is unavoidable with that component. On a
  mobile-first app whose primary loop is tapping ten buttons a hundred times a game, that
  is disqualifying. The owner ruled the popovers out entirely; the Help panel alone carries
  the glossary, and `outcome_button()` was deleted. Any future inline help must not overlay
  the action grid on the recording tap.

`APP_CONFIG$outcome_codes` is derived from `names(APP_CONFIG$outcome_meta)` so the two can
never drift.

### 2.0.3 Box score

`batting_lines()` returns `player_id` as its first column and `tracking_module.R` renders
it with `renderTable()`.

- Drop `player_id` from the returned data frame. Nothing consumes it; `boxscore.R` uses
  `ids` internally, which is unaffected.
- Render with `DT::datatable()` instead of `renderTable()`: sortable columns, no paging,
  no search box, no row names, compact styling. Default sort by batting order.
- DT 0.34.0 is already installed but is **not** in `manifest.json`. Regenerate the manifest
  (`rsconnect::writeManifest()`) as part of this task, or the Posit Connect Cloud deploy
  will fail.
- Table headers use the team name, not `"away"` / `"home"`.

### 2.0.4 Graceful authentication failure

Three separate crash paths exist today:

1. `.gotrue_request()` sets `req_error(is_error = ~FALSE)`, which suppresses HTTP *status*
   errors but not *connectivity* errors. With `SUPABASE_URL` unset the request goes to
   `/auth/v1/token?...` on an empty host and `req_perform()` throws, taking down the
   session. Wrap the perform in `tryCatch()` and return the standard
   `list(ok = FALSE, error = ...)` shape on any condition.
2. `storage_for_identity()` calls `supabase_connect()`, which throws if `RPostgres` is
   missing or the database refuses the connection. `bookworm_server`'s `observeEvent` has
   no handler, so the app dies after a successful sign-in. Wrap it; on failure fall back to
   guest storage and set a flag that renders a persistent warning banner explaining that
   the game will not be saved.
3. Bad credentials already return `res$error`, but the raw GoTrue message
   (`"Invalid login credentials"`) is passed straight through. Map known GoTrue error
   strings to friendly text; fall back to the raw message for unknown ones.

Additionally: when `supabase_configured()` is `FALSE`, the auth screen must say so up front
— disable Sign in / Create account, explain that saving is not configured on this
deployment, and make "Continue as guest" the primary action. Do not let the user submit a
form that cannot succeed.

### 2.0.5 README

Add an **AI roadmap** section recording the two planned AI capabilities:

- **Scorebook photo import.** Upload photos of a paper scorebook page; a vision model
  reads the grid and emits a populated Bookworm game (lineups + plate appearances) for
  review and correction before commit.
- **Ruleset from a rules document.** Upload a league's rules PDF or text; a model extracts
  the parameters into a Bookworm ruleset, presented as a diff against the closest preset
  for the user to confirm.

Also update the Rules-supported and Known-limitations sections to match what slice 2 ships.

---

## Slice 2.1 — Ruleset model and setup flow

### 2.1.1 Ruleset schema

`default_ruleset_config()` becomes:

```r
list(
  preset = "anything_goes",
  starting_count = list(balls = 0L, strikes = 0L),
  foul_out_rule = "unlimited",              # out | one_courtesy_foul | unlimited
  batting_gender_rule = list(type = "none", n = NA_integer_),
  batting_size = NA_integer_,               # NA = everyone bats
  fielding = list(
    fielder_count   = NA_integer_,          # NA = unenforced
    min_females     = 0L,
    max_males       = NA_integer_,
    tiers           = list(),
    position_requirements = list()
  ),
  innings = 7L,
  run_cap = list(
    per_inning           = NA_integer_,     # NA = no cap
    open_last_inning     = TRUE,
    same_play_runs_count = TRUE,            # NEW
    cap_ends_half        = TRUE             # NEW
  ),
  mercy_rule = list(tiers = list()),        # list of {after_inning, differential}
  home_run_rule = list(
    over_fence_limit   = NA_integer_,       # NA = unlimited
    limit_by_gender    = list(),            # optional overrides, e.g. list(M = 2L)
    over_limit_result  = "out",             # out | ground_rule_double | single
    inside_park_counts = FALSE              # ITPHR counts toward the limit?
  ),
  pinch_runner = list(
    max_per_inning          = NA_integer_,  # NA = unlimited
    max_per_game            = NA_integer_,
    max_per_player_per_game = NA_integer_,
    eligibility             = "anyone",     # anyone | same_gender | last_out | last_same_gender_out
    allowed_for             = "anyone"      # anyone | pitcher_catcher
  ),
  short_lineup_auto_out = FALSE
)
```

`batting_gender_rule$type` values:

| Type | `n` | Meaning |
|---|---|---|
| `none` | — | no restriction |
| `max_consecutive_males` | n | at most `n` males may bat in a row |
| `max_consecutive_same_gender` | n | at most `n` of either gender in a row |
| `min_females_per_n` | n | at least one female in every window of `n` batters |

`male_walk_rule` is retained unchanged from slice 1.1.

### 2.1.2 Backward compatibility

Games already in the event log embed their ruleset in the `game_start` payload, and
`fold_events` re-coerces it on every load. `coerce_ruleset_config()` must therefore migrate
the old shapes rather than break:

| Old | New |
|---|---|
| top-level `run_cap_per_inning` | `run_cap$per_inning` |
| top-level `open_last_inning` | `run_cap$open_last_inning` |
| `mercy_rule$differential` + `$after_inning` | single-entry `mercy_rule$tiers` |
| `batting_gender_rule$type == "no_two_males_consecutive"` | `max_consecutive_males`, `n = 1` |
| `batting_gender_rule$type == "every_other"` | `max_consecutive_same_gender`, `n = 1` |
| `batting_gender_rule$type == "every_n"` | `min_females_per_n`, `n` unchanged |
| `courtesy_runner = TRUE` | `pinch_runner` left at defaults (unlimited) |
| `courtesy_runner = FALSE` | `pinch_runner$max_per_game = 0L` |

Migration is idempotent: coercing an already-migrated config is a no-op. A test must
assert that.

Absent keys must not clobber present ones — the current implementation uses
`utils::modifyList()`, which merges recursively and is correct for this, but the migration
step has to run *before* the merge so old scalar keys are not left stranded alongside the
new nested defaults.

Nesting `run_cap` moves fields that four call sites read directly. All of them must be
updated in the same task or the cap silently stops working:

| Site | Reads |
|---|---|
| `R/game_reducer.R:71-72` (`.refresh_flags`) | `run_cap_per_inning`, `open_last_inning` |
| `R/game_reducer.R:130-132` (`half_runs` branch) | `run_cap_per_inning`, `open_last_inning` |
| `R/rules_engine.R:143-145` (`apply_run_cap`) | `run_cap_per_inning`, `open_last_inning` |
| `R/setup_module.R:45` (`collect_ruleset`) | writes `run_cap_per_inning` |

**Behavior change:** the default starting count moves 1‑1 → 0‑0 and `foul_out_rule` moves
`"out"` → `"unlimited"`, because Anything Goes is now the default preset. Tests in
`tests/test_reducer_core.R`, `tests/test_rules_engine.R`, and any other file asserting the
old defaults must be updated, not worked around.

### 2.1.3 Rule evaluation changes

**Batting gender** (`next_batter_gender_ok`): reimplement against the four new types.
`max_consecutive_males` fails when the last `n` batters were all male and the next is male.
`max_consecutive_same_gender` is the same test applied to whichever gender. Retain the
existing signature `(cfg, prev_genders, next_gender)`.

**Run cap** (`apply_run_cap`): now takes the whole `run_cap` block.

- `same_play_runs_count = TRUE`: a play in progress completes fully. A batter one run below
  a 5-run cap who hits a grand slam scores 4; the half then ends.
- `same_play_runs_count = FALSE`: runs are clamped at the cap mid-play (today's behavior).
- `cap_ends_half = TRUE`: reaching or passing the cap ends the half-inning immediately,
  which is what a run cap means in practice. This is a behavior change — today the reducer
  clamps runs and emits a notice but keeps batting.
- `open_last_inning = TRUE` disables the cap from `inning >= innings` onward, unchanged.

**Mercy** (`game_should_end`): evaluate every tier; the game ends if any tier is satisfied
(`inning >= after_inning && abs(diff) >= differential`). An empty `tiers` list means no
mercy rule. The old NA-handling special case goes away.

**Home run limit** (new `evaluate_home_run_limit(cfg, state, batter, outcome)`): counts
`HR` outcomes already recorded by that team this game (and `ITPHR` too when
`inside_park_counts` is `TRUE`). The effective limit is
`limit_by_gender[[batter$gender]]` when present, otherwise `over_fence_limit`. When the
limit is already met and the batter hits another over-the-fence home run, the outcome is
rewritten per `over_limit_result` and a notice explains the substitution. Returns the
effective outcome plus an optional warning; it does not mutate state itself.

**Pinch runners** (new `evaluate_pinch_runner(cfg, state, out_player, in_player)`): checks
count limits (inning, game, per-player) and eligibility:

| Eligibility | Rule |
|---|---|
| `anyone` | any player on the roster |
| `same_gender` | `in_player$gender == out_player$gender` |
| `last_out` | must be the player who made the most recent out |
| `last_same_gender_out` | most recent out by a player of the same gender as `out_player` |

`allowed_for = "pitcher_catcher"` additionally requires `out_player$position` to be `P` or
`C`. Returns `list(ok =, errors = character())`.

Counting requires state the reducer does not track today. Add `state$pinch_runner_log`: a
list of `{inning, half, team, out_player_id, in_player_id}` appended by the `substitution`
reducer branch when `kind == "courtesy_runner"`.

**Fielder count**: when `fielding$fielder_count` is set and the defensive lineup has a
different number of players holding a fielding position, emit a `notice` (not a violation)
— teams legitimately play shorthanded.

### 2.1.4 Presets

New file `R/rule_presets.R` exporting `RULE_PRESETS`, an ordered named list of
`list(id =, label =, description =, config =)`. `preset_ruleset(id)` returns a fully
coerced config. Every preset must round-trip
`validate_ruleset_config(preset_ruleset(id))$ok == TRUE` — assert that for all presets in a
single test.

| Preset | Count | Foul | Batting gender | Batters | Fielders | Innings | Run cap | Mercy | HR limit | Pinch runner |
|---|---|---|---|---|---|---|---|---|---|---|
| **Anything Goes** (default) | 0‑0 | unlimited | none | unlimited | — | 7 | none | none | none | unlimited |
| **Standard Baseball** | 0‑0 | unlimited | none | 9 | 9 | 9 | none | none | none | unlimited |
| **Standard Slowpitch Softball** | 0‑0 | out | none | unlimited | 10 | 7 | none | 20@3, 15@4, 10@5 | none | unlimited |
| **Standard Fastpitch Softball** | 0‑0 | unlimited | none | 9 | 9 | 7 | none | 20@3, 15@4, 10@5 | none | `allowed_for = pitcher_catcher` |
| **GameOn Summer** | 0‑0 | one courtesy foul | max 2 males in a row | unlimited | 10 | 7 | none | none | 3, over-limit = out | 1/inning, same gender |
| **GameOn Spring** | 1‑1 | one courtesy foul | max 2 males in a row | unlimited | 10 | 7 | none | none | 3, over-limit = out | 1/inning, same gender |

Both GameOn presets use `STANDARD_COED_FIELDING` **unchanged**. It already encodes the
GameOn requirements exactly: `min_females = 4`, `max_males = 6`, and tiers at 3F/4F/5F/6F
where `battery = "one"` (pitcher and catcher opposite genders) is equivalent to "one female
at either pitcher or catcher, but not both". Add `fielder_count = 10L`. Do not fork the
constant; add a test asserting the GameOn presets reference it.

Presets marked genderless — those whose `batting_gender_rule$type == "none"` **and** whose
`fielding` block imposes no gender constraints (`min_females == 0`, `max_males` NA, empty
`tiers`) — drive the UI's gender-column suppression. Expose this as a predicate
`ruleset_is_genderless(cfg)` in `R/rules_engine.R` so the setup module and the lineup table
share one definition.

### 2.1.5 Setup flow

Reorder `setup_ui()` to **Rules first, then Teams**:

1. **Ruleset** — a preset picker at the top. Choosing a preset populates every rule control
   below it. An "Advanced" accordion (closed by default) exposes the individual controls;
   editing any of them flips the preset label to "Custom (based on <preset>)". The controls
   must cover every field in the schema, including the new run-cap, mercy-schedule,
   home-run, and pinch-runner blocks. Mercy tiers need add/remove rows.
2. **Teams** — away then home, each with a name field, the lineup table, and a **Save
   lineup** button.

**Save lineup** validates that team's lineup against the current ruleset and reports
results inline beneath the table: batting-size mismatch, batting-order gender violations,
fielding gender-balance violations, fielder count, duplicate jersey numbers, duplicate
names, and blank names in non-blank rows. Violations block nothing — they warn. The button
gives visible confirmation of the saved state (row count, validation summary), which is the
point: today there is no feedback at all until the game starts.

**Start game** remains, and re-runs both teams' validation.

An empty lineup remains legal and still means "track this team by runs per inning", as in
slice 1.1.

### 2.1.6 Lineup table

Replace `.player_row()`'s `d-flex` of loose inputs with a real `<table class="bw-lineup">`.
Hand-rolled: each `<td>` holds the Shiny input it holds today, so `collect_lineup()`'s
input-name scheme (`<prefix>_<field>_<id>`) is unchanged and its tests keep passing.

Columns: **#** (batting order, read-only, derived from row position) · **Name** (text) ·
**Gender** (`selectInput` M/F) · **Jersey** (text input constrained to digits) ·
**Position** (`selectInput` from `APP_CONFIG$positions`) · **×** (remove).

Requirements:

- The **Gender** column — header and every cell — is omitted entirely when
  `ruleset_is_genderless(cfg)`. Not hidden with CSS; not rendered. When it is absent,
  `collect_lineup()` must default the gender field rather than reading a missing input.
- **Jersey** switches from `numericInput` (a spinner, unusable for a 0–99 range) to a
  `textInput` with `inputmode="numeric"` and a digit-only pattern. `collect_lineup()`
  already coerces with `as.integer()`; it must now tolerate a non-numeric string by
  producing `NA_integer_` rather than a warning, and blank must mean "no number" (`NA`),
  not `0L` as it does today.
- Inputs inside cells must not carry Bootstrap's default bottom margin; the table needs to
  read as a table.
- Header row sticks when the lineup is long.
- Usable at 375px wide. If the table cannot fit, it scrolls horizontally with the name
  column pinned — it does not reflow back into stacked inputs.
- **Add player** appends a row; **×** removes one and renumbers the order column.

The existing `insertUI`/`removeUI` row mechanism in `setup_server()` is retained; only the
markup changes.

### 2.1.7 Mid-game lineup entry

New event type `lineup_set` with payload `{team, lineup}`. The reducer replaces
`state$lineups[[team]]`, recomputes the batting index safely (clamp to the new lineup
length), refreshes the current batter, and re-runs `.refresh_flags()`.

This is what lets a team that started as run-only, or a team whose lineup was not ready at
first pitch, be entered during the first inning.

**Deferred warnings:** rule checks that depend on a lineup cannot run while it is empty.
When a `lineup_set` arrives, re-evaluate the batting-order gender rule *retroactively* over
the plate appearances already recorded for that team and, if any are violated, emit a
single `violation` naming the earliest offending batter. This is the "warn once the batter
is registered" requirement. `evaluate_fielding` similarly re-runs against the new lineup.

`validate_event()` gains a `lineup_set` branch: `team` must be `home`/`away` and `lineup`
must be a list.

---

## Slice 2.2 — Play entry and substitutions

### 2.2.1 Runner disposition

`suggest_advances()` stops being authoritative. It is demoted to the *pre-fill* for a modal
and its name should say so; keep the function and its tests, but the reducer no longer
relies on it to be right.

Flow:

- Tapping an outcome with **no runners on base** commits immediately, exactly as today.
- Tapping an outcome with **any runner on base** opens a disposition modal listing every
  runner plus the batter. Each row offers `1 / 2 / 3 / H / OUT`, pre-selected from
  `suggest_advances()`. A live footer shows outs on the play, runs, and an editable RBI
  field defaulted to the count of scored advances. **Commit play** / **Cancel**.

Rules for the modal:

- Every runner must have a selection before Commit enables. Pre-fill means this is
  normally satisfied on open.
- Two runners may not be placed on the same base. Show the conflict inline and block
  Commit.
- A runner may not be placed *behind* their current base.
- Outs on the play are derived: advances flagged `OUT`, plus the batter when the outcome is
  in `.OUT_OUTCOMES` and the batter is not otherwise placed. If derived outs would take the
  half past 3, warn but allow — the scorer is the authority.
- RBI defaults to the number of scored advances but is user-editable, so sacrifice and
  error situations can be scored correctly.

The emitted `plate_appearance` payload keeps its existing shape. `advances` is now
guaranteed complete: it contains one entry per runner who was on base plus the batter
(`from = 0`).

Because `advances` is now authoritative, `apply_plate_appearance()`'s fallback that places
the batter from `p$reached` when no advance covers them becomes a compatibility path for
events written by slice 1.1. Keep it; add a comment saying so.

### 2.2.2 Runner origin tracking

`apply_plate_appearance()` gains bookkeeping the scorebook needs in slice 2.3:

- `state$runner_origin`: a named list mapping a runner currently on base to the 1-based
  index in `pa_log` of the plate appearance that put them there.
- On each PA, the batter's own advance gets the index the entry is about to occupy —
  `length(state$pa_log) + 1L`, resolved before the entry is appended. Every other advance
  looks its origin up in `runner_origin`. A runner with no recorded origin (possible when a
  lineup is set mid-inning, or for a slice 1.1 event) gets `NA_integer_`.
- Each advance stored in `pa_log` carries its resolved `origin_index`.
- Runners who score or are put out are dropped from `runner_origin`.
- The `courtesy_runner` substitution branch transfers the entry:
  `runner_origin[[in_id]] <- runner_origin[[out_id]]`, then removes `out_id`. Without this,
  a pinch runner's run would be credited to no cell.

`pa_log` entries also gain `pa_index_in_half` (1-based, per team per half) so the scorebook
can split inning columns, and `outs_before` so out numbering can be derived.

### 2.2.3 Substitutions

The Substitution button currently has no observer at all — it is inert. Wire it to a modal
with three kinds, all of which the reducer's `apply_substitution()` already handles:

- **Batting substitution** — pick an order slot, enter the incoming player. Replaces the
  lineup entry at that slot.
- **Defensive substitution** — pick the outgoing player, enter the incoming player and
  position.
- **Pinch runner** — pick an occupied base, pick or enter the incoming runner. Validated by
  `evaluate_pinch_runner()`; failures are shown inline in the modal and block the
  substitution. Success appends to `state$pinch_runner_log`.

The modal only offers substitutions for the team that is currently batting (pinch runner)
or fielding (defensive); batting substitutions are offered for either team.

`apply_substitution()` needs two fixes found while reading it:

- The `batting` branch overwrites the lineup entry wholesale, dropping the outgoing
  player's identity. That is acceptable for now but the incoming player must be built with
  `make_player()` so it carries every field; currently it takes `p$in_player` as-is and only
  patches `order_slot`.
- The `defensive` branch has the same issue with `position`.

---

## Slice 2.3 — Scorebook and presentation

### 2.3.1 Runner paths

New file `R/runner_paths.R`.

```r
runner_paths(state) -> list of per-plate-appearance path records
```

One record per `pa_log` entry, in order:

```r
list(
  pa_index         = <int>,   # index into pa_log
  team             = ,
  inning           = ,
  half             = ,
  pa_index_in_half = ,
  batter_id        = ,
  outcome          = ,
  fielding         = ,
  rbi              = ,
  bases_reached    = <int 0..4>,  # furthest base this batter reached on this trip
  scored           = <lgl>,
  out_at           = <int|NA>,    # base at which the trip ended in an out
  out_number       = <int|NA>     # which out of the half-inning (1, 2, or 3)
)
```

Algorithm — a single forward pass over `pa_log`:

1. For each entry `i`, seed `bases_reached[i]` from the batter's own advance
   (`from == 0`), or from the legacy `reached` field when no such advance exists.
2. For every advance in every entry, if `origin_index` is set, update that origin's record:
   `bases_reached <- max(bases_reached, to)`; set `scored` when the advance scored; set
   `out_at` and `out_number` when it was an out.
3. Out numbers come from a running per-half counter seeded by each entry's `outs_before`.
   When one play produces more than one out, they are numbered in the order the advances
   appear in the payload, with the batter's own out — if the outcome is in `.OUT_OUTCOMES`
   and no advance covers the batter — numbered last. The counter resets at each half.

This is the fix for "no other users' scored runs appear": today
`scorebook_cell_svg()` reads only `rec$reached`, which is frozen at the moment of the
plate appearance and never learns that the runner later came around to score.

The function is pure and takes only `state`. It must be tested independently of any
rendering: a fixture where a batter singles, advances to second on the next batter's walk,
to third on a ground out, and scores on a sacrifice fly must produce
`bases_reached = 4, scored = TRUE` on the *first* batter's record.

### 2.3.2 Scorebook cell

Rewrite `scorebook_cell_svg()` to take a path record.

Diamond geometry: home at bottom-center, first at right, second at top, third at left.
Four legs — home→1, 1→2, 2→3, 3→home.

- Leg `j` is drawn **heavy** (`stroke-width` 3, `ink`) when `bases_reached >= j`, and
  **light** (`stroke-width` 1, `ink-muted` at low opacity) otherwise. A runner standing on
  third therefore shows three heavy legs and one light one, which is precisely the
  "make it clear they are on 3B" requirement — and it updates live, mid-inning, as the
  runner advances on later plays.
- When `scored`, all four legs are heavy and the interior is filled with `primary_light`.
- `out_number`, when present, renders as a circled numeral at the top-right in `ink`.
- `rbi` renders as that many small `pencil-red` dots at the top-left, capped at 4 glyphs.
- `out_at`, when present, renders a small `×` at that base's vertex.
- Outcome code plus fielding notation renders below the diamond, centered.

Cells are 64px. Verify legibility at that size before considering the task done — the whole
point of this slice is that the current rendering is unreadable.

### 2.3.3 Scorebook grid

Rewrite `render_scorebook_svg()`:

- **Batting around.** Compute `max(pa_index_in_half)` per (team, inning). An inning whose
  maximum is 1 renders a single column; an inning whose maximum is `k` renders `k`
  side-by-side sub-columns under one merged inning header. Today two plate appearances in
  the same inning draw at the same `x` and overlap. Columns only split in innings where
  someone actually batted twice.
- **Current batter.** The batting team's current batter's row label renders bold, with a
  subtle row highlight. Only for the team currently batting.
- **Row labels.** `#<jersey> <name>`, with the jersey omitted when `NA`. Today a missing
  jersey still emits a leading space.
- Faint `rule-blue` horizontal and vertical rules between cells so the grid reads as a
  scorebook page rather than floating diamonds.
- Innings shown is `max(state$inning, number of innings with recorded plate appearances)`,
  so a completed game does not truncate.
- Horizontal scroll on narrow screens, with the name column pinned.

Both teams' scorebooks should be reachable — today only the batting team's renders, and it
swaps out from under you every half-inning. Put them behind a team toggle or render both
stacked.

### 2.3.4 Situation summary card

Replace `output$situation`'s bare `d-flex` with a single card showing, at one glance:

- Score, with the batting team indicated
- Inning and half (with an arrow glyph, not the words "top"/"bottom")
- Outs
- Count (balls–strikes)
- Current batter: jersey, name, position in the order
- That batter's in-game line (e.g. `1-for-2, 2B, RBI`)
- `FINAL` state when the game has ended

The batter's in-game line is derived from `batting_lines()` filtered to that player, so
there is no new statistics code.

Layout must survive 375px width. Score and count are the two things a scorekeeper looks at
most; they get the most visual weight.

### 2.3.5 Information architecture

- Tracking view tabs become: **Scorebook** · **Box score** · **Help**.
- Box score gains the away/home team names as headers and the sortable DT from slice 2.0.
- The Undo and Substitution buttons move next to each other in a clearly secondary
  position, below the primary outcome grid.

---

## Testing

Every slice extends the existing `tests/test_*.R` suite, run with `Rscript run_tests.R`.
The suite is plain `testthat` sourced by a loop; follow the existing file conventions.

**Slice 2.0**
- `test_app_config.R`: every code in `outcome_meta` has a label, description, and valid
  category; `outcome_codes` matches `names(outcome_meta)`.
- New `test_brand.R`: required `BRAND_COLORS` keys resolve to hex; removed keys are gone;
  `ink`-on-`paper` and `ink_muted`-on-`paper_shade` contrast ratios are ≥ 4.5.
- `test_boxscore.R`: `batting_lines()` has no `player_id` column.
- `test_auth_module.R`: a `sign_in` that throws yields an error message rather than
  propagating; unconfigured Supabase produces the guest-only auth state.
- New `test_storage_fallback.R`: `storage_for_identity()` returns guest storage when
  `supabase_connect()` throws.

**Slice 2.1**
- `test_rules_engine.R`: each migration in 2.1.2, plus idempotency; all four
  `batting_gender_rule` types; mercy tiers; `apply_run_cap()` under all four combinations
  of `same_play_runs_count` × `cap_ends_half`; the grand-slam-at-the-cap case explicitly.
- New `test_rule_presets.R`: every preset validates; GameOn presets reference
  `STANDARD_COED_FIELDING`; `ruleset_is_genderless()` is TRUE for Anything Goes and the
  three Standard presets and FALSE for both GameOn presets.
- New `test_home_run_limit.R` and `test_pinch_runner.R` covering every eligibility mode and
  limit combination, including per-gender overrides and `allowed_for`.
- `test_setup_module.R`: `collect_lineup()` with no gender input present; blank jersey →
  `NA_integer_` not `0L`; non-numeric jersey → `NA_integer_` without a warning.
- `test_reducer_core.R`: `lineup_set` replaces a lineup, clamps the batting index, and
  triggers retroactive gender-rule evaluation.

**Slice 2.2**
- New `test_disposition.R`: pre-fill matches `suggest_advances()`; duplicate-base and
  backward-move rejection; derived outs and RBI default.
- `test_reducer_pa.R`: `runner_origin` is seeded, transferred through a pinch runner, and
  cleared on score and on out; `pa_index_in_half` increments per half.
- `test_reducer_subs.R`: all three substitution kinds through the modal's event shape;
  pinch-runner limit rejection.

**Slice 2.3**
- New `test_runner_paths.R`: the four-plate-appearance fixture from 2.3.1; a runner put out
  on the basepaths; a pinch runner scoring credited to the original batter's cell; a
  legacy event with no `origin_index`.
- `test_scorebook_render.R`: heavy-leg count matches `bases_reached`; scored cells fill;
  batting around produces `k` sub-columns; the current batter's label is bold; output is
  valid SVG.

Regression bar: `Rscript run_tests.R` exits 0 at the end of every task, not just every
slice.

## Risks

- **Ruleset migration.** Every game in the event log re-folds its embedded ruleset on load.
  A migration bug corrupts existing games silently rather than loudly. Mitigated by
  idempotency tests and by keeping the legacy `reached` fallback in
  `apply_plate_appearance()`.
- **Behavior change on run caps.** `cap_ends_half` defaults to TRUE, which changes how
  existing capped games fold. Acceptable — the current behavior is wrong — but it must be
  called out in the README.
- **DT and `manifest.json`.** Adding a package without regenerating the manifest breaks the
  Posit Connect Cloud deploy, and the app is local-only right now so the failure would not
  surface until deploy day.
- **Modal friction.** A disposition modal on every play with a runner on base is more taps
  than today. Pre-fill is what keeps it usable; if the pre-fill is wrong often, the feature
  is a burden rather than a fix.
