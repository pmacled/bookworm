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

## Known limitations
- Undo reverts in-session; persisted event rows are pruned in a later phase.
- Lineups are entered per game (no cross-game roster persistence yet — Phase 3).
- Row-Level Security is defined but not enforced (app-level owner scoping).
- Fielding gender tiers are configurable via presets + base knobs; full per-tier
  hand-editing in the UI is deferred (the engine supports arbitrary tiers).
- The mercy schedule is three fixed rows in the UI; the engine accepts any number
  of tiers, and add/remove rows are deferred.
- Pinch-/courtesy-runner allowances are configurable and validated by the engine,
  but nothing in the tracking UI records a runner substitution yet (slice 2.2).
- The home-run limit is enforced by the engine; wiring it into play entry is
  slice 2.2.
- Sign-in requires Supabase configuration; without it the app runs guest-only and says so.

## Rules supported

Setup is **rules first, then teams**. A preset picker at the top of the New game
screen fills in every rule control below it; editing any control relabels the
saved ruleset `custom`. Built-in presets: Anything Goes (the genderless default),
Standard Baseball, Standard Slowpitch Softball, Standard Fastpitch Softball,
GameOn Summer and GameOn Spring.

Each team gets a lineup table (order, name, gender, jersey, position). **Save
lineup** validates it against the current ruleset and reports inline: batting-size
mismatch, duplicate jerseys, duplicate names, batting-order gender breaks
(including where the order wraps from the last slot back to the first), fielder
count, and fielding gender balance. Nothing blocks the scorer — a knowingly
illegal lineup is still allowed. For a ruleset that never mentions gender, the
Gender column disappears entirely. A team with an empty lineup is tracked by runs
per inning.

Configurable rules:

- **Count and fouls** — arbitrary starting count; foul with two strikes is an out /
  one courtesy foul / unlimited.
- **Batting order** — number of batters (unlimited / 9 / 10); gender order as none,
  max N males in a row, max N of either gender in a row, or at least one F every N.
- **Innings and run cap** — a per-inning run cap, with `open_last_inning` (no cap in
  the final inning), `same_play_runs_count` (a play in progress finishes in full, so a
  grand slam past the cap still counts for four), and `cap_ends_half` (reaching the
  cap ends the half-inning, which is what a run cap means in practice).
- **Mercy** — any number of `{after_inning, differential}` tiers; the game ends as
  soon as one is satisfied. `after_inning` counts *completed* innings and is checked
  only at the end of a half-inning. Game status is derived from state, so an Undo or
  a differential that shrinks again reopens the game.
- **Home runs** — an over-the-fence limit, optional per-gender overrides, what a home
  run past the limit becomes (out / ground-rule double / single), and whether
  inside-the-park home runs count toward the limit.
- **Pinch / courtesy runners** — max per inning, per game and per player; who may
  run (anyone / same gender / the last out / the last same-gender out) and who may
  be run for (anyone / pitcher or catcher only).
- **Coed fielding** — min females, max males, per-category minimums (outfield /
  infield), pitcher and catcher opposite genders, and count-specific tiers that
  relax the requirements as more women take the field.

Rulesets from before this slice are migrated on load, so older saved games keep
working.

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
