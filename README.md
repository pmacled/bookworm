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

## Rules supported
Arbitrary starting count; foul-with-2-strikes (out / one courtesy foul / unlimited);
batting gender order (none / no-two-males / every-other / one-F-every-N); number of
batters (unlimited / 9 / 10); innings, per-inning run cap, mercy; coed fielding
gender balance (min females, max males, per-category minimums, P/C opposite, and
count-specific tiers). A team with an empty lineup is tracked by runs per inning.

## Roadmap
- Phase 2: vision-LLM photo import.
- Phase 3: team management, sharing, standings, RLS.
