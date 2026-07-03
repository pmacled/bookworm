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
