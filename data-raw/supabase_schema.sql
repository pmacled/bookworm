-- Bookworm schema. Apply in the Supabase SQL editor.
-- Self-managed username/password auth (no Supabase Auth / GoTrue).
-- The app connects with the owner's Supabase secrets (set in .Renviron); end users never see the database and authenticate against the
-- `users` table below.

create extension if not exists "pgcrypto";  -- crypt(), gen_salt(), gen_random_uuid()
create extension if not exists "citext";    -- case-insensitive username

-- ---------------------------------------------------------------------------
-- Users
-- ---------------------------------------------------------------------------
-- Username is citext (case-insensitive: MIKE == mike) with a unique index.
-- Passwords are stored ONLY as a bcrypt hash produced by
--   crypt(:password, gen_salt('bf')).
-- Verify in the app with:  password_hash = crypt(:password, password_hash).
create table if not exists users (
  id uuid primary key default gen_random_uuid(),
  username citext not null unique
    check (char_length(username) between 3 and 30
           and username ~ '^[A-Za-z0-9_]+$'),
  password_hash text not null,
  display_name text,
  is_admin boolean not null default false,
  -- Coarse role kept alongside is_admin for future granularity.
  role text not null default 'user' check (role in ('user','admin')),
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

-- ---------------------------------------------------------------------------
-- Remember-me tokens (persistent login)
-- ---------------------------------------------------------------------------
-- Supports "keep me signed in". The browser stores a random token in
-- localStorage; the server stores ONLY its sha256 hash here, so a database read
-- alone cannot be replayed as a login. A token is validated by hashing the
-- presented value and matching token_hash while not expired.
create table if not exists auth_tokens (
  token_hash text primary key,
  user_id uuid not null references users(id) on delete cascade,
  expires_at timestamptz not null,
  created_at timestamptz not null default now(),
  last_used_at timestamptz
);
create index if not exists idx_auth_tokens_user on auth_tokens(user_id);

-- ---------------------------------------------------------------------------
-- Leagues / rulesets / teams / players
-- ---------------------------------------------------------------------------
create table if not exists leagues (
  id uuid primary key default gen_random_uuid(),
  owner_id uuid not null references users(id) on delete cascade,
  name text not null check (char_length(name) > 0),
  sport text not null default 'softball' check (sport in ('softball','baseball')),
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  unique (owner_id, name)
);

-- ---------------------------------------------------------------------------
-- League membership
-- ---------------------------------------------------------------------------
-- Any member of a league may VIEW that league's games. The league owner is a
-- member implicitly (the app treats owner_id as a member; membership rows are
-- added for other users by exact username lookup, so no user list is exposed).
-- `role` allows a future distinction between viewers and league managers.
create table if not exists league_members (
  league_id uuid not null references leagues(id) on delete cascade,
  user_id uuid not null references users(id) on delete cascade,
  role text not null default 'member' check (role in ('member','manager')),
  created_at timestamptz not null default now(),
  primary key (league_id, user_id)
);

create table if not exists rulesets (
  id uuid primary key default gen_random_uuid(),
  league_id uuid not null references leagues(id) on delete cascade,
  name text not null check (char_length(name) > 0),
  config jsonb not null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  unique (league_id, name)
);

create table if not exists teams (
  id uuid primary key default gen_random_uuid(),
  league_id uuid not null references leagues(id) on delete cascade,
  name text not null check (char_length(name) > 0),
  -- The team's captain: a user who can manage that team's players. Setting a
  -- captain also adds them to league_members so they can view league games.
  -- Null-out on user delete so the team survives its captain leaving.
  captain_user_id uuid references users(id) on delete set null,
  created_at timestamptz not null default now(),
  unique (league_id, name)
);
create index if not exists idx_teams_captain on teams(captain_user_id);

create table if not exists players (
  id uuid primary key default gen_random_uuid(),
  team_id uuid not null references teams(id) on delete cascade,
  name text not null check (char_length(name) > 0),
  gender text check (gender in ('M','F')),
  jersey_number int check (jersey_number between 0 and 999),
  default_position int check (default_position between 1 and 10),
  created_at timestamptz not null default now(),
  -- Jersey numbers unique within a team when present.
  unique (team_id, jersey_number)
);

-- ---------------------------------------------------------------------------
-- Games
-- ---------------------------------------------------------------------------
-- Deleting a league/ruleset/team must NOT silently wipe recorded games, so
-- those references null-out (games) rather than cascade-delete. Deleting the
-- owning user removes their standalone games but PRESERVES games assigned to a
-- league: ownership transfers to the league owner (see the before-delete
-- trigger on users below). Any league member may view a league's games.
create table if not exists games (
  id uuid primary key default gen_random_uuid(),
  owner_id uuid references users(id) on delete set null,
  league_id uuid references leagues(id) on delete set null,
  ruleset_id uuid references rulesets(id) on delete set null,
  home_team_id uuid references teams(id) on delete set null,
  away_team_id uuid references teams(id) on delete set null,
  played_on date,
  location text,
  status text not null default 'in_progress' check (status in ('in_progress','final')),
  state_snapshot jsonb,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  check (home_team_id is null or away_team_id is null or home_team_id <> away_team_id)
);

-- ---------------------------------------------------------------------------
-- Game sharing
-- ---------------------------------------------------------------------------
-- Grants another user read (and optionally edit) access to a single game.
-- Sharing is by exact username lookup in the app, so no user list is exposed.
-- For now the app should treat only the owner as editable; can_edit is stored
-- for when shared editing is turned on.
create table if not exists game_shares (
  game_id uuid not null references games(id) on delete cascade,
  user_id uuid not null references users(id) on delete cascade,
  can_edit boolean not null default false,
  created_at timestamptz not null default now(),
  primary key (game_id, user_id)
);

-- ---------------------------------------------------------------------------
-- Event log + derived plate appearances
-- ---------------------------------------------------------------------------
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
  inning int,
  half text check (half in ('top','bottom')),
  team_id uuid references teams(id) on delete set null,
  batter_id uuid references players(id) on delete set null,
  batting_order_slot int,
  outcome text,
  fielding_notation text,
  rbi int,
  outs_recorded int,
  errors jsonb,
  base_advancement jsonb,
  count_at_end jsonb,
  seq int,
  unique (game_id, seq)
);

-- ---------------------------------------------------------------------------
-- Indexes
-- ---------------------------------------------------------------------------
create index if not exists idx_game_events_game on game_events(game_id, seq);
create index if not exists idx_plate_appearances_game on plate_appearances(game_id, seq);
create index if not exists idx_games_owner on games(owner_id);
create index if not exists idx_game_shares_user on game_shares(user_id);
create index if not exists idx_leagues_owner on leagues(owner_id);
create index if not exists idx_league_members_user on league_members(user_id);
create index if not exists idx_games_league on games(league_id);

-- ---------------------------------------------------------------------------
-- updated_at maintenance
-- ---------------------------------------------------------------------------
create or replace function set_updated_at() returns trigger as $$
begin
  new.updated_at = now();
  return new;
end;
$$ language plpgsql;

do $$
declare t text;
begin
  foreach t in array array['users','leagues','rulesets','games']
  loop
    execute format(
      'drop trigger if exists trg_%1$s_updated_at on %1$s;
       create trigger trg_%1$s_updated_at before update on %1$s
       for each row execute function set_updated_at();', t);
  end loop;
end $$;

-- ---------------------------------------------------------------------------
-- User deletion: transfer league games, remove standalone games
-- ---------------------------------------------------------------------------
-- FK actions can't be conditional, so `owner_id ... on delete set null` alone
-- would orphan ALL of a deleted user's games. This before-delete trigger runs
-- first and, for the departing user's games:
--   * league games  -> ownership transfers to the league's owner (so a league
--                       game always has a real owner and stays visible to the
--                       league). If the departing user IS the league owner, the
--                       FK's `set null` applies and league membership still
--                       governs access.
--   * standalone games (no league) -> deleted outright.
create or replace function reassign_or_purge_games_on_user_delete()
returns trigger as $$
begin
  -- Transfer league-assigned games to the league owner, unless the departing
  -- user is that owner (nothing to transfer to; FK will null the owner).
  update games g
     set owner_id = l.owner_id
    from leagues l
   where g.league_id = l.id
     and g.owner_id = old.id
     and l.owner_id <> old.id;

  -- Remove the user's standalone games (no league to preserve them).
  delete from games
   where owner_id = old.id
     and league_id is null;

  return old;
end;
$$ language plpgsql;

drop trigger if exists trg_users_purge_standalone_games on users;
drop trigger if exists trg_users_reassign_or_purge_games on users;
create trigger trg_users_reassign_or_purge_games
  before delete on users
  for each row execute function reassign_or_purge_games_on_user_delete();

-- ---------------------------------------------------------------------------
-- Row-Level Security
-- ---------------------------------------------------------------------------
-- NOTE: because the app connects with a single service/owner credential (your
-- Supabase secret) and identifies users itself against the `users` table,
-- Postgres RLS based on auth.uid() does NOT apply here. Access scoping
-- (owner_id = signed-in user, plus league membership and game_shares) is
-- enforced in the R application layer. Keep the service key server-side only (Posit Connect
-- Cloud secrets); never ship it to the client.

-- ---------------------------------------------------------------------------
-- Incremental migrations (safe to re-run; for deployments created before a
-- column existed). The create-table blocks above are the source of truth for
-- fresh installs; these keep an already-provisioned database in sync.
-- ---------------------------------------------------------------------------
-- teams.captain_user_id (added for team-captain management).
alter table teams
  add column if not exists captain_user_id uuid references users(id) on delete set null;
create index if not exists idx_teams_captain on teams(captain_user_id);
