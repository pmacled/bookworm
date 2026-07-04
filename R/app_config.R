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
