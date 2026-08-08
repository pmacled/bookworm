# App configuration and small shared utilities. Sourced early (with brand_colors.R).

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

APP_CONFIG <- list(
  app_name = "Bookworm",
  positions = c("P","C","1B","2B","3B","SS","LF","LCF","CF","RCF","RF","OF","ROVER","DH"),
  POSITION_CATEGORY = c(
    P = "battery", C = "battery",
    "1B" = "infield", "2B" = "infield", SS = "infield", "3B" = "infield",
    LF = "outfield", LCF = "outfield", CF = "outfield", RCF = "outfield",
    RF = "outfield", OF = "outfield", ROVER = "outfield"
    # DH and blank are intentionally absent: they are non-fielders.
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
