# App configuration and small shared utilities. Sourced early (with brand_colors.R).

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

APP_CONFIG <- list(
  app_name = "Bookworm",
  positions = c(
    "P",
    "C",
    "1B",
    "2B",
    "3B",
    "SS",
    "LF",
    "LCF",
    "CF",
    "RCF",
    "RF",
    "OF",
    "ROVER",
    "DH"
  ),
  POSITION_CATEGORY = c(
    P = "battery",
    C = "battery",
    "1B" = "infield",
    "2B" = "infield",
    SS = "infield",
    "3B" = "infield",
    LF = "outfield",
    LCF = "outfield",
    CF = "outfield",
    RCF = "outfield",
    RF = "outfield",
    OF = "outfield",
    ROVER = "outfield"
    # DH and blank are intentionally absent: they are non-fielders.
  ),
  outcome_meta = list(
    "1B" = list(
      label = "Single",
      category = "hit",
      description = "Batter reaches first base safely on a batted ball."
    ),
    "2B" = list(
      label = "Double",
      category = "hit",
      description = "Batter reaches second base safely on a batted ball."
    ),
    "3B" = list(
      label = "Triple",
      category = "hit",
      description = "Batter reaches third base safely on a batted ball."
    ),
    "HR" = list(
      label = "Home run (over the fence)",
      category = "hit",
      description = "Ball leaves the park in fair territory. Counts toward a league home-run limit."
    ),
    "ITPHR" = list(
      label = "Inside-the-park home run",
      category = "hit",
      description = "Batter circles the bases on a ball that stays in play. Normally exempt from a home-run limit."
    ),
    "BB" = list(
      label = "Walk",
      category = "on_base",
      description = "Four balls; batter is awarded first base. Not an at-bat."
    ),
    "IBB" = list(
      label = "Intentional walk",
      category = "on_base",
      description = "Defence deliberately awards first base. Not an at-bat."
    ),
    "HBP" = list(
      label = "Hit by pitch",
      category = "on_base",
      description = "Pitch strikes the batter; awarded first base. Not an at-bat."
    ),
    "FC" = list(
      label = "Fielder's choice",
      category = "on_base",
      description = "Batter reaches because the defence chose to retire another runner."
    ),
    "E" = list(
      label = "Reached on error",
      category = "on_base",
      description = "Batter reaches because of a defensive misplay. Counts as an at-bat, not a hit."
    ),
    "K" = list(
      label = "Strikeout swinging",
      category = "out",
      description = "Third strike with the batter swinging."
    ),
    "KL" = list(
      label = "Strikeout looking",
      category = "out",
      description = "Third strike called with the batter not swinging."
    ),
    "GO" = list(
      label = "Ground out",
      category = "out",
      description = "Ground ball fielded and thrown out."
    ),
    "FO" = list(
      label = "Fly out",
      category = "out",
      description = "Fly ball caught in the air."
    ),
    "LO" = list(
      label = "Line out",
      category = "out",
      description = "Line drive caught in the air."
    ),
    "PO" = list(
      label = "Pop out",
      category = "out",
      description = "Short, high pop-up caught in the air."
    ),
    "SF" = list(
      label = "Sacrifice fly",
      category = "other",
      description = "Fly out that scores a runner. Not an at-bat; the batter is credited an RBI."
    ),
    "SAC" = list(
      label = "Sacrifice bunt",
      category = "other",
      description = "Bunt out that advances a runner. Not an at-bat."
    )
  )
)

# Derived so the code list and the glossary can never drift apart.
APP_CONFIG$outcome_codes <- names(APP_CONFIG$outcome_meta)

# Supabase table names (Postgres), one source of truth.
DB_TABLES <- list(
  profiles = "profiles",
  leagues = "leagues",
  rulesets = "rulesets",
  teams = "teams",
  players = "players",
  games = "games",
  game_events = "game_events",
  plate_appearances = "plate_appearances"
)
