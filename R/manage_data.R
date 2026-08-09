# Data-access layer for league / team / player management. Thin, parameterized
# DBI helpers in the style of supabase_client.R. Every function takes a live
# connection `con` (the app already holds one via storage_for_identity), which
# also makes them trivial to fake in tests. Access scoping is enforced here in
# the R layer (owner manages everything; a team captain manages their team's
# players), consistent with the rest of the app.
#
# Return shape convention for mutating calls: list(ok = TRUE/FALSE, ...,
# error = <friendly message or NA>). Read calls return data frames.

.manage_error <- function(msg) {
  list(ok = FALSE, error = msg)
}

# Map internal codes to friendly copy; pass anything else through.
.manage_friendly <- function(msg) {
  known <- c(
    "name_required" = "Please enter a name.",
    "name_taken" = "That name is already in use.",
    "user_not_found" = "No user with that username was found.",
    "not_permitted" = "You do not have permission to do that.",
    "username_invalid" = "Usernames are 3-30 characters: letters, numbers, or underscore."
  )
  if (!is.null(msg) && length(msg) == 1 && !is.na(msg) && msg %in% names(known)) {
    return(unname(known[[msg]]))
  }
  msg %||% "Something went wrong. Please try again."
}

# ---------------------------------------------------------------------------
# Permission helpers
# ---------------------------------------------------------------------------
db_user_owns_league <- function(con, user_id, league_id) {
  row <- DBI::dbGetQuery(
    con,
    "select 1 from leagues where id = $1 and owner_id = $2",
    params = list(league_id, user_id)
  )
  nrow(row) > 0
}

# TRUE if the user owns the team's league OR is that team's captain.
db_user_manages_team <- function(con, user_id, team_id) {
  row <- DBI::dbGetQuery(
    con,
    "select 1
       from teams t
       join leagues l on l.id = t.league_id
      where t.id = $1
        and (l.owner_id = $2 or t.captain_user_id = $2)",
    params = list(team_id, user_id)
  )
  nrow(row) > 0
}

# ---------------------------------------------------------------------------
# Users (exact-username lookup only; no enumeration)
# ---------------------------------------------------------------------------
db_find_user_by_username <- function(con, username) {
  if (!.valid_username(username)) {
    return(.manage_error(.manage_friendly("username_invalid")))
  }
  row <- DBI::dbGetQuery(
    con,
    "select id from users where username = $1",
    params = list(username)
  )
  if (nrow(row) != 1) {
    return(.manage_error(.manage_friendly("user_not_found")))
  }
  list(ok = TRUE, user_id = row$id[[1]], error = NA_character_)
}

# ---------------------------------------------------------------------------
# Leagues
# ---------------------------------------------------------------------------
# Leagues the user owns or is a member of. `is_owner` drives edit permissions.
db_list_leagues <- function(con, user_id) {
  DBI::dbGetQuery(
    con,
    "select l.id, l.name, l.sport, (l.owner_id = $1) as is_owner
       from leagues l
      where l.owner_id = $1
         or exists (select 1 from league_members m
                     where m.league_id = l.id and m.user_id = $1)
      order by l.name",
    params = list(user_id)
  )
}

db_create_league <- function(con, user_id, name, sport = "softball") {
  name <- trimws(name %||% "")
  if (!nzchar(name)) {
    return(.manage_error(.manage_friendly("name_required")))
  }
  tryCatch(
    {
      row <- DBI::dbGetQuery(
        con,
        "insert into leagues (owner_id, name, sport)
           values ($1, $2, $3) returning id",
        params = list(user_id, name, sport)
      )
      list(ok = TRUE, league_id = row$id[[1]], error = NA_character_)
    },
    error = function(e) {
      if (grepl("unique|duplicate", conditionMessage(e), ignore.case = TRUE)) {
        .manage_error(.manage_friendly("name_taken"))
      } else {
        .manage_error(.manage_friendly(NULL))
      }
    }
  )
}

db_rename_league <- function(con, user_id, league_id, name) {
  name <- trimws(name %||% "")
  if (!nzchar(name)) {
    return(.manage_error(.manage_friendly("name_required")))
  }
  if (!db_user_owns_league(con, user_id, league_id)) {
    return(.manage_error(.manage_friendly("not_permitted")))
  }
  tryCatch(
    {
      DBI::dbExecute(
        con,
        "update leagues set name = $1 where id = $2",
        params = list(name, league_id)
      )
      list(ok = TRUE, error = NA_character_)
    },
    error = function(e) .manage_error(.manage_friendly("name_taken"))
  )
}

db_delete_league <- function(con, user_id, league_id) {
  # Owner-only. Teams/players cascade; games null-out their league_id.
  n <- DBI::dbExecute(
    con,
    "delete from leagues where id = $1 and owner_id = $2",
    params = list(league_id, user_id)
  )
  if (n > 0) {
    list(ok = TRUE, error = NA_character_)
  } else {
    .manage_error(.manage_friendly("not_permitted"))
  }
}

# ---------------------------------------------------------------------------
# Teams
# ---------------------------------------------------------------------------
db_list_teams <- function(con, league_id) {
  DBI::dbGetQuery(
    con,
    "select t.id, t.name, t.captain_user_id, u.username as captain_username
       from teams t
       left join users u on u.id = t.captain_user_id
      where t.league_id = $1
      order by t.name",
    params = list(league_id)
  )
}

# Create a team, optionally assigning a captain by username. Assigning a captain
# also adds them to league_members (role 'member') so they can view league
# games. Owner-only (only a league owner adds teams).
db_create_team <- function(con, user_id, league_id, name, captain_username = NULL) {
  name <- trimws(name %||% "")
  if (!nzchar(name)) {
    return(.manage_error(.manage_friendly("name_required")))
  }
  if (!db_user_owns_league(con, user_id, league_id)) {
    return(.manage_error(.manage_friendly("not_permitted")))
  }
  captain_id <- NA_character_
  captain_username <- trimws(captain_username %||% "")
  if (nzchar(captain_username)) {
    found <- db_find_user_by_username(con, captain_username)
    if (!isTRUE(found$ok)) {
      return(found)
    }
    captain_id <- found$user_id
  }
  tryCatch(
    {
      row <- DBI::dbGetQuery(
        con,
        "insert into teams (league_id, name, captain_user_id)
           values ($1, $2, $3) returning id",
        params = list(league_id, name, if (is.na(captain_id)) NA else captain_id)
      )
      if (!is.na(captain_id)) {
        .add_league_member(con, league_id, captain_id)
      }
      list(ok = TRUE, team_id = row$id[[1]], error = NA_character_)
    },
    error = function(e) {
      if (grepl("unique|duplicate", conditionMessage(e), ignore.case = TRUE)) {
        .manage_error(.manage_friendly("name_taken"))
      } else {
        .manage_error(.manage_friendly(NULL))
      }
    }
  )
}

db_rename_team <- function(con, user_id, team_id, name) {
  name <- trimws(name %||% "")
  if (!nzchar(name)) {
    return(.manage_error(.manage_friendly("name_required")))
  }
  if (!db_user_manages_team(con, user_id, team_id)) {
    return(.manage_error(.manage_friendly("not_permitted")))
  }
  tryCatch(
    {
      DBI::dbExecute(
        con,
        "update teams set name = $1 where id = $2",
        params = list(name, team_id)
      )
      list(ok = TRUE, error = NA_character_)
    },
    error = function(e) .manage_error(.manage_friendly("name_taken"))
  )
}

# Set (or clear, with empty username) a team's captain. Owner-only, since it
# grants management rights. Adding a captain also grants league membership.
db_set_team_captain <- function(con, user_id, team_id, captain_username) {
  lg <- DBI::dbGetQuery(
    con,
    "select league_id from teams where id = $1",
    params = list(team_id)
  )
  if (nrow(lg) != 1) {
    return(.manage_error(.manage_friendly("not_permitted")))
  }
  league_id <- lg$league_id[[1]]
  if (!db_user_owns_league(con, user_id, league_id)) {
    return(.manage_error(.manage_friendly("not_permitted")))
  }
  captain_username <- trimws(captain_username %||% "")
  if (!nzchar(captain_username)) {
    DBI::dbExecute(
      con,
      "update teams set captain_user_id = null where id = $1",
      params = list(team_id)
    )
    return(list(ok = TRUE, captain_user_id = NA_character_, error = NA_character_))
  }
  found <- db_find_user_by_username(con, captain_username)
  if (!isTRUE(found$ok)) {
    return(found)
  }
  DBI::dbExecute(
    con,
    "update teams set captain_user_id = $1 where id = $2",
    params = list(found$user_id, team_id)
  )
  .add_league_member(con, league_id, found$user_id)
  list(ok = TRUE, captain_user_id = found$user_id, error = NA_character_)
}

db_delete_team <- function(con, user_id, team_id) {
  # Owner-only; players cascade, games null-out their team refs.
  n <- DBI::dbExecute(
    con,
    "delete from teams t
      using leagues l
      where t.id = $1 and t.league_id = l.id and l.owner_id = $2",
    params = list(team_id, user_id)
  )
  if (n > 0) {
    list(ok = TRUE, error = NA_character_)
  } else {
    .manage_error(.manage_friendly("not_permitted"))
  }
}

# Idempotent membership grant (does nothing if already a member or the owner).
.add_league_member <- function(con, league_id, member_id) {
  DBI::dbExecute(
    con,
    "insert into league_members (league_id, user_id, role)
       values ($1, $2, 'member')
       on conflict (league_id, user_id) do nothing",
    params = list(league_id, member_id)
  )
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Players
# ---------------------------------------------------------------------------
db_list_players <- function(con, team_id) {
  DBI::dbGetQuery(
    con,
    "select id, name, gender, jersey_number, default_position
       from players where team_id = $1
      order by jersey_number nulls last, name",
    params = list(team_id)
  )
}

.clean_gender <- function(g) {
  g <- toupper(trimws(g %||% ""))
  if (g %in% c("M", "F")) g else NA_character_
}

.clean_int <- function(x, lo, hi) {
  if (is.null(x) || length(x) != 1 || is.na(x)) {
    return(NA_integer_)
  }
  x <- suppressWarnings(as.integer(x))
  if (is.na(x) || x < lo || x > hi) NA_integer_ else x
}

db_add_player <- function(
  con,
  user_id,
  team_id,
  name,
  gender = NA_character_,
  jersey_number = NA_integer_,
  default_position = NA_integer_
) {
  name <- trimws(name %||% "")
  if (!nzchar(name)) {
    return(.manage_error(.manage_friendly("name_required")))
  }
  if (!db_user_manages_team(con, user_id, team_id)) {
    return(.manage_error(.manage_friendly("not_permitted")))
  }
  tryCatch(
    {
      row <- DBI::dbGetQuery(
        con,
        "insert into players (team_id, name, gender, jersey_number, default_position)
           values ($1, $2, $3, $4, $5) returning id",
        params = list(
          team_id,
          name,
          .clean_gender(gender),
          .clean_int(jersey_number, 0L, 999L),
          .clean_int(default_position, 1L, 10L)
        )
      )
      list(ok = TRUE, player_id = row$id[[1]], error = NA_character_)
    },
    error = function(e) {
      if (grepl("unique|duplicate", conditionMessage(e), ignore.case = TRUE)) {
        .manage_error("That jersey number is already taken on this team.")
      } else {
        .manage_error(.manage_friendly(NULL))
      }
    }
  )
}

db_update_player <- function(
  con,
  user_id,
  player_id,
  name,
  gender = NA_character_,
  jersey_number = NA_integer_,
  default_position = NA_integer_
) {
  name <- trimws(name %||% "")
  if (!nzchar(name)) {
    return(.manage_error(.manage_friendly("name_required")))
  }
  tid <- DBI::dbGetQuery(
    con,
    "select team_id from players where id = $1",
    params = list(player_id)
  )
  if (nrow(tid) != 1 || !db_user_manages_team(con, user_id, tid$team_id[[1]])) {
    return(.manage_error(.manage_friendly("not_permitted")))
  }
  tryCatch(
    {
      DBI::dbExecute(
        con,
        "update players
            set name = $1, gender = $2, jersey_number = $3, default_position = $4
          where id = $5",
        params = list(
          name,
          .clean_gender(gender),
          .clean_int(jersey_number, 0L, 999L),
          .clean_int(default_position, 1L, 10L),
          player_id
        )
      )
      list(ok = TRUE, error = NA_character_)
    },
    error = function(e) {
      if (grepl("unique|duplicate", conditionMessage(e), ignore.case = TRUE)) {
        .manage_error("That jersey number is already taken on this team.")
      } else {
        .manage_error(.manage_friendly(NULL))
      }
    }
  )
}

db_delete_player <- function(con, user_id, player_id) {
  tid <- DBI::dbGetQuery(
    con,
    "select team_id from players where id = $1",
    params = list(player_id)
  )
  if (nrow(tid) != 1 || !db_user_manages_team(con, user_id, tid$team_id[[1]])) {
    return(.manage_error(.manage_friendly("not_permitted")))
  }
  DBI::dbExecute(con, "delete from players where id = $1", params = list(player_id))
  list(ok = TRUE, error = NA_character_)
}
