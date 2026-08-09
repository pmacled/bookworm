supabase_configured <- function() {
  vars <- c(
    "SUPABASE_DB_HOST",
    "SUPABASE_DB_PORT",
    "SUPABASE_DB_NAME",
    "SUPABASE_DB_USER",
    "SUPABASE_DB_PASSWORD"
  )
  all(nzchar(Sys.getenv(vars)))
}

supabase_connect <- function() {
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("RPostgres is required for Supabase persistence.")
  }
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("SUPABASE_DB_HOST"),
    port = as.integer(Sys.getenv("SUPABASE_DB_PORT")),
    dbname = Sys.getenv("SUPABASE_DB_NAME"),
    user = Sys.getenv("SUPABASE_DB_USER"),
    password = Sys.getenv("SUPABASE_DB_PASSWORD"),
    sslmode = "require"
  )
}

.auth_error <- function(msg) {
  list(
    ok = FALSE,
    user_id = NA_character_,
    is_admin = FALSE,
    error = friendly_auth_error(msg)
  )
}

# Map the internal messages we raise to friendly copy; pass anything else
# through so a real backend message is never swallowed.
friendly_auth_error <- function(msg) {
  if (is.null(msg) || length(msg) != 1 || is.na(msg) || !nzchar(msg)) {
    return("Sign-in failed. Please try again.")
  }
  known <- c(
    "invalid_credentials" = "That username or password is not correct.",
    "username_taken" = "That username is already taken — try signing in.",
    "username_invalid" = "Usernames must be 3-30 characters: letters, numbers, or underscore.",
    "password_too_short" = "Passwords must be at least 6 characters long."
  )
  if (msg %in% names(known)) {
    return(unname(known[[msg]]))
  }
  msg
}

.valid_username <- function(username) {
  is.character(username) &&
    length(username) == 1 &&
    !is.na(username) &&
    grepl("^[A-Za-z0-9_]{3,30}$", username)
}

# Password verification and hashing happen in Postgres via pgcrypto's crypt(),
# so raw passwords are never compared or stored in R. `connect` is injectable
# so tests can supply a stub connection instead of a live database.
db_sign_in <- function(username, password, connect = supabase_connect) {
  if (!.valid_username(username)) {
    return(.auth_error("invalid_credentials"))
  }
  tryCatch(
    {
      con <- connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      row <- DBI::dbGetQuery(
        con,
        "select id, is_admin from users
           where username = $1 and password_hash = crypt($2, password_hash)",
        params = list(username, password)
      )
      if (nrow(row) != 1) {
        return(.auth_error("invalid_credentials"))
      }
      list(
        ok = TRUE,
        user_id = row$id[[1]],
        is_admin = isTRUE(row$is_admin[[1]]),
        error = NA_character_
      )
    },
    error = function(e) .auth_error("Could not reach the sign-in service.")
  )
}

db_sign_up <- function(username, password, connect = supabase_connect) {
  if (!.valid_username(username)) {
    return(.auth_error("username_invalid"))
  }
  if (
    !is.character(password) ||
      length(password) != 1 ||
      is.na(password) ||
      nchar(password) < 6
  ) {
    return(.auth_error("password_too_short"))
  }
  tryCatch(
    {
      con <- connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      taken <- DBI::dbGetQuery(
        con,
        "select 1 from users where username = $1",
        params = list(username)
      )
      if (nrow(taken) > 0) {
        return(.auth_error("username_taken"))
      }
      row <- DBI::dbGetQuery(
        con,
        "insert into users (username, password_hash)
           values ($1, crypt($2, gen_salt('bf')))
           returning id, is_admin",
        params = list(username, password)
      )
      list(
        ok = TRUE,
        user_id = row$id[[1]],
        is_admin = isTRUE(row$is_admin[[1]]),
        error = NA_character_
      )
    },
    error = function(e) .auth_error("Could not reach the sign-in service.")
  )
}

# ---------------------------------------------------------------------------
# Remember-me tokens (persistent login)
# ---------------------------------------------------------------------------
# The browser holds a random token; the database holds only sha256(token). We
# never store or transmit the password for auto-login, and a DB leak alone can't
# be replayed because only the hash is stored.

.token_hash <- function(token) {
  as.character(openssl::sha256(charToRaw(token)))
}

# Generate a 256-bit random token, persist its hash + expiry, and return the raw
# token (shown only to the browser). Returns NULL on failure so a sign-in is
# never blocked by remember-me bookkeeping.
db_issue_remember_token <- function(
  user_id,
  connect = supabase_connect,
  days = 30L
) {
  tryCatch(
    {
      token <- paste(openssl::rand_bytes(32), collapse = "")
      con <- connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbExecute(
        con,
        "insert into auth_tokens (token_hash, user_id, expires_at)
           values ($1, $2, now() + ($3 || ' days')::interval)",
        params = list(
          .token_hash(token),
          user_id,
          as.character(as.integer(days))
        )
      )
      token
    },
    error = function(e) NULL
  )
}

# Validate a presented token against the stored hash. Requires a live,
# non-expired row whose user_id and username still match. On success updates
# last_used_at and returns an identity result; otherwise ok = FALSE.
db_validate_remember_token <- function(
  user_id,
  username,
  token,
  connect = supabase_connect
) {
  if (
    !is.character(token) ||
      length(token) != 1 ||
      is.na(token) ||
      !nzchar(token)
  ) {
    return(list(ok = FALSE, user_id = NA_character_, is_admin = FALSE))
  }
  tryCatch(
    {
      con <- connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      row <- DBI::dbGetQuery(
        con,
        "select u.id, u.is_admin
           from auth_tokens t
           join users u on u.id = t.user_id
          where t.token_hash = $1
            and t.user_id = $2
            and u.username = $3
            and t.expires_at > now()",
        params = list(.token_hash(token), user_id, username)
      )
      if (nrow(row) != 1) {
        return(list(ok = FALSE, user_id = NA_character_, is_admin = FALSE))
      }
      DBI::dbExecute(
        con,
        "update auth_tokens set last_used_at = now() where token_hash = $1",
        params = list(.token_hash(token))
      )
      list(
        ok = TRUE,
        user_id = row$id[[1]],
        is_admin = isTRUE(row$is_admin[[1]])
      )
    },
    error = function(e) {
      list(ok = FALSE, user_id = NA_character_, is_admin = FALSE)
    }
  )
}

# Delete a single token (sign-out). Best-effort.
db_revoke_remember_token <- function(token, connect = supabase_connect) {
  if (
    !is.character(token) ||
      length(token) != 1 ||
      is.na(token) ||
      !nzchar(token)
  ) {
    return(invisible(FALSE))
  }
  tryCatch(
    {
      con <- connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbExecute(
        con,
        "delete from auth_tokens where token_hash = $1",
        params = list(.token_hash(token))
      )
      invisible(TRUE)
    },
    error = function(e) invisible(FALSE)
  )
}

# Sweep expired tokens. Best-effort; failure never blocks a login attempt.
db_purge_expired_tokens <- function(connect = supabase_connect) {
  tryCatch(
    {
      con <- connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      DBI::dbExecute(con, "delete from auth_tokens where expires_at < now()")
      invisible(TRUE)
    },
    error = function(e) invisible(FALSE)
  )
}
