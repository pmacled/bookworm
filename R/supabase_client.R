supabase_configured <- function() {
  vars <- c("SUPABASE_DB_HOST","SUPABASE_DB_PORT","SUPABASE_DB_NAME",
            "SUPABASE_DB_USER","SUPABASE_DB_PASSWORD","SUPABASE_URL","SUPABASE_ANON_KEY")
  all(nzchar(Sys.getenv(vars)))
}

supabase_connect <- function() {
  if (!requireNamespace("RPostgres", quietly = TRUE))
    stop("RPostgres is required for Supabase persistence.")
  DBI::dbConnect(RPostgres::Postgres(),
    host = Sys.getenv("SUPABASE_DB_HOST"),
    port = as.integer(Sys.getenv("SUPABASE_DB_PORT")),
    dbname = Sys.getenv("SUPABASE_DB_NAME"),
    user = Sys.getenv("SUPABASE_DB_USER"),
    password = Sys.getenv("SUPABASE_DB_PASSWORD"),
    sslmode = "require")
}

.auth_error <- function(msg)
  list(ok = FALSE, user_id = NA_character_, access_token = NA_character_,
       error = friendly_auth_error(msg))

# GoTrue's raw messages are terse and sometimes cryptic. Map the ones we know;
# pass anything else through so a real backend message is never swallowed.
friendly_auth_error <- function(msg) {
  if (is.null(msg) || length(msg) != 1 || is.na(msg) || !nzchar(msg))
    return("Sign-in failed. Please try again.")
  known <- c(
    "Invalid login credentials" = "That email or password is not correct.",
    "Email not confirmed"       = "Check your inbox and confirm your email address first.",
    "User already registered"   = "An account with that email already exists — try signing in.",
    "Password should be at least 6 characters" =
      "Passwords must be at least 6 characters long.")
  if (msg %in% names(known)) return(unname(known[[msg]]))
  msg
}

.gotrue_parse <- function(body) {
  if (!is.null(body$access_token) && !is.null(body$user)) {
    return(list(ok = TRUE, user_id = body$user$id,
                access_token = body$access_token, error = NA_character_))
  }
  msg <- body$error_description %||% body$msg %||% body$error %||% "authentication failed"
  .auth_error(msg)
}

.gotrue_request <- function(path, email, password) {
  base <- Sys.getenv("SUPABASE_URL")
  if (!nzchar(base))
    return(.auth_error("Saving is not configured on this deployment."))
  # req_error(is_error = FALSE) suppresses HTTP *status* errors but not transport
  # errors (DNS, refused connection, TLS), which is why the whole call is wrapped.
  tryCatch({
    resp <- httr2::request(paste0(base, "/auth/v1/", path)) |>
      httr2::req_headers(apikey = Sys.getenv("SUPABASE_ANON_KEY"),
                         "Content-Type" = "application/json") |>
      httr2::req_body_json(list(email = email, password = password)) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    .gotrue_parse(httr2::resp_body_json(resp))
  }, error = function(e) .auth_error("Could not reach the sign-in service."))
}

gotrue_sign_in <- function(email, password) .gotrue_request("token?grant_type=password", email, password)
gotrue_sign_up <- function(email, password) .gotrue_request("signup", email, password)
