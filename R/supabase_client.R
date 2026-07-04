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

.gotrue_parse <- function(body) {
  if (!is.null(body$access_token) && !is.null(body$user)) {
    return(list(ok = TRUE, user_id = body$user$id,
                access_token = body$access_token, error = NA_character_))
  }
  msg <- body$error_description %||% body$msg %||% body$error %||% "authentication failed"
  list(ok = FALSE, user_id = NA_character_, access_token = NA_character_, error = msg)
}

.gotrue_request <- function(path, email, password) {
  url <- paste0(Sys.getenv("SUPABASE_URL"), "/auth/v1/", path)
  resp <- httr2::request(url) |>
    httr2::req_headers(apikey = Sys.getenv("SUPABASE_ANON_KEY"),
                       "Content-Type" = "application/json") |>
    httr2::req_body_json(list(email = email, password = password)) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  .gotrue_parse(httr2::resp_body_json(resp))
}

gotrue_sign_in <- function(email, password) .gotrue_request("token?grant_type=password", email, password)
gotrue_sign_up <- function(email, password) .gotrue_request("signup", email, password)
