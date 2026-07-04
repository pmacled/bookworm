make_storage <- function(backend = c("guest","supabase"), con = NULL, user_id = NULL) {
  backend <- match.arg(backend)
  if (backend == "guest") return(.guest_storage())
  .supabase_storage(con = con, user_id = user_id)
}

.guest_storage <- function() {
  env <- new.env(parent = emptyenv())
  env$games <- list(); env$events <- list()
  list(
    create_game = function(meta = list()) {
      gid <- uuid::UUIDgenerate()
      env$games[[gid]] <- list(game_id = gid, name = meta$name %||% "Untitled",
        status = "in_progress", updated_at = NA_character_)
      env$events[[gid]] <- list(); gid
    },
    append_event = function(game_id, evt) {
      cur <- env$events[[game_id]] %||% list()
      evt$seq <- length(cur) + 1L
      env$events[[game_id]] <- c(cur, list(evt)); evt
    },
    load_events = function(game_id) env$events[[game_id]] %||% list(),
    save_snapshot = function(game_id, state) invisible(NULL),
    list_games = function() {
      if (length(env$games) == 0)
        return(data.frame(game_id=character(), name=character(),
                          status=character(), updated_at=character()))
      do.call(rbind, lapply(env$games, function(g) as.data.frame(g, stringsAsFactors = FALSE)))
    }
  )
}

.supabase_storage <- function(con, user_id) {
  stopifnot(!is.null(con), !is.null(user_id))
  list(
    create_game = function(meta = list()) {
      gid <- uuid::UUIDgenerate()
      DBI::dbExecute(con,
        "insert into games (id, owner_id, played_on, location, status) values ($1,$2,$3,$4,'in_progress')",
        params = list(gid, user_id, meta$played_on %||% NA, meta$location %||% NA))
      gid
    },
    append_event = function(game_id, evt) {
      nxt <- DBI::dbGetQuery(con,
        "select coalesce(max(seq),0)+1 as n from game_events where game_id=$1",
        params = list(game_id))$n[[1]]
      evt$seq <- as.integer(nxt)
      DBI::dbExecute(con,
        "insert into game_events (game_id, seq, type, payload) values ($1,$2,$3,$4)",
        params = list(game_id, evt$seq, evt$type,
                      jsonlite::toJSON(evt$payload, auto_unbox = TRUE, null="null", na="null")))
      DBI::dbExecute(con, "update games set updated_at=now() where id=$1", params=list(game_id))
      evt
    },
    load_events = function(game_id) {
      df <- DBI::dbGetQuery(con,
        "select seq, type, payload from game_events where game_id=$1 order by seq",
        params = list(game_id))
      lapply(seq_len(nrow(df)), function(i)
        new_event(df$type[i], jsonlite::fromJSON(df$payload[i], simplifyVector = FALSE),
                  seq = as.integer(df$seq[i])))
    },
    save_snapshot = function(game_id, state) {
      DBI::dbExecute(con, "update games set state_snapshot=$1, status=$2, updated_at=now() where id=$3",
        params = list(jsonlite::toJSON(state, auto_unbox=TRUE, null="null", na="null"),
                      state$status %||% "in_progress", game_id))
      invisible(NULL)
    },
    list_games = function() {
      DBI::dbGetQuery(con,
        "select id as game_id, coalesce(location,'Game') as name, status, updated_at::text
           from games where owner_id=$1 order by updated_at desc", params = list(user_id))
    }
  )
}
