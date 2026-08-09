make_storage <- function(
  backend = c("guest", "supabase"),
  con = NULL,
  user_id = NULL
) {
  backend <- match.arg(backend)
  if (backend == "guest") {
    return(.guest_storage())
  }
  .supabase_storage(con = con, user_id = user_id)
}

.guest_storage <- function() {
  env <- new.env(parent = emptyenv())
  env$games <- list()
  env$events <- list()
  list(
    create_game = function(meta = list()) {
      gid <- uuid::UUIDgenerate()
      env$games[[gid]] <- list(
        game_id = gid,
        name = meta$name %||% "Untitled",
        status = "in_progress",
        updated_at = NA_character_
      )
      env$events[[gid]] <- list()
      gid
    },
    append_event = function(game_id, evt) {
      cur <- env$events[[game_id]] %||% list()
      evt$seq <- length(cur) + 1L
      env$events[[game_id]] <- c(cur, list(evt))
      evt
    },
    load_events = function(game_id) env$events[[game_id]] %||% list(),
    save_snapshot = function(game_id, state) invisible(NULL),
    delete_game = function(game_id, user_id = NULL) {
      env$games[[game_id]] <- NULL
      env$events[[game_id]] <- NULL
      invisible(TRUE)
    },
    list_games = function() {
      if (length(env$games) == 0) {
        return(data.frame(
          game_id = character(),
          name = character(),
          status = character(),
          updated_at = character(),
          relationship = character(),
          home_team = character(),
          away_team = character(),
          can_delete = logical()
        ))
      }
      do.call(
        rbind,
        lapply(env$games, function(g) {
          g$relationship <- "owned"
          g$home_team <- g$home_team %||% NA_character_
          g$away_team <- g$away_team %||% NA_character_
          # Guests own everything in their in-memory session.
          g$can_delete <- TRUE
          as.data.frame(g, stringsAsFactors = FALSE)
        })
      )
    }
  )
}

.supabase_storage <- function(con, user_id) {
  stopifnot(!is.null(con), !is.null(user_id))
  list(
    create_game = function(meta = list()) {
      gid <- uuid::UUIDgenerate()
      DBI::dbExecute(
        con,
        "insert into games (id, owner_id, played_on, location, status) values ($1,$2,$3,$4,'in_progress')",
        params = list(
          gid,
          user_id,
          meta$played_on %||% NA,
          meta$location %||% NA
        )
      )
      gid
    },
    append_event = function(game_id, evt) {
      nxt <- DBI::dbGetQuery(
        con,
        "select coalesce(max(seq),0)+1 as n from game_events where game_id=$1",
        params = list(game_id)
      )$n[[1]]
      evt$seq <- as.integer(nxt)
      DBI::dbExecute(
        con,
        "insert into game_events (game_id, seq, type, payload) values ($1,$2,$3,$4)",
        params = list(
          game_id,
          evt$seq,
          evt$type,
          jsonlite::toJSON(
            evt$payload,
            auto_unbox = TRUE,
            null = "null",
            na = "null"
          )
        )
      )
      DBI::dbExecute(
        con,
        "update games set updated_at=now() where id=$1",
        params = list(game_id)
      )
      evt
    },
    load_events = function(game_id) {
      df <- DBI::dbGetQuery(
        con,
        "select seq, type, payload from game_events where game_id=$1 order by seq",
        params = list(game_id)
      )
      lapply(seq_len(nrow(df)), function(i) {
        new_event(
          df$type[i],
          jsonlite::fromJSON(df$payload[i], simplifyVector = FALSE),
          seq = as.integer(df$seq[i])
        )
      })
    },
    save_snapshot = function(game_id, state) {
      DBI::dbExecute(
        con,
        "update games set state_snapshot=$1, status=$2, updated_at=now() where id=$3",
        params = list(
          jsonlite::toJSON(
            state,
            auto_unbox = TRUE,
            null = "null",
            na = "null"
          ),
          state$status %||% "in_progress",
          game_id
        )
      )
      invisible(NULL)
    },
    delete_game = function(game_id, delete_user_id = user_id) {
      # Ownership rule: a user may delete a game they own directly, and a
      # league owner may delete games assigned to a league they own. Enforced
      # in the WHERE clause so a non-owner's request affects zero rows. Event
      # rows cascade-delete via the games FK.
      n <- DBI::dbExecute(
        con,
        "delete from games g
          where g.id = $1
            and (g.owner_id = $2
                 or exists (select 1 from leagues l
                             where l.id = g.league_id
                               and l.owner_id = $2))",
        params = list(game_id, delete_user_id)
      )
      invisible(n > 0)
    },
    list_games = function() {
      # Surface every game the user may view:
      #   * games they own directly
      #   * games in a league they own or are a member of
      #   * games shared with them directly (game_shares)
      # `relationship` lets the UI distinguish these; ordered owned > league >
      # shared so a game the user owns is labeled as owned even if it also
      # matches via league or share. `can_delete` is true when the user owns the
      # game or owns its league.
      DBI::dbGetQuery(
        con,
        "select g.id as game_id,
                coalesce(g.location, 'Game') as name,
                g.status,
                g.updated_at::text as updated_at,
                ht.name as home_team,
                at.name as away_team,
                case
                  when g.owner_id = $1 then 'owned'
                  when l.id is not null then 'league'
                  else 'shared'
                end as relationship,
                (g.owner_id = $1 or l.owner_id = $1) as can_delete
           from games g
           left join leagues l
             on l.id = g.league_id
            and (l.owner_id = $1
                 or exists (select 1 from league_members m
                             where m.league_id = l.id and m.user_id = $1))
           left join teams ht on ht.id = g.home_team_id
           left join teams at on at.id = g.away_team_id
           left join game_shares s
             on s.game_id = g.id and s.user_id = $1
          where g.owner_id = $1
             or l.id is not null
             or s.game_id is not null
          order by g.updated_at desc",
        params = list(user_id)
      )
    }
  )
}
