.HIT <- c("1B", "2B", "3B", "HR", "ITPHR")
.AB_EXCLUDE <- c("BB", "IBB", "HBP", "SF", "SAC") # not at-bats

batting_lines <- function(state, team) {
  lineup <- state$lineups[[team]]
  names_ <- vapply(lineup, function(p) p$name, character(1))
  a <- .accumulate_batting(state, team)
  ids <- a$ids
  acc <- a$acc
  ord <- vapply(lineup, function(p) p$order_slot %||% NA_integer_, integer(1))
  # unlist(X[ids]) on an empty named list returns NULL, which data.frame() would
  # silently drop as a column — coerce back to integer(0) so an empty lineup
  # (run-only team) still yields all eight columns, just with zero rows.
  stat_col <- function(x) as.integer(unlist(x[ids]) %||% integer(0))
  df <- data.frame(
    Order = ord,
    Player = names_,
    AB = stat_col(acc$AB),
    R = stat_col(acc$R),
    H = stat_col(acc$H),
    RBI = stat_col(acc$RBI),
    BB = stat_col(acc$BB),
    K = stat_col(acc$K),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  # Batters in order, then anyone without a slot (defensive subs, unentered players).
  df <- df[order(is.na(df$Order), df$Order), , drop = FALSE]
  rownames(df) <- NULL
  df
}

# Per-player counting stats for one team, keyed by player_id. The card and the box
# score share this so they can never disagree about what counts as an at-bat.
.accumulate_batting <- function(state, team) {
  ids <- vapply(state$lineups[[team]], function(p) p$player_id, character(1))
  init <- function() setNames(as.list(rep(0L, length(ids))), ids)
  acc <- list(
    AB = init(),
    R = init(),
    H = init(),
    RBI = init(),
    BB = init(),
    K = init()
  )

  for (rec in state$pa_log) {
    if (!identical(rec$team, team)) {
      next
    }
    id <- rec$batter_id
    if (is.null(acc$AB[[id]])) {
      next
    }
    o <- rec$outcome
    if (!o %in% .AB_EXCLUDE) {
      acc$AB[[id]] <- acc$AB[[id]] + 1L
    }
    if (o %in% .HIT) {
      acc$H[[id]] <- acc$H[[id]] + 1L
    }
    if (o %in% c("K", "KL")) {
      acc$K[[id]] <- acc$K[[id]] + 1L
    }
    if (o %in% c("BB", "IBB")) {
      acc$BB[[id]] <- acc$BB[[id]] + 1L
    }
    acc$RBI[[id]] <- acc$RBI[[id]] + as.integer(rec$rbi %||% 0L)
    r <- rec$reached %||% NA_integer_
    if (!is.na(r) && r == 4L) acc$R[[id]] <- acc$R[[id]] + 1L
  }
  list(ids = ids, acc = acc)
}

batting_line_for <- function(state, team, player_id) {
  a <- .accumulate_batting(state, team)
  if (!player_id %in% a$ids) {
    return(NULL)
  }
  stats::setNames(
    lapply(names(a$acc), function(k) a$acc[[k]][[player_id]]),
    names(a$acc)
  )
}

line_score <- function(state) {
  totals <- function(team) {
    runs <- state$line_score[[team]]
    if (identical(team, state$batting_team)) {
      runs <- c(runs, state$runs_this_half)
    }
    h <- sum(vapply(
      state$pa_log,
      function(r) {
        identical(r$team, team) && r$outcome %in% .HIT
      },
      logical(1)
    ))
    list(
      runs = as.integer(runs),
      R = sum(as.integer(runs)),
      H = as.integer(h),
      E = 0L
    )
  }
  list(home = totals("home"), away = totals("away"))
}
