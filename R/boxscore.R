.HIT <- c("1B","2B","3B","HR","ITPHR")
.AB_EXCLUDE <- c("BB","IBB","HBP","SF","SAC")  # not at-bats

batting_lines <- function(state, team) {
  lineup <- state$lineups[[team]]
  ids <- vapply(lineup, function(p) p$player_id, character(1))
  names_ <- vapply(lineup, function(p) p$name, character(1))
  init <- function() setNames(as.list(rep(0L, length(ids))), ids)
  AB<-init(); R<-init(); H<-init(); RBI<-init(); BB<-init(); K<-init()

  for (rec in state$pa_log) {
    if (!identical(rec$team, team)) next
    id <- rec$batter_id
    if (is.null(AB[[id]])) next
    o <- rec$outcome
    if (!o %in% .AB_EXCLUDE) AB[[id]] <- AB[[id]] + 1L
    if (o %in% .HIT) H[[id]] <- H[[id]] + 1L
    if (o %in% c("K","KL")) K[[id]] <- K[[id]] + 1L
    if (o %in% c("BB","IBB")) BB[[id]] <- BB[[id]] + 1L
    RBI[[id]] <- RBI[[id]] + as.integer(rec$rbi %||% 0L)
    if (!is.na(rec$reached) && rec$reached == 4L) R[[id]] <- R[[id]] + 1L
  }
  ord <- vapply(lineup, function(p) p$order_slot %||% NA_integer_, integer(1))
  # unlist(X[ids]) on an empty named list returns NULL, which data.frame() would
  # silently drop as a column — coerce back to integer(0) so an empty lineup
  # (run-only team) still yields all eight columns, just with zero rows.
  stat_col <- function(x) as.integer(unlist(x[ids]) %||% integer(0))
  df <- data.frame(
    Order = ord, Player = names_,
    AB = stat_col(AB), R = stat_col(R), H = stat_col(H),
    RBI = stat_col(RBI), BB = stat_col(BB), K = stat_col(K),
    row.names = NULL, stringsAsFactors = FALSE)
  # Batters in order, then anyone without a slot (defensive subs, unentered players).
  df <- df[order(is.na(df$Order), df$Order), , drop = FALSE]
  rownames(df) <- NULL
  df
}

line_score <- function(state) {
  totals <- function(team) {
    runs <- state$line_score[[team]]
    if (identical(team, state$batting_team)) runs <- c(runs, state$runs_this_half)
    h <- sum(vapply(state$pa_log, function(r)
      identical(r$team, team) && r$outcome %in% .HIT, logical(1)))
    list(runs = as.integer(runs), R = sum(as.integer(runs)), H = as.integer(h), E = 0L)
  }
  list(home = totals("home"), away = totals("away"))
}
