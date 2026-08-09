# Reconstructs each batter's full trip around the bases from the pa_log.
#
# The bug this fixes: the old renderer read pa_log[[i]]$reached, which is frozen at the
# moment of the plate appearance. A runner who singles and later comes around to score
# never updated their own cell, so only home runs ever showed a run. Every advance now
# carries origin_index — the pa_log entry that put that runner on base — so a later
# advance can be credited back to the cell it belongs to.
#
# Pure data. No Shiny, no colours, no SVG.

.own_advance <- function(rec) {
  Filter(
    function(a) identical(as.integer(a$from %||% -1L), 0L),
    rec$advances %||% list()
  )
}

.seed_from_own <- function(rec) {
  own <- .own_advance(rec)
  if (length(own)) {
    a <- own[[1]]
    if (isTRUE(a$scored)) {
      return(4L)
    }
    if (isTRUE(a$out)) {
      return(0L)
    }
    return(as.integer(a$to))
  }
  # Legacy path: slice 1.1 events have no batter advance, only `reached`.
  r <- rec$reached %||% NA_integer_
  if (is.null(r) || length(r) != 1 || is.na(r)) 0L else as.integer(r)
}

runner_paths <- function(state) {
  pal <- state$pa_log %||% list()
  n <- length(pal)
  if (n == 0L) {
    return(list())
  }

  recs <- vector("list", n)
  for (i in seq_len(n)) {
    rec <- pal[[i]]
    seed <- .seed_from_own(rec)
    recs[[i]] <- list(
      pa_index = i,
      team = rec$team,
      inning = as.integer(rec$inning),
      half = rec$half,
      pa_index_in_half = as.integer(rec$pa_index_in_half %||% NA_integer_),
      batter_id = rec$batter_id,
      outcome = rec$outcome,
      fielding = rec$fielding %||% NA_character_,
      rbi = as.integer(rec$rbi %||% 0L),
      bases_reached = seed,
      scored = seed >= 4L,
      out_at = NA_integer_,
      out_number = NA_integer_
    )
  }

  for (i in seq_len(n)) {
    rec <- pal[[i]]
    # outs_before carries the half's running total, so out numbering is local to the play.
    k <- as.integer(rec$outs_before %||% 0L)
    advances <- rec$advances %||% list()

    for (a in advances) {
      oi <- a$origin_index %||% NA_integer_
      if (is.null(oi) || length(oi) != 1 || is.na(oi) || oi < 1L || oi > n) {
        next
      }
      oi <- as.integer(oi)
      if (isTRUE(a$scored)) {
        recs[[oi]]$bases_reached <- 4L
        recs[[oi]]$scored <- TRUE
      } else if (isTRUE(a$out)) {
        k <- k + 1L
        recs[[oi]]$out_at <- as.integer(a$from)
        recs[[oi]]$out_number <- k
        recs[[oi]]$bases_reached <- max(
          recs[[oi]]$bases_reached,
          as.integer(a$from)
        )
      } else {
        recs[[oi]]$bases_reached <- max(
          recs[[oi]]$bases_reached,
          as.integer(a$to)
        )
      }
    }

    # Legacy events record outs_on_play without an out advance for the batter.
    if (
      !length(Filter(function(a) isTRUE(a$out), advances)) &&
        as.integer(rec$outs_on_play %||% 0L) > 0L &&
        is.na(recs[[i]]$out_number)
    ) {
      recs[[i]]$out_number <- k + 1L
      recs[[i]]$out_at <- 0L
    }
  }
  recs
}

# Column layout for one team's scorebook. An inning gets one sub-column per time
# through the order, so batting around no longer draws cells on top of each other.
scorebook_layout <- function(paths, team) {
  cells <- Filter(function(p) identical(p$team, team), paths)
  if (!length(cells)) {
    return(list(innings = 0L, sub_counts = integer(0), cells = list()))
  }

  seen <- list() # "<inning>|<batter_id>" -> count so far
  for (i in seq_along(cells)) {
    key <- paste0(cells[[i]]$inning, "|", cells[[i]]$batter_id)
    seen[[key]] <- (seen[[key]] %||% 0L) + 1L
    cells[[i]]$sub_index <- seen[[key]]
  }

  innings <- max(vapply(cells, function(c) c$inning, integer(1)))
  sub_counts <- stats::setNames(
    rep(1L, innings),
    as.character(seq_len(innings))
  )
  for (c in cells) {
    k <- as.character(c$inning)
    sub_counts[[k]] <- max(sub_counts[[k]], c$sub_index)
  }
  # Column offset: every preceding inning's sub-columns, plus this cell's own index.
  offsets <- c(0L, cumsum(unname(sub_counts)))
  for (i in seq_along(cells)) {
    cells[[i]]$col <- offsets[[cells[[i]]$inning]] + cells[[i]]$sub_index - 1L
  }

  list(innings = as.integer(innings), sub_counts = sub_counts, cells = cells)
}
