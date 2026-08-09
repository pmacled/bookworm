# One scorebook cell: a diamond whose edges darken as the runner advances, filled when
# they score. Takes a record from runner_paths(), not a raw pa_log entry — the whole
# point is that a cell reflects the runner's *cumulative* trip, updated by later plays.

# Vertices, clockwise from home. Index 1..4 == first, second, third, home.
.diamond_vertices <- function(cx, cy, r) {
  list(
    home = c(cx, cy + r),
    first = c(cx + r, cy),
    second = c(cx, cy - r),
    third = c(cx - r, cy)
  )
}

.leg_endpoints <- function(v) {
  list(
    list(v$home, v$first), # leg 1: home to first
    list(v$first, v$second), # leg 2
    list(v$second, v$third), # leg 3
    list(v$third, v$home)
  )
} # leg 4: third to home

.base_point <- function(v, base) {
  switch(
    as.character(base),
    "1" = v$first,
    "2" = v$second,
    "3" = v$third,
    "4" = v$home,
    NULL
  )
}

scorebook_cell_svg <- function(path, x, y, cell) {
  cx <- x + cell / 2
  cy <- y + cell * 0.42 # leaves room for the outcome text beneath
  r <- cell * 0.28
  v <- .diamond_vertices(cx, cy, r)
  reached <- as.integer(path$bases_reached %||% 0L)
  parts <- character()

  # Filled interior means the runner scored. Drawn first so the legs sit on top.
  if (isTRUE(path$scored)) {
    pts <- paste(
      vapply(
        list(v$home, v$first, v$second, v$third),
        function(p) sprintf("%.1f,%.1f", p[1], p[2]),
        character(1)
      ),
      collapse = " "
    )
    parts <- c(
      parts,
      sprintf(
        '<polygon points="%s" fill="%s" stroke="none"/>',
        pts,
        BRAND_COLORS$primary_light
      )
    )
  }

  # One line per leg: heavy for legs the runner completed, light for the rest.
  legs <- .leg_endpoints(v)
  for (j in seq_along(legs)) {
    done <- reached >= j
    parts <- c(
      parts,
      sprintf(
        '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="%s" stroke-linecap="round" %s/>',
        legs[[j]][[1]][1],
        legs[[j]][[1]][2],
        legs[[j]][[2]][1],
        legs[[j]][[2]][2],
        if (done) BRAND_COLORS$dark else BRAND_COLORS$secondary,
        if (done) "3" else "1",
        if (done) "" else 'opacity="0.35"'
      )
    )
  }

  # Out number, circled, top right.
  if (!is.na(path$out_number %||% NA)) {
    ox <- x + cell - 10
    oy <- y + 11
    parts <- c(
      parts,
      sprintf(
        '<circle cx="%.1f" cy="%.1f" r="7" fill="none" stroke="%s" stroke-width="1"/>',
        ox,
        oy,
        BRAND_COLORS$dark
      ),
      sprintf(
        '<text x="%.1f" y="%.1f" font-size="9" text-anchor="middle" fill="%s">%d</text>',
        ox,
        oy + 3.2,
        BRAND_COLORS$dark,
        as.integer(path$out_number)
      )
    )
  }

  # RBI dots, top left, capped at four glyphs.
  rbi <- min(4L, as.integer(path$rbi %||% 0L))
  for (i in seq_len(rbi)) {
    parts <- c(
      parts,
      sprintf(
        '<circle class="bw-rbi" cx="%.1f" cy="%.1f" r="2.2" fill="%s"/>',
        x + 6 + (i - 1) * 5.5,
        y + 8,
        BRAND_COLORS$danger
      )
    )
  }

  # A cross at the base where the trip ended, when it ended on the basepaths.
  out_at <- path$out_at %||% NA_integer_
  if (!is.na(out_at) && out_at >= 1L) {
    p <- .base_point(v, out_at)
    parts <- c(
      parts,
      sprintf(
        '<text class="bw-out-mark" x="%.1f" y="%.1f" font-size="10" text-anchor="middle" fill="%s">&#215;</text>',
        p[1],
        p[2] + 3.5,
        BRAND_COLORS$danger
      )
    )
  }

  # Outcome plus fielding notation, centred beneath the diamond.
  label <- paste0(
    path$outcome,
    if (!is.na(path$fielding %||% NA)) paste0(" ", path$fielding) else ""
  )
  parts <- c(
    parts,
    sprintf(
      '<text x="%.1f" y="%.1f" font-size="%.1f" text-anchor="middle" fill="%s">%s</text>',
      cx,
      y + cell - 5,
      cell * 0.17,
      BRAND_COLORS$dark,
      htmltools::htmlEscape(label)
    )
  )

  paste(parts, collapse = "")
}

render_scorebook_svg <- function(state, team, current_batter_id = NULL) {
  lineup <- state$lineups[[team]]
  batters <- Filter(function(p) !is.na(p$order_slot), lineup)
  batters <- batters[order(vapply(
    batters,
    function(p) p$order_slot,
    integer(1)
  ))]

  lay <- scorebook_layout(runner_paths(state), team)
  # Show every inning reached, even one with no plate appearances yet.
  innings <- max(1L, lay$innings, as.integer(state$inning %||% 1L))
  sub_counts <- stats::setNames(
    rep(1L, innings),
    as.character(seq_len(innings))
  )
  for (k in names(lay$sub_counts)) {
    sub_counts[[k]] <- lay$sub_counts[[k]]
  }
  total_cols <- sum(sub_counts)

  cell <- 64
  label_w <- 130
  header_h <- 26
  w <- label_w + total_cols * cell
  h <- header_h + max(1L, length(batters)) * cell
  offsets <- c(0L, cumsum(unname(sub_counts)))

  parts <- character()

  # Faint rules so the grid reads as a scorebook page.
  for (i in seq_len(innings)) {
    gx <- label_w + offsets[[i]] * cell
    parts <- c(
      parts,
      sprintf(
        '<line x1="%d" y1="0" x2="%d" y2="%d" stroke="%s" stroke-width="1"/>',
        gx,
        gx,
        h,
        BRAND_COLORS$rule_line
      )
    )
    # Inning header, centred over its sub-columns.
    parts <- c(
      parts,
      sprintf(
        '<text x="%.1f" y="17" font-size="11" text-anchor="middle" fill="%s">%d</text>',
        gx + sub_counts[[i]] * cell / 2,
        BRAND_COLORS$secondary,
        i
      )
    )
  }
  for (bi in seq_len(max(1L, length(batters)))) {
    ry <- header_h + bi * cell
    parts <- c(
      parts,
      sprintf(
        '<line x1="0" y1="%d" x2="%d" y2="%d" stroke="%s" stroke-width="1"/>',
        ry,
        w,
        ry,
        BRAND_COLORS$rule_line
      )
    )
  }
  parts <- c(
    parts,
    sprintf(
      '<line x1="0" y1="%d" x2="%d" y2="%d" stroke="%s" stroke-width="1"/>',
      header_h,
      w,
      header_h,
      BRAND_COLORS$rule_line
    )
  )

  for (bi in seq_along(batters)) {
    p <- batters[[bi]]
    y <- header_h + (bi - 1) * cell
    is_current <- !is.null(current_batter_id) &&
      identical(p$player_id, current_batter_id)
    label <- paste0(
      if (!is.na(p$jersey_number)) paste0("#", p$jersey_number, " ") else "",
      p$name
    )
    if (is_current) {
      parts <- c(
        parts,
        sprintf(
          '<rect x="0" y="%.1f" width="%d" height="%d" fill="%s" opacity="0.5"/>',
          y,
          w,
          cell,
          BRAND_COLORS$primary_light
        )
      )
    }
    parts <- c(
      parts,
      sprintf(
        '<text %s x="6" y="%.1f" font-size="12" fill="%s" font-weight="%s">%s</text>',
        if (is_current) 'class="bw-current"' else "",
        y + cell / 2,
        BRAND_COLORS$dark,
        if (is_current) "700" else "400",
        htmltools::htmlEscape(label)
      )
    )

    for (c in lay$cells) {
      if (!identical(c$batter_id, p$player_id)) {
        next
      }
      parts <- c(parts, scorebook_cell_svg(c, label_w + c$col * cell, y, cell))
    }
  }

  htmltools::HTML(sprintf(
    '<div class="bw-scorebook"><svg width="%d" height="%d" viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg">%s</svg></div>',
    w,
    h,
    w,
    h,
    paste(parts, collapse = "")
  ))
}
