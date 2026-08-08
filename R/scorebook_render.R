# A diamond centered in a `cell`-sized box at (x, y); bases filled per rec$reached.
scorebook_cell_svg <- function(rec, x, y, cell) {
  cx <- x + cell / 2; cy <- y + cell / 2; r <- cell * 0.36
  top <- sprintf("%f,%f", cx, cy - r); right <- sprintf("%f,%f", cx + r, cy)
  bot <- sprintf("%f,%f", cx, cy + r); left <- sprintf("%f,%f", cx - r, cy)
  reached <- rec$reached %||% NA
  filled <- if (!is.na(reached) && reached == 4L) BRAND_COLORS$primary else "none"
  parts <- c(
    sprintf('<polygon points="%s %s %s %s" fill="%s" stroke="%s" stroke-width="1"/>',
            top, right, bot, left, filled, BRAND_COLORS$dark),
    sprintf('<text x="%f" y="%f" font-size="%f" text-anchor="middle" fill="%s">%s</text>',
            cx, y + cell - 4, cell * 0.18, BRAND_COLORS$dark,
            paste0(rec$outcome, if (!is.na(rec$fielding %||% NA)) paste0(" ", rec$fielding) else ""))
  )
  if (!is.na(rec$rbi %||% 0L) && rec$rbi > 0)
    parts <- c(parts, sprintf('<circle cx="%f" cy="%f" r="2.5" fill="%s"/>',
                              x + 6, y + 8, BRAND_COLORS$danger))
  paste(parts, collapse = "")
}

render_scorebook_svg <- function(state, team) {
  lineup <- state$lineups[[team]]
  batters <- Filter(function(p) !is.na(p$order_slot), lineup)
  batters <- batters[order(vapply(batters, function(p) p$order_slot, integer(1)))]
  innings <- max(1L, state$inning)
  cell <- 60; label_w <- 120; header_h <- 24
  w <- label_w + innings * cell; h <- header_h + length(batters) * cell

  rows <- character()
  for (bi in seq_along(batters)) {
    p <- batters[[bi]]; y <- header_h + (bi - 1) * cell
    rows <- c(rows, sprintf('<text x="6" y="%f" font-size="12" fill="%s">%s %s</text>',
      y + cell/2, BRAND_COLORS$dark,
      if (!is.na(p$jersey_number)) paste0("#", p$jersey_number) else "", p$name))
    for (rec in state$pa_log) {
      if (!identical(rec$team, team) || !identical(rec$batter_id, p$player_id)) next
      x <- label_w + (rec$inning - 1L) * cell
      rows <- c(rows, scorebook_cell_svg(rec, x, y, cell))
    }
  }
  grid <- sprintf('<rect x="0" y="0" width="%d" height="%d" fill="none" stroke="%s"/>',
                  w, h, BRAND_COLORS$rule_line)
  htmltools::HTML(sprintf(
    '<div class="bw-scorebook"><svg width="%d" height="%d" viewBox="0 0 %d %d" xmlns="http://www.w3.org/2000/svg">%s%s</svg></div>',
    w, h, w, h, grid, paste(rows, collapse = "")))
}
