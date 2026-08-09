# The one-glance game situation: score, inning, outs, count, batter, batter's line.

batter_line_text <- function(line) {
  if (is.null(line)) {
    return("")
  }
  parts <- sprintf("%d-for-%d", line$H, line$AB)
  extras <- character()
  if (line$RBI > 0L) {
    extras <- c(
      extras,
      if (line$RBI == 1L) "RBI" else sprintf("%d RBI", line$RBI)
    )
  }
  if (line$BB > 0L) {
    extras <- c(extras, if (line$BB == 1L) "BB" else sprintf("%d BB", line$BB))
  }
  if (line$K > 0L) {
    extras <- c(extras, if (line$K == 1L) "K" else sprintf("%d K", line$K))
  }
  if (line$AB == 0L && !length(extras)) {
    return("first at-bat")
  }
  paste(c(parts, extras), collapse = ", ")
}

.score_block <- function(name, runs, batting) {
  tags$div(
    class = paste("bw-score-side", if (batting) "bw-batting" else ""),
    tags$div(class = "bw-team", name),
    tags$div(class = "bw-runs", runs)
  )
}

situation_card_ui <- function(state) {
  final <- identical(state$status, "final")
  away_name <- state$teams$away$name %||% "Away"
  home_name <- state$teams$home$name %||% "Home"
  arrow <- if (identical(state$half, "top")) {
    HTML("&#9650;")
  } else {
    HTML("&#9660;")
  }

  b <- state$current_batter
  line <- if (is.null(b)) {
    NULL
  } else {
    batting_line_for(state, state$batting_team, b$player_id)
  }
  slot <- if (is.null(b) || is.na(b$order_slot)) {
    NULL
  } else {
    sprintf("#%d in the order", b$order_slot)
  }

  card(
    class = "bw-situation",
    full_screen = FALSE,
    card_body(
      class = "p-2",
      tags$div(
        class = "bw-sit-top d-flex align-items-center justify-content-between",
        .score_block(
          away_name,
          state$score$away,
          !final && identical(state$batting_team, "away")
        ),
        tags$div(
          class = "bw-sit-mid text-center",
          if (final) {
            tags$div(class = "bw-final", "FINAL")
          } else {
            tagList(
              tags$div(class = "bw-inning", arrow, " ", state$inning),
              tags$div(
                class = "bw-count",
                sprintf("%d-%d", state$count$balls, state$count$strikes)
              ),
              tags$div(
                class = "bw-outs text-muted",
                sprintf("%d out", state$outs)
              )
            )
          }
        ),
        .score_block(
          home_name,
          state$score$home,
          !final && identical(state$batting_team, "home")
        )
      ),
      if (!is.null(b)) {
        tags$div(
          class = "bw-sit-batter d-flex gap-2 align-items-baseline mt-1",
          tags$span(
            class = "bw-batter-name",
            paste0(
              if (!is.na(b$jersey_number)) {
                paste0("#", b$jersey_number, " ")
              } else {
                ""
              },
              b$name
            )
          ),
          if (!is.null(slot)) tags$span(class = "text-muted small", slot),
          tags$span(class = "text-muted small ms-auto", batter_line_text(line))
        )
      }
    )
  )
}
