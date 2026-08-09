# Enter or replace a lineup during a game. This is what makes a run-only team become a
# tracked team mid-inning, and what lets a lineup that was not ready at first pitch be
# entered later. The reducer's lineup_set branch re-evaluates the batting-order rule
# retroactively, so a rule broken before the lineup was known surfaces here.

lineup_modal_ui <- function(ns, state, team, show_gender, n_rows = 12L) {
  existing <- state$lineups[[team]] %||% list()
  existing <- existing[order(
    vapply(existing, function(p) p$order_slot %||% NA_integer_, integer(1)),
    na.last = TRUE
  )]
  n_rows <- max(as.integer(n_rows), length(existing) + 3L)
  prefix <- paste0("lu_", team)

  rows <- lapply(seq_len(n_rows), function(i) {
    p <- if (i <= length(existing)) existing[[i]] else NULL
    vals <- if (is.null(p)) {
      NULL
    } else {
      list(
        name = p$name,
        gender = p$gender,
        jersey = p$jersey_number,
        position = p$position
      )
    }
    .player_row(
      ns,
      prefix,
      i,
      order = i,
      show_gender = show_gender,
      values = vals
    )
  })

  modalDialog(
    title = sprintf("%s lineup", state$teams[[team]]$name %||% team),
    tags$p(
      class = "text-muted small",
      "Leave a row blank to skip it. Saving replaces this team's lineup; anything already recorded is re-checked against the rules."
    ),
    tags$div(
      class = "bw-lineup-wrap",
      tags$table(
        class = "table table-sm bw-lineup",
        .lineup_table_head(show_gender),
        tags$tbody(!!!rows)
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("lu_commit"), "Save lineup", class = "btn-primary")
    ),
    easyClose = FALSE,
    size = "l"
  )
}

build_lineup_set_event <- function(input, team, row_ids, show_gender) {
  lineup <- collect_lineup(
    input,
    paste0("lu_", team),
    row_ids,
    show_gender = show_gender
  )
  new_event("lineup_set", list(team = team, lineup = lineup))
}
