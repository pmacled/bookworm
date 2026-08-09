# The runner-disposition modal. Rendering only — every rule lives in R/disposition.R.

.BASE_LABEL <- c("batter", "first", "second", "third")

.disposition_row_ui <- function(ns, row, selected) {
  label <- paste0(
    if (!is.na(row$jersey)) paste0("#", row$jersey, " ") else "",
    row$name, "  (", .BASE_LABEL[[row$from + 1L]], ")")
  tags$div(class = "bw-disp-row d-flex align-items-center justify-content-between gap-2",
    tags$span(class = "bw-disp-name", label),
    radioButtons(ns(paste0("disp_", row$runner_id)), NULL,
      choices = stats::setNames(DISPOSITION_LEVELS, DISPOSITION_LEVELS),
      selected = selected, inline = TRUE))
}

disposition_modal_ui <- function(ns, rows, prefill, outcome, errors = character()) {
  lbl <- APP_CONFIG$outcome_meta[[outcome]]$label %||% outcome
  runs_default <- sum(prefill == "H")
  modalDialog(
    title = sprintf("%s — where did everyone end up?", lbl),
    tags$p(class = "text-muted small",
      "1, 2, 3 = the base they finished on. H = scored. OUT = out on the play."),
    tags$div(class = "bw-disp-grid",
      !!!lapply(rows, function(r) .disposition_row_ui(ns, r, prefill[[r$runner_id]]))),
    if (length(errors))
      tags$div(class = "alert alert-danger py-2 small mt-2",
        tags$ul(class = "mb-0", !!!lapply(errors, tags$li))),
    tags$hr(),
    numericInput(ns("disp_rbi"), "RBI", value = runs_default, min = 0, max = 4,
                 width = "7rem"),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("disp_commit"), "Commit play", class = "btn-primary")),
    easyClose = FALSE, size = "m")
}

read_disposition_choices <- function(input, rows) {
  out <- character(0)
  for (r in rows) {
    v <- input[[paste0("disp_", r$runner_id)]]
    out[[r$runner_id]] <- if (is.null(v) || length(v) != 1 || is.na(v)) "" else as.character(v)
  }
  out
}
