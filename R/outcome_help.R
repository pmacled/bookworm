# Outcome-code help: the glossary panel and the per-button popovers.
# Both read APP_CONFIG$outcome_meta so the vocabulary has one source of truth.

.OUTCOME_CATEGORY_LABELS <- c(
  hit = "Hits", on_base = "Reached base", out = "Outs", other = "Other"
)

outcome_help_ui <- function() {
  sections <- lapply(names(.OUTCOME_CATEGORY_LABELS), function(cat) {
    codes <- names(Filter(function(m) identical(m$category, cat), APP_CONFIG$outcome_meta))
    if (!length(codes)) return(NULL)
    rows <- lapply(codes, function(code) {
      m <- APP_CONFIG$outcome_meta[[code]]
      tags$tr(
        tags$th(scope = "row", class = "bw-help-code", code),
        tags$td(class = "bw-help-label", m$label),
        tags$td(class = "bw-help-desc text-muted small", m$description))
    })
    tagList(
      tags$h5(class = "mt-3", .OUTCOME_CATEGORY_LABELS[[cat]]),
      tags$table(class = "table table-sm bw-help-table", tags$tbody(!!!rows)))
  })
  tagList(
    tags$div(class = "p-3",
      tags$p(class = "text-muted small",
        "Codes you can record for a plate appearance. Tap and hold an outcome button during a game for the same description."),
      !!!Filter(Negate(is.null), sections)))
}

# Wraps an outcome actionButton in a popover carrying its label and description.
# `ns_id` must already be namespaced; the button's id is unchanged so the existing
# observeEvent(input[[paste0("o_", code)]]) wiring keeps working.
outcome_button <- function(ns_id, code) {
  m <- APP_CONFIG$outcome_meta[[code]]
  if (is.null(m)) stop(sprintf("unknown outcome code: %s", code))
  bslib::popover(
    actionButton(ns_id, code, class = "btn-outline-primary bw-outcome-btn"),
    tags$strong(m$label), tags$br(), m$description,
    title = code, placement = "top")
}
