# Outcome-code help: the glossary panel.
# Reads APP_CONFIG$outcome_meta so the vocabulary has one source of truth.

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
        paste("Outcome codes used in Bookworm. The ten most common have buttons",
              "on the tracking screen; the rest are recognised by the scorebook",
              "and by imported games.")),
      !!!Filter(Negate(is.null), sections)))
}
