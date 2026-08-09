# Over-the-fence home-run limits. Pure: never mutates state, never touches Shiny.
# Called by the tracking module before an outcome is committed to an event.

.OVER_FENCE <- "HR"
.INSIDE_PARK <- "ITPHR"
.OVER_LIMIT_OUTCOME <- c(out = "GO", ground_rule_double = "2B", single = "1B")

# Home runs already hit by `team` that count toward the limit.
count_over_fence_home_runs <- function(cfg, state, team) {
  counted <- if (isTRUE(cfg$home_run_rule$inside_park_counts)) {
    c(.OVER_FENCE, .INSIDE_PARK)
  } else {
    .OVER_FENCE
  }
  sum(vapply(
    state$pa_log %||% list(),
    function(r) identical(r$team, team) && (r$outcome %in% counted),
    logical(1)
  ))
}

# The limit in force for this batter: a per-gender override wins over the overall limit.
.effective_hr_limit <- function(cfg, batter) {
  by_gender <- cfg$home_run_rule$limit_by_gender %||% list()
  g <- batter$gender %||% NA_character_
  if (!is.na(g) && !is.null(by_gender[[g]])) {
    return(by_gender[[g]])
  }
  cfg$home_run_rule$over_fence_limit
}

evaluate_home_run_limit <- function(cfg, state, batter, outcome) {
  pass <- list(outcome = outcome, warning = NULL)
  # An inside-the-park home run is never rewritten; inside_park_counts only affects
  # whether previous ones count toward the total.
  if (!identical(outcome, .OVER_FENCE)) {
    return(pass)
  }

  limit <- .effective_hr_limit(cfg, batter)
  if (is.null(limit) || length(limit) != 1 || is.na(limit)) {
    return(pass)
  }

  team <- state$batting_team
  already <- count_over_fence_home_runs(cfg, state, team)
  if (already < limit) {
    return(pass)
  }

  replacement <- unname(.OVER_LIMIT_OUTCOME[[
    cfg$home_run_rule$over_limit_result
  ]])
  # APP_CONFIG$outcome_meta arrived with slice 2.0 (ab9be84) and every replacement
  # code above is a key in it, so this lookup resolves; %||% is belt-and-braces
  # against a future code being added to .OVER_LIMIT_OUTCOME but not to the glossary.
  lbl <- APP_CONFIG$outcome_meta[[replacement]]$label %||% replacement
  list(
    outcome = replacement,
    warning = list(
      severity = "notice",
      code = "home_run_limit",
      message = sprintf(
        "Home-run limit of %d reached; recorded as %s (%s).",
        limit,
        replacement,
        lbl
      )
    )
  )
}
