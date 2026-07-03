default_ruleset_config <- function() {
  list(
    starting_count = list(balls = 1L, strikes = 1L),
    foul_out_rule = "out",
    batting_gender_rule = list(type = "none", n = NA_integer_),
    male_walk_rule = "none",
    fielding = list(min_females = 0L, position_requirements = list()),
    innings = 7L,
    run_cap_per_inning = NA_integer_,
    open_last_inning = TRUE,
    mercy_rule = list(differential = NA_integer_, after_inning = NA_integer_),
    short_lineup_auto_out = FALSE,
    courtesy_runner = FALSE
  )
}

.as_int_or_na <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA_integer_ else as.integer(x)

coerce_ruleset_config <- function(cfg) {
  d <- default_ruleset_config()
  cfg <- cfg %||% list()
  d <- utils::modifyList(d, cfg)
  d$starting_count$balls <- as.integer(d$starting_count$balls)
  d$starting_count$strikes <- as.integer(d$starting_count$strikes)
  d$innings <- as.integer(d$innings)
  d$run_cap_per_inning <- .as_int_or_na(d$run_cap_per_inning)
  d$batting_gender_rule$n <- .as_int_or_na(d$batting_gender_rule$n)
  d$fielding$min_females <- as.integer(d$fielding$min_females)
  d$mercy_rule$differential <- .as_int_or_na(d$mercy_rule$differential)
  d$mercy_rule$after_inning <- .as_int_or_na(d$mercy_rule$after_inning)
  d
}

validate_ruleset_config <- function(cfg) {
  errors <- character()
  add <- function(msg) errors <<- c(errors, msg)

  b <- cfg$starting_count$balls; s <- cfg$starting_count$strikes
  if (!is.numeric(b) || b < 0 || b > 3) add("starting balls must be 0-3")
  if (!is.numeric(s) || s < 0 || s > 2) add("starting strikes must be 0-2")
  if (!identical(cfg$foul_out_rule, "out") &&
      !identical(cfg$foul_out_rule, "one_courtesy_foul")) add("invalid foul_out_rule")

  bg <- cfg$batting_gender_rule$type
  if (!bg %in% c("none", "no_two_males_consecutive", "every_other", "every_n")) {
    add("invalid batting_gender_rule type")
  }
  if (identical(bg, "every_n") && is.na(cfg$batting_gender_rule$n)) {
    add("every_n batting rule requires n")
  }
  if (!cfg$male_walk_rule %in% c("none", "two_bases_then_female")) add("invalid male_walk_rule")
  if (!is.numeric(cfg$innings) || cfg$innings < 1) add("innings must be >= 1")

  list(ok = length(errors) == 0, errors = errors)
}
