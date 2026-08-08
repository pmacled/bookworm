default_ruleset_config <- function() {
  list(
    starting_count = list(balls = 1L, strikes = 1L),
    foul_out_rule = "out",
    batting_gender_rule = list(type = "none", n = NA_integer_),
    male_walk_rule = "none",
    fielding = list(min_females = 0L, max_males = NA_integer_, tiers = list(),
                    position_requirements = list()),
    innings = 7L,
    run_cap_per_inning = NA_integer_,
    open_last_inning = TRUE,
    mercy_rule = list(differential = NA_integer_, after_inning = NA_integer_),
    short_lineup_auto_out = FALSE,
    courtesy_runner = FALSE,
    batting_size = NA_integer_
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
  d$fielding$max_males <- .as_int_or_na(d$fielding$max_males)
  d$fielding$tiers <- d$fielding$tiers %||% list()
  d$mercy_rule$differential <- .as_int_or_na(d$mercy_rule$differential)
  d$mercy_rule$after_inning <- .as_int_or_na(d$mercy_rule$after_inning)
  d$batting_size <- .as_int_or_na(d$batting_size)
  if (!is.na(d$batting_size) && d$batting_size < 1L) d$batting_size <- NA_integer_
  d
}

validate_ruleset_config <- function(cfg) {
  errors <- character()
  add <- function(msg) errors <<- c(errors, msg)

  b <- cfg$starting_count$balls; s <- cfg$starting_count$strikes
  if (!is.numeric(b) || b < 0 || b > 3) add("starting balls must be 0-3")
  if (!is.numeric(s) || s < 0 || s > 2) add("starting strikes must be 0-2")
  if (!cfg$foul_out_rule %in% c("out", "one_courtesy_foul", "unlimited")) add("invalid foul_out_rule")

  bg <- cfg$batting_gender_rule$type
  if (!bg %in% c("none", "no_two_males_consecutive", "every_other", "every_n")) {
    add("invalid batting_gender_rule type")
  }
  if (identical(bg, "every_n") && is.na(cfg$batting_gender_rule$n)) {
    add("every_n batting rule requires n")
  }
  if (!cfg$male_walk_rule %in% c("none", "two_bases_then_female")) add("invalid male_walk_rule")
  if (!is.numeric(cfg$innings) || cfg$innings < 1) add("innings must be >= 1")

  if (!is.na(cfg$batting_size) && (!is.numeric(cfg$batting_size) || cfg$batting_size < 1)) {
    add("batting_size must be a positive integer or NA (unlimited)")
  }

  list(ok = length(errors) == 0, errors = errors)
}

next_batter_gender_ok <- function(cfg, prev_genders, next_gender) {
  rule <- cfg$batting_gender_rule
  type <- rule$type
  if (type == "none") return(TRUE)
  last <- if (length(prev_genders)) tail(prev_genders, 1) else NA_character_
  if (type == "no_two_males_consecutive") return(!(identical(last, "M") && identical(next_gender, "M")))
  if (type == "every_other") return(is.na(last) || !identical(last, next_gender))
  if (type == "every_n") {
    n <- cfg$batting_gender_rule$n
    recent <- tail(c(prev_genders, next_gender), n)
    return(any(recent == "F"))  # at least one F in every window of n
  }
  TRUE
}

fielding_warnings <- function(cfg, defense_lineup) {
  warns <- character()
  on_d <- Filter(function(p) !is.na(p$position), defense_lineup)
  n_f <- sum(vapply(on_d, function(p) identical(p$gender, "F"), logical(1)))
  if (n_f < (cfg$fielding$min_females %||% 0L))
    warns <- c(warns, sprintf("Fielding requires ≥ %d female players (currently %d).",
                              cfg$fielding$min_females, n_f))
  warns
}

STANDARD_COED_FIELDING <- list(
  min_females = 4L, max_males = 6L,
  tiers = list(
    list(females = 3L, outfield = 1L, infield = 1L, battery = "one"),
    list(females = 4L, outfield = 1L, infield = 1L, battery = "one"),
    list(females = 5L, outfield = 2L, infield = 2L, battery = "one"),
    list(females = 6L, outfield = 1L, infield = 1L, battery = "any")
  ),
  position_requirements = list()
)

.position_category <- function(pos) {
  if (is.null(pos) || length(pos) != 1 || is.na(pos)) return(NA_character_)
  key <- as.character(pos)
  # POSITION_CATEGORY is a named CHARACTER VECTOR: `[[missing]]` throws
  # "subscript out of bounds", so gate on membership first.
  if (!key %in% names(APP_CONFIG$POSITION_CATEGORY)) return(NA_character_)
  unname(APP_CONFIG$POSITION_CATEGORY[[key]])
}

evaluate_fielding <- function(cfg, defense_lineup) {
  f <- cfg$fielding
  viol <- list()
  add <- function(code, message)
    viol[[length(viol) + 1]] <<- list(severity = "violation", code = code, message = message)

  fielders <- Filter(function(p) !is.na(.position_category(p$position)), defense_lineup)
  if (length(fielders) == 0) return(list())  # cannot evaluate without positions

  cat_of <- vapply(fielders, function(p) .position_category(p$position), character(1))
  is_f <- vapply(fielders, function(p) identical(p$gender, "F"), logical(1))
  Ftot <- sum(is_f); Mtot <- sum(!is_f)
  n_of <- sum(is_f & cat_of == "outfield")
  n_if <- sum(is_f & cat_of == "infield")

  minf <- f$min_females %||% 0L
  if (Ftot < minf) add("min_females", sprintf("Need at least %d females in the field (have %d).", minf, Ftot))
  maxm <- f$max_males
  if (!is.null(maxm) && !is.na(maxm) && Mtot > maxm)
    add("max_males", sprintf("No more than %d males in the field (have %d).", maxm, Mtot))

  tiers <- f$tiers %||% list()
  if (length(tiers) > 0) {
    thr <- vapply(tiers, function(t) as.integer(t$females), integer(1))
    ord <- order(thr); tiers <- tiers[ord]; thr <- thr[ord]
    hits <- which(thr <= Ftot)
    tier <- if (length(hits)) tiers[[max(hits)]] else tiers[[1]]
    if (n_of < as.integer(tier$outfield))
      add("outfield_min", sprintf("Need at least %d females in the outfield (have %d).", tier$outfield, n_of))
    if (n_if < as.integer(tier$infield))
      add("infield_min", sprintf("Need at least %d females in the infield (have %d).", tier$infield, n_if))
    if (identical(tier$battery, "one")) {
      ppos <- Filter(function(p) identical(as.character(p$position), "P"), fielders)
      cpos <- Filter(function(p) identical(as.character(p$position), "C"), fielders)
      if (length(ppos) && length(cpos) && identical(ppos[[1]]$gender, cpos[[1]]$gender))
        add("battery_opposite", "Pitcher and catcher must be opposite genders.")
    }
  }
  viol
}

apply_run_cap <- function(cfg, runs_this_half, inning) {
  cap <- cfg$run_cap_per_inning
  if (is.na(cap)) return(as.integer(runs_this_half))
  if (isTRUE(cfg$open_last_inning) && inning >= cfg$innings) return(as.integer(runs_this_half))
  as.integer(min(runs_this_half, cap))
}

game_should_end <- function(cfg, state) {
  m <- cfg$mercy_rule
  if (!is.na(m$differential)) {
    diff <- abs(state$score$home - state$score$away)
    after <- if (is.na(m$after_inning)) 1L else m$after_inning  # %||% won't catch NA
    if (state$inning >= after && diff >= m$differential) return(TRUE)
  }
  # Regulation complete: finished the bottom of the final inning.
  if (state$inning > cfg$innings) return(TRUE)
  FALSE
}
