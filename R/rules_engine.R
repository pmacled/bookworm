default_ruleset_config <- function() {
  list(
    preset = "anything_goes",
    starting_count = list(balls = 0L, strikes = 0L),
    foul_out_rule = "unlimited",
    batting_gender_rule = list(type = "none", n = NA_integer_),
    male_walk_rule = "none",
    batting_size = NA_integer_,
    fielding = list(fielder_count = NA_integer_, min_females = 0L,
                    max_males = NA_integer_, tiers = list(),
                    position_requirements = list()),
    innings = 7L,
    run_cap = list(per_inning = NA_integer_, open_last_inning = TRUE,
                   same_play_runs_count = TRUE, cap_ends_half = TRUE),
    mercy_rule = list(tiers = list()),
    home_run_rule = list(over_fence_limit = NA_integer_, limit_by_gender = list(),
                         over_limit_result = "out", inside_park_counts = FALSE),
    pinch_runner = list(max_per_inning = NA_integer_, max_per_game = NA_integer_,
                        max_per_player_per_game = NA_integer_,
                        eligibility = "anyone", allowed_for = "anyone"),
    short_lineup_auto_out = FALSE
  )
}

.as_int_or_na <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) NA_integer_ else as.integer(x)

# Like utils::modifyList(), but array-valued fields (unnamed lists of records —
# mercy_rule$tiers, fielding$tiers) are replaced wholesale rather than
# recursed into. utils::modifyList() recurses whenever both sides are lists,
# and an unnamed list has no names() to iterate, so its recursive call is a
# silent no-op: a config's tiers would vanish, replaced by the (usually empty)
# default. Named sub-lists (starting_count, run_cap, ...) still merge key-by-key.
.merge_ruleset <- function(x, val) {
  if (!is.list(val)) return(val)
  vnames <- names(val)
  vnames <- if (is.null(vnames)) character() else vnames[nzchar(vnames)]
  if (length(vnames) == 0L) {
    # An unnamed, non-empty list is an array (tiers): replace wholesale. But an
    # *empty* list has no names either, and here it always means "the caller
    # didn't override this dict" (e.g. migrated `courtesy_runner = TRUE` leaves
    # pinch_runner = list()), not "replace the dict with nothing" -- keep `x`.
    return(if (length(val) == 0L) x else val)
  }
  if (!is.list(x)) x <- list()
  for (v in vnames) {
    x[[v]] <- if (v %in% names(x) && is.list(x[[v]]) && is.list(val[[v]]))
      .merge_ruleset(x[[v]], val[[v]])
    else val[[v]]
  }
  x
}

.BATTING_GENDER_ALIASES <- list(
  no_two_males_consecutive = list(type = "max_consecutive_males",       n = 1L),
  every_other              = list(type = "max_consecutive_same_gender", n = 1L),
  every_n                  = list(type = "min_females_per_n",           n = NA_integer_)
)

# Rewrites pre-slice-2 ruleset shapes in place. Idempotent: a config that is already
# in the new shape passes through untouched. Games persisted before slice 2 embed their
# ruleset in the game_start event and re-coerce on every load, so this must never lose data.
.migrate_ruleset_config <- function(cfg) {
  # run_cap: scalar top-level keys -> nested block
  if (!is.null(cfg$run_cap_per_inning) || !is.null(cfg$open_last_inning)) {
    # `cfg[["run_cap", exact = TRUE]]`, not `cfg$run_cap`: with `run_cap_per_inning`
    # still present and no exact `run_cap` key, `$`'s partial-name matching would
    # silently resolve to the scalar legacy field instead of NULL.
    rc <- cfg[["run_cap", exact = TRUE]] %||% list()
    if (is.null(rc$per_inning) && !is.null(cfg$run_cap_per_inning))
      rc$per_inning <- cfg$run_cap_per_inning
    if (is.null(rc$open_last_inning) && !is.null(cfg$open_last_inning))
      rc$open_last_inning <- cfg$open_last_inning
    cfg$run_cap <- rc
    cfg$run_cap_per_inning <- NULL
    cfg$open_last_inning <- NULL
  }

  # mercy: scalar differential/after_inning -> single-entry tiers list
  m <- cfg$mercy_rule
  if (!is.null(m) && !is.null(m$differential)) {
    d <- m$differential
    if (length(d) == 1 && !is.na(d)) {
      after <- m$after_inning
      after <- if (is.null(after) || length(after) != 1 || is.na(after)) 1L else as.integer(after)
      cfg$mercy_rule <- list(tiers = list(
        list(after_inning = after, differential = as.integer(d))))
    } else {
      cfg$mercy_rule <- list(tiers = m$tiers %||% list())
    }
  }

  # batting gender: renamed types
  bg <- cfg$batting_gender_rule
  if (!is.null(bg) && !is.null(bg$type) && bg$type %in% names(.BATTING_GENDER_ALIASES)) {
    alias <- .BATTING_GENDER_ALIASES[[bg$type]]
    n <- if (is.na(alias$n)) bg$n else alias$n
    cfg$batting_gender_rule <- list(type = alias$type, n = n)
  }

  # courtesy_runner boolean -> pinch_runner block
  if (!is.null(cfg$courtesy_runner)) {
    pr <- cfg$pinch_runner %||% list()
    if (is.null(pr$max_per_game) && identical(cfg$courtesy_runner, FALSE))
      pr$max_per_game <- 0L
    cfg$pinch_runner <- pr
    cfg$courtesy_runner <- NULL
  }
  cfg
}

coerce_ruleset_config <- function(cfg) {
  d <- default_ruleset_config()
  cfg <- .migrate_ruleset_config(cfg %||% list())
  d <- .merge_ruleset(d, cfg)

  d$starting_count$balls   <- as.integer(d$starting_count$balls)
  d$starting_count$strikes <- as.integer(d$starting_count$strikes)
  d$innings                <- as.integer(d$innings)
  d$batting_gender_rule$n  <- .as_int_or_na(d$batting_gender_rule$n)

  d$batting_size <- .as_int_or_na(d$batting_size)
  if (!is.na(d$batting_size) && d$batting_size < 1L) d$batting_size <- NA_integer_

  d$fielding$fielder_count <- .as_int_or_na(d$fielding$fielder_count)
  d$fielding$min_females   <- as.integer(d$fielding$min_females)
  d$fielding$max_males     <- .as_int_or_na(d$fielding$max_males)
  d$fielding$tiers         <- d$fielding$tiers %||% list()

  d$run_cap$per_inning           <- .as_int_or_na(d$run_cap$per_inning)
  d$run_cap$open_last_inning     <- isTRUE(d$run_cap$open_last_inning)
  d$run_cap$same_play_runs_count <- isTRUE(d$run_cap$same_play_runs_count)
  d$run_cap$cap_ends_half        <- isTRUE(d$run_cap$cap_ends_half)

  d$mercy_rule$tiers <- lapply(d$mercy_rule$tiers %||% list(), function(t)
    list(after_inning = .as_int_or_na(t$after_inning),
         differential = .as_int_or_na(t$differential)))

  d$home_run_rule$over_fence_limit <- .as_int_or_na(d$home_run_rule$over_fence_limit)
  d$home_run_rule$limit_by_gender <-
    lapply(d$home_run_rule$limit_by_gender %||% list(), .as_int_or_na)
  d$home_run_rule$inside_park_counts <- isTRUE(d$home_run_rule$inside_park_counts)

  for (k in c("max_per_inning", "max_per_game", "max_per_player_per_game"))
    d$pinch_runner[[k]] <- .as_int_or_na(d$pinch_runner[[k]])
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
  valid_bg <- c("none", "max_consecutive_males", "max_consecutive_same_gender",
                "min_females_per_n")
  if (!bg %in% valid_bg) add("invalid batting_gender_rule type")
  if (!identical(bg, "none") && is.na(cfg$batting_gender_rule$n))
    add(sprintf("%s batting rule requires n", bg))
  if (!identical(bg, "none") && !is.na(cfg$batting_gender_rule$n) &&
      cfg$batting_gender_rule$n < 1L)
    add("batting_gender_rule n must be >= 1")

  if (!cfg$male_walk_rule %in% c("none", "two_bases_then_female")) add("invalid male_walk_rule")
  if (!is.numeric(cfg$innings) || cfg$innings < 1) add("innings must be >= 1")

  if (!is.na(cfg$batting_size) && (!is.numeric(cfg$batting_size) || cfg$batting_size < 1)) {
    add("batting_size must be a positive integer or NA (unlimited)")
  }

  for (t in cfg$mercy_rule$tiers) {
    # %||%, not a bare $: a hand-built tier list (as opposed to one that has
    # passed through coerce_ruleset_config) may omit a key entirely, and
    # is.na(NULL) is logical(0) -- `||`ing that in gives NA, not FALSE.
    ai <- t$after_inning %||% NA_integer_
    df <- t$differential %||% NA_integer_
    if (is.na(ai) || is.na(df))
      add("each mercy tier needs after_inning and differential")
    else if (ai < 1L || df < 1L)
      add("mercy tier values must be >= 1")
  }

  if (!cfg$home_run_rule$over_limit_result %in%
      c("out", "ground_rule_double", "single")) add("invalid over_limit_result")
  if (!cfg$pinch_runner$eligibility %in%
      c("anyone", "same_gender", "last_out", "last_same_gender_out"))
    add("invalid pinch_runner eligibility")
  if (!cfg$pinch_runner$allowed_for %in% c("anyone", "pitcher_catcher"))
    add("invalid pinch_runner allowed_for")

  list(ok = length(errors) == 0, errors = errors)
}

next_batter_gender_ok <- function(cfg, prev_genders, next_gender) {
  rule <- cfg$batting_gender_rule
  type <- rule$type
  if (identical(type, "none")) return(TRUE)
  n <- rule$n
  # A rule that needs n but never got one (e.g. a persisted event that never
  # re-validated) must fail open, not crash: tail(x, NA + 1L) throws.
  if (is.na(n)) return(TRUE)
  # NA-safe equality: an unknown gender (NA) must never satisfy `identical(x, g)`,
  # whereas the vectorized `x == g` used previously turns any NA in `recent` into
  # an NA result, and all()/any() on a vector containing NA (with no TRUE already
  # present) returns NA rather than TRUE/FALSE -- which then blows up the caller's
  # `if (!next_batter_gender_ok(...))`.
  is_g <- function(x, g) !is.na(x) && identical(x, g)

  if (identical(type, "max_consecutive_males")) {
    recent <- tail(c(prev_genders, next_gender), n + 1L)
    all_m <- length(recent) == n + 1L && all(vapply(recent, is_g, logical(1), "M"))
    return(!all_m)
  }
  if (identical(type, "max_consecutive_same_gender")) {
    recent <- tail(c(prev_genders, next_gender), n + 1L)
    all_same <- length(recent) == n + 1L && !is.na(recent[1]) && length(unique(recent)) == 1L
    return(!all_same)
  }
  if (identical(type, "min_females_per_n")) {
    recent <- tail(c(prev_genders, next_gender), n)
    if (length(recent) < n) return(TRUE)  # window not full yet; cannot be violated
    return(any(vapply(recent, is_g, logical(1), "F")))  # at least one F in every window of n
  }
  TRUE
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

# Returns how many of `runs_on_play` actually count, and whether the cap was reached.
# same_play_runs_count = TRUE: a play in progress completes fully -- even a play that
# pushes the half past the cap (e.g. a grand slam) counts every run -- but once an
# *earlier* play has already brought the half to the cap, this play is stopped
# entirely: the cap stops the *next* batter, not the one at bat when it was reached.
# same_play_runs_count = FALSE: runs are clamped mid-play, right at the cap.
apply_run_cap <- function(cfg, runs_before, runs_on_play, inning) {
  # `cfg[["run_cap", exact = TRUE]]`, not `cfg$run_cap`: callers are expected to pass
  # an already-coerced ruleset (no flat legacy keys left to collide with), but this
  # guards the one remaining spot that trusted that instead of enforcing it.
  rc <- cfg[["run_cap", exact = TRUE]]
  cap <- rc$per_inning
  runs_on_play <- as.integer(runs_on_play)
  runs_before <- as.integer(runs_before)
  if (is.na(cap)) return(list(runs = runs_on_play, cap_hit = FALSE))
  if (isTRUE(rc$open_last_inning) && inning >= cfg$innings)
    return(list(runs = runs_on_play, cap_hit = FALSE))
  total <- runs_before + runs_on_play
  runs <- if (isTRUE(rc$same_play_runs_count)) {
    if (runs_before >= cap) 0L else runs_on_play
  } else {
    # Clamp to *remaining room under the cap*, not to the room itself: an
    # at-or-below-cap entry (e.g. half_runs of 3 against a cap of 5) must
    # pass through unchanged, not get inflated up to the cap.
    min(runs_on_play, max(0L, cap - runs_before))
  }
  list(runs = as.integer(runs), cap_hit = total >= cap)
}

game_should_end <- function(cfg, state) {
  diff <- abs(state$score$home - state$score$away)
  for (t in cfg$mercy_rule$tiers %||% list()) {
    # %||% NA_integer_ first: a tier missing a key entirely (t$after_inning is
    # then NULL, not NA) must not reach `is.na(t$after_inning)` -- and a
    # persisted event never re-validates, so a malformed tier is reachable here.
    after <- t$after_inning %||% NA_integer_
    after <- if (is.na(after)) 1L else after
    diff_needed <- t$differential %||% NA_integer_
    if (!is.na(diff_needed) && state$inning >= after && diff >= diff_needed) return(TRUE)
  }
  # Regulation complete: finished the bottom of the final inning.
  if (state$inning > cfg$innings) return(TRUE)
  FALSE
}
