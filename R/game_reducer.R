initial_game_state <- function(ruleset = default_ruleset_config()) {
  ruleset <- coerce_ruleset_config(ruleset)
  list(
    status = "in_progress",
    inning = 1L,
    half = "top",
    outs = 0L,
    count = list(
      balls = ruleset$starting_count$balls,
      strikes = ruleset$starting_count$strikes
    ),
    bases = list(
      first = NA_character_,
      second = NA_character_,
      third = NA_character_
    ),
    score = list(home = 0L, away = 0L),
    runs_this_half = 0L,
    lineups = list(home = list(), away = list()),
    batting_index = list(home = 0L, away = 0L),
    batting_team = "away",
    current_batter = NULL,
    pa_log = list(),
    line_score = list(home = integer(), away = integer()),
    warnings = list(),
    ruleset = ruleset,
    cap_hit_last_play = FALSE,
    cap_hit_bulk = FALSE,
    pinch_runner_log = list(),
    runner_origin = list(),
    pa_counter = list(home = 0L, away = 0L)
  )
}

reset_count <- function(state) {
  state$count <- list(
    balls = state$ruleset$starting_count$balls,
    strikes = state$ruleset$starting_count$strikes
  )
  state
}

.set_current_batter <- function(state) {
  team <- state$batting_team
  lineup <- state$lineups[[team]]
  if (length(lineup) == 0) {
    state$current_batter <- NULL
    return(state)
  }
  batters <- Filter(function(p) !is.na(p$order_slot), lineup)
  if (length(batters) == 0) {
    state$current_batter <- NULL
    return(state)
  }
  batters <- batters[order(vapply(
    batters,
    function(p) p$order_slot,
    integer(1)
  ))]
  idx <- (state$batting_index[[team]] %% length(batters))
  state$current_batter <- batters[[idx + 1]]
  state
}

advance_half <- function(state) {
  # record runs for the completed half into the line score
  state$line_score[[state$batting_team]] <-
    c(state$line_score[[state$batting_team]], state$runs_this_half)
  if (identical(state$half, "top")) {
    state$half <- "bottom"
    state$batting_team <- "home"
  } else {
    state$half <- "top"
    state$batting_team <- "away"
    state$inning <- state$inning + 1L
  }
  state$outs <- 0L
  state$runs_this_half <- 0L
  state$bases <- list(
    first = NA_character_,
    second = NA_character_,
    third = NA_character_
  )
  # A new half starts with empty bases, so no origins carry over. advance_half()
  # has already flipped batting_team, so this resets the counter for the incoming team.
  state$runner_origin <- list()
  state$pa_counter[[state$batting_team]] <- 0L
  state <- reset_count(state)
  state <- .set_current_batter(state)
  state
}

# Replays the batting-order gender rule over plate appearances already recorded for
# `team`, using the lineup that is now known. Returns a single violation naming the
# earliest offending batter, or NULL. This is what surfaces a rule break that could not
# be evaluated while the lineup was empty. Only called from the lineup_set branch of
# apply_event() -- this is specifically the "warn once the batter is registered" case,
# not a general substitute for the live due-up check in .refresh_flags().
.retro_batting_gender_violation <- function(state, team) {
  cfg <- state$ruleset
  if (identical(cfg$batting_gender_rule$type, "none")) {
    return(NULL)
  }
  recs <- Filter(function(r) identical(r$team, team), state$pa_log %||% list())
  if (length(recs) < 2L) {
    return(NULL)
  }
  gender_of <- function(id) {
    hit <- Filter(function(p) identical(p$player_id, id), state$lineups[[team]])
    if (length(hit)) hit[[1]]$gender else NA_character_
  }
  seen <- character()
  for (r in recs) {
    g <- gender_of(r$batter_id)
    if (is.na(g)) {
      next
    }
    if (!next_batter_gender_ok(cfg, seen, g)) {
      nm <- Filter(
        function(p) identical(p$player_id, r$batter_id),
        state$lineups[[team]]
      )
      nm <- if (length(nm)) nm[[1]]$name else r$batter_id
      return(list(
        severity = "violation",
        code = "batting_gender_retro",
        message = sprintf(
          "Batting order: %s batted out of order under this ruleset (inning %d).",
          nm,
          r$inning
        )
      ))
    }
    seen <- c(seen, g)
  }
  NULL
}

.refresh_flags <- function(state) {
  cfg <- state$ruleset
  def_team <- if (identical(state$batting_team, "away")) "home" else "away"
  w <- evaluate_fielding(cfg, state$lineups[[def_team]]) # list of violation items

  if (!is.null(state$current_batter)) {
    bt <- state$batting_team
    prev_genders <- vapply(
      Filter(function(r) identical(r$team, bt), state$pa_log),
      function(r) {
        pl <- Filter(
          function(p) identical(p$player_id, r$batter_id),
          state$lineups[[bt]]
        )
        if (length(pl)) pl[[1]]$gender else NA_character_
      },
      character(1)
    )
    prev_genders <- prev_genders[!is.na(prev_genders)]
    if (
      !next_batter_gender_ok(cfg, prev_genders, state$current_batter$gender)
    ) {
      w <- c(
        w,
        list(list(
          severity = "violation",
          code = "batting_gender",
          message = "Batting order: the batter due up violates the gender rule."
        ))
      )
    }
  }

  if (isTRUE(state$cap_hit_last_play)) {
    # The second sentence has to match what actually happened. Under
    # same_play_runs_count = FALSE a grand slam really is clamped (4 -> 3 against a
    # cap of 3), so the "same at-bat" wording would state the opposite; and a bulk
    # half-inning entry has no at-bats at all and is always clamped, so no caveat
    # applies. Only the first case is the one the owner's wording describes.
    detail <- if (isTRUE(state$cap_hit_bulk)) {
      ""
    } else if (isTRUE(cfg$run_cap$same_play_runs_count)) {
      " Runs beyond the cap only count if they happen in the same at-bat that reaches it."
    } else {
      " Runs beyond the cap did not count."
    }
    w <- c(
      w,
      list(list(
        severity = "notice",
        code = "run_cap",
        message = sprintf(
          "Run cap of %d reached this inning.%s",
          cfg$run_cap$per_inning,
          detail
        )
      ))
    )
  }

  bs <- cfg$batting_size
  if (!is.na(bs)) {
    n_bat <- length(Filter(
      function(p) !is.na(p$order_slot),
      state$lineups[[state$batting_team]]
    ))
    if (n_bat > 0 && n_bat != bs) {
      w <- c(
        w,
        list(list(
          severity = "notice",
          code = "batting_size",
          message = sprintf(
            "Batting team has %d batters; rule expects %d.",
            n_bat,
            bs
          )
        ))
      )
    }
  }

  fc <- cfg$fielding$fielder_count
  if (!is.na(fc)) {
    n_field <- length(Filter(
      function(p) !is.na(.position_category(p$position)),
      state$lineups[[def_team]]
    ))
    if (n_field > 0L && n_field != fc) {
      w <- c(
        w,
        list(list(
          severity = "notice",
          code = "fielder_count",
          message = sprintf(
            "%d fielders have positions; rule expects %d.",
            n_field,
            fc
          )
        ))
      )
    }
  }

  # DERIVED, not latched. Latching made "final" a one-way trap: tracking_module
  # refuses every input once final and nothing ever set it back, so an Undo or a
  # differential that shrinks again could not reopen the game. Because the load
  # path is fold_events(), recomputing here is exactly as cheap and always agrees
  # with the current state. Regulation completion (inning > innings) never reverts
  # on its own, so it simply stays true.
  is_final <- game_should_end(cfg, state)
  state$status <- if (is_final) "final" else "in_progress"
  if (is_final) {
    w <- c(
      w,
      list(list(
        severity = "notice",
        code = "final",
        message = "Game is final."
      ))
    )
  }

  state$warnings <- w
  state
}

apply_event <- function(state, evt) {
  type <- evt$type
  if (type != "game_start") {
    state$cap_hit_last_play <- FALSE
    state$cap_hit_bulk <- FALSE
  }
  if (type == "game_start") {
    p <- evt$payload
    state <- initial_game_state(p$ruleset %||% state$ruleset)
    state$lineups$home <- p$home$lineup
    state$lineups$away <- p$away$lineup
    state$teams <- list(
      home = p$home[c("team_id", "name")],
      away = p$away[c("team_id", "name")]
    )
    state$batting_team <- p$first_bat %||% "away"
    state <- .set_current_batter(state)
    return(.refresh_flags(state))
  }
  if (type == "count_override") {
    state$count <- list(
      balls = as.integer(evt$payload$balls),
      strikes = as.integer(evt$payload$strikes)
    )
    return(.refresh_flags(state))
  }
  if (type == "inning_end") {
    return(.refresh_flags(advance_half(state)))
  }
  if (type == "plate_appearance") {
    # Full advance/scoring logic added in Task 5; core handles outs + turn here.
    state <- apply_plate_appearance(state, evt) # defined in Task 5
    return(.refresh_flags(state))
  }
  if (type == "half_runs") {
    team <- state$batting_team
    runs <- as.integer(evt$payload$runs %||% 0L)
    # A half_runs entry is a bulk total for the whole half, not a single play in
    # progress, so same_play_runs_count (which exists to let one play finish in
    # full, e.g. a grand slam) must not apply: a bulk entry is always clamped
    # at the cap, regardless of the ruleset's same_play_runs_count setting.
    cap_cfg <- state$ruleset
    cap_cfg$run_cap$same_play_runs_count <- FALSE
    cr <- apply_run_cap(cap_cfg, state$runs_this_half, runs, state$inning)
    state$score[[team]] <- state$score[[team]] + cr$runs
    state$runs_this_half <- state$runs_this_half + cr$runs
    state <- advance_half(state)
    state$cap_hit_last_play <- cr$cap_hit # set AFTER advance_half so the notice survives
    state$cap_hit_bulk <- cr$cap_hit # ... and mark it as a bulk entry, not a play
    return(.refresh_flags(state))
  }
  if (type == "lineup_set") {
    team <- evt$payload$team
    state$lineups[[team]] <- evt$payload$lineup %||% list()
    n <- length(Filter(function(p) !is.na(p$order_slot), state$lineups[[team]]))
    if (n > 0L) {
      state$batting_index[[team]] <- state$batting_index[[team]] %% n
    } else {
      state$batting_index[[team]] <- 0L
    }
    # current_batter is a single slot scoped to whichever team is currently batting.
    # lineup_set can arrive for either team -- e.g. entering a defensive team's
    # lineup while the run-only team bats -- so only recompute current_batter when
    # `team` is the one actually up; otherwise leave the batting team's own slot
    # alone. (.set_current_batter() always derives from state$batting_team, so this
    # guard is also a safety net if that ever changes to take a team argument again.)
    if (identical(team, state$batting_team)) {
      state <- .set_current_batter(state)
    }
    state <- .refresh_flags(state)
    retro <- .retro_batting_gender_violation(state, team)
    if (!is.null(retro)) {
      state$warnings <- c(state$warnings, list(retro))
    }
    return(state)
  }
  if (type == "substitution") {
    return(.refresh_flags(apply_substitution(state, evt)))
  } # Task 7
  state
}

fold_events <- function(events, ruleset = NULL) {
  state <- initial_game_state(ruleset %||% default_ruleset_config())
  for (evt in events) {
    state <- apply_event(state, evt)
  }
  state
}

.clear_base_of <- function(bases, runner_id) {
  for (b in c("first", "second", "third")) {
    if (!is.na(bases[[b]]) && bases[[b]] == runner_id) {
      bases[[b]] <- NA_character_
    }
  }
  bases
}
.base_slot <- function(n) {
  c("1" = "first", "2" = "second", "3" = "third")[as.character(n)]
}

apply_plate_appearance <- function(state, evt) {
  p <- evt$payload
  team <- state$batting_team
  bases <- state$bases
  runs <- 0L
  own_index <- length(state$pa_log) + 1L
  origin <- state$runner_origin %||% list()

  advances <- p$advances %||% list()
  # Resolve each advance to the plate appearance that put that runner on base. The
  # batter's own advance (from == 0) belongs to this entry; everyone else looks theirs up.
  advances <- lapply(advances, function(a) {
    a$origin_index <- if (identical(as.integer(a$from), 0L)) {
      own_index
    } else {
      origin[[a$runner_id]] %||% NA_integer_
    }
    a
  })

  # Apply each advance: remove runner from old base, place at new base or score/out.
  for (a in advances) {
    bases <- .clear_base_of(bases, a$runner_id)
    if (isTRUE(a$scored)) {
      runs <- runs + 1L
      origin[[a$runner_id]] <- NULL
    } else if (isTRUE(a$out)) {
      origin[[a$runner_id]] <- NULL
    } else if (a$to %in% c(1L, 2L, 3L)) {
      bases[[.base_slot(a$to)]] <- a$runner_id
      origin[[a$runner_id]] <- a$origin_index
    }
  }
  # Batter's own landing spot if not covered by an advance and not out.
  # %||% guards against NULL, which occurs when a NA_integer_ `reached`
  # round-trips through JSON (jsonlite emits null; simplifyVector=FALSE
  # reads it back as NULL rather than NA_integer_).
  reached <- p$reached %||% NA_integer_
  if (!is.na(reached) && reached %in% c(1L, 2L, 3L)) {
    already <- any(vapply(
      advances,
      function(a) identical(a$runner_id, p$batter_id),
      logical(1)
    ))
    if (!already) {
      bases[[.base_slot(reached)]] <- p$batter_id
      origin[[p$batter_id]] <- own_index
    }
  }
  if (!is.na(reached) && reached == 4L) {
    already <- any(vapply(
      advances,
      function(a) identical(a$runner_id, p$batter_id) && isTRUE(a$scored),
      logical(1)
    ))
    if (!already) runs <- runs + 1L
  }

  state$bases <- bases
  state$runner_origin <- origin
  state$pa_counter[[team]] <- (state$pa_counter[[team]] %||% 0L) + 1L
  outs_before <- state$outs
  cr <- apply_run_cap(state$ruleset, state$runs_this_half, runs, state$inning)
  runs <- cr$runs
  state$cap_hit_last_play <- cr$cap_hit
  state$score[[team]] <- state$score[[team]] + runs
  state$runs_this_half <- state$runs_this_half + runs
  state$outs <- state$outs + as.integer(p$outs_on_play %||% 0L)

  state$pa_log <- c(
    state$pa_log,
    list(list(
      inning = state$inning,
      half = state$half,
      team = team,
      batter_id = p$batter_id,
      outcome = p$outcome,
      fielding = p$fielding %||% NA_character_,
      rbi = as.integer(p$rbi %||% 0L),
      outs_on_play = as.integer(p$outs_on_play %||% 0L),
      reached = reached,
      advances = advances,
      pa_index_in_half = state$pa_counter[[team]],
      outs_before = as.integer(outs_before),
      bases_after = list(
        first = bases$first,
        second = bases$second,
        third = bases$third
      )
    ))
  )

  state$batting_index[[team]] <- state$batting_index[[team]] + 1L
  state <- reset_count(state)
  cap_ends <- isTRUE(cr$cap_hit) && isTRUE(state$ruleset$run_cap$cap_ends_half)
  if (state$outs >= 3L || cap_ends) {
    state <- advance_half(state)
  } else {
    state <- .set_current_batter(state)
  }
  state
}

suggest_advances <- function(state, outcome) {
  b <- state$bases
  occ <- c(
    first = !is.na(b$first),
    second = !is.na(b$second),
    third = !is.na(b$third)
  )
  adv <- list()
  push <- function(id, from, to, scored = FALSE) {
    adv[[length(adv) + 1]] <<- make_advance(id, from, to, scored = scored)
  }

  bump <- switch(
    outcome,
    "1B" = 1L,
    "2B" = 2L,
    "3B" = 3L,
    "HR" = 4L,
    "ITPHR" = 4L,
    "BB" = 1L,
    "IBB" = 1L,
    "HBP" = 1L,
    0L
  )
  if (bump == 0L) {
    return(adv)
  } # outs: no automatic advance suggestion

  is_walk <- outcome %in% c("BB", "IBB", "HBP")
  # Runners advance by `bump` bases on hits; on walks only forced runners move.
  if (occ["third"]) {
    to <- if (is_walk) {
      (if (occ["second"] && occ["first"]) 4L else 3L)
    } else {
      min(4L, 3L + bump)
    }
    push(b$third, 3L, to, scored = to >= 4L)
  }
  if (occ["second"]) {
    to <- if (is_walk) (if (occ["first"]) 3L else 2L) else min(4L, 2L + bump)
    push(b$second, 2L, to, scored = to >= 4L)
  }
  if (occ["first"]) {
    to <- min(4L, 1L + bump)
    push(b$first, 1L, to, scored = to >= 4L)
  }
  # The batter's own advance (to `bump` bases), used to pre-fill the UI.
  if (!is.null(state$current_batter)) {
    push(state$current_batter$player_id, 0L, bump, scored = bump >= 4L)
  }
  adv
}

apply_substitution <- function(state, evt) {
  p <- evt$payload
  team <- p$team
  inp <- p$in_player
  if (p$kind == "batting") {
    lineup <- state$lineups[[team]]
    for (i in seq_along(lineup)) {
      if (identical(lineup[[i]]$order_slot, as.integer(p$order_slot))) {
        # Rebuild through make_player() so every field is present and typed, and the
        # incoming player takes the slot it replaced while keeping its own position.
        lineup[[i]] <- make_player(
          inp$player_id,
          inp$name,
          inp$gender,
          jersey_number = inp$jersey_number,
          order_slot = as.integer(p$order_slot),
          position = inp$position
        )
      }
    }
    state$lineups[[team]] <- lineup
    state <- .set_current_batter(state)
  } else if (p$kind == "defensive") {
    lineup <- state$lineups[[team]]
    for (i in seq_along(lineup)) {
      if (identical(lineup[[i]]$player_id, p$out_player_id)) {
        # The batting-order slot stays with the lineup spot; only the fielder changes.
        lineup[[i]] <- make_player(
          inp$player_id,
          inp$name,
          inp$gender,
          jersey_number = inp$jersey_number,
          order_slot = lineup[[i]]$order_slot,
          position = p$position
        )
      }
    }
    state$lineups[[team]] <- lineup
  } else if (p$kind == "courtesy_runner") {
    for (b in c("first", "second", "third")) {
      if (!is.na(state$bases[[b]]) && state$bases[[b]] == p$out_player_id) {
        state$bases[[b]] <- p$in_player$player_id
      }
    }
    # The pinch runner inherits the origin of the batter they run for, so the run
    # is still credited to the correct scorebook cell.
    origin <- state$runner_origin %||% list()
    if (!is.null(origin[[p$out_player_id]])) {
      origin[[p$in_player$player_id]] <- origin[[p$out_player_id]]
      origin[[p$out_player_id]] <- NULL
      state$runner_origin <- origin
    }
    state$pinch_runner_log <- c(
      state$pinch_runner_log %||% list(),
      list(list(
        inning = state$inning,
        half = state$half,
        team = p$team %||% state$batting_team,
        out_player_id = p$out_player_id,
        in_player_id = p$in_player$player_id
      ))
    )
  }
  state
}
