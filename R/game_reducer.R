initial_game_state <- function(ruleset = default_ruleset_config()) {
  ruleset <- coerce_ruleset_config(ruleset)
  list(
    status = "in_progress", inning = 1L, half = "top", outs = 0L,
    count = list(balls = ruleset$starting_count$balls,
                 strikes = ruleset$starting_count$strikes),
    bases = list(first = NA_character_, second = NA_character_, third = NA_character_),
    score = list(home = 0L, away = 0L), runs_this_half = 0L,
    lineups = list(home = list(), away = list()),
    batting_index = list(home = 0L, away = 0L),
    batting_team = "away", current_batter = NULL,
    pa_log = list(), line_score = list(home = integer(), away = integer()),
    warnings = character(), ruleset = ruleset
  )
}

reset_count <- function(state) {
  state$count <- list(balls = state$ruleset$starting_count$balls,
                      strikes = state$ruleset$starting_count$strikes)
  state
}

.set_current_batter <- function(state) {
  team <- state$batting_team
  lineup <- state$lineups[[team]]
  if (length(lineup) == 0) { state$current_batter <- NULL; return(state) }
  batters <- Filter(function(p) !is.na(p$order_slot), lineup)
  batters <- batters[order(vapply(batters, function(p) p$order_slot, integer(1)))]
  idx <- (state$batting_index[[team]] %% length(batters))
  state$current_batter <- batters[[idx + 1]]
  state
}

advance_half <- function(state) {
  # record runs for the completed half into the line score
  state$line_score[[state$batting_team]] <-
    c(state$line_score[[state$batting_team]], state$runs_this_half)
  if (identical(state$half, "top")) {
    state$half <- "bottom"; state$batting_team <- "home"
  } else {
    state$half <- "top"; state$batting_team <- "away"; state$inning <- state$inning + 1L
  }
  state$outs <- 0L
  state$runs_this_half <- 0L
  state$bases <- list(first = NA_character_, second = NA_character_, third = NA_character_)
  state <- reset_count(state)
  state <- .set_current_batter(state)
  state
}

apply_event <- function(state, evt) {
  type <- evt$type
  if (type == "game_start") {
    p <- evt$payload
    state <- initial_game_state(p$ruleset %||% state$ruleset)
    state$lineups$home <- p$home$lineup
    state$lineups$away <- p$away$lineup
    state$teams <- list(home = p$home[c("team_id","name")], away = p$away[c("team_id","name")])
    state$batting_team <- p$first_bat %||% "away"
    state <- .set_current_batter(state)
    return(state)
  }
  if (type == "count_override") {
    state$count <- list(balls = as.integer(evt$payload$balls),
                        strikes = as.integer(evt$payload$strikes))
    return(state)
  }
  if (type == "inning_end") return(advance_half(state))
  if (type == "plate_appearance") {
    # Full advance/scoring logic added in Task 5; core handles outs + turn here.
    state <- apply_plate_appearance(state, evt)   # defined in Task 5
    return(state)
  }
  if (type == "substitution") return(apply_substitution(state, evt))  # Task 7
  state
}

fold_events <- function(events, ruleset = NULL) {
  state <- initial_game_state(ruleset %||% default_ruleset_config())
  for (evt in events) state <- apply_event(state, evt)
  state
}

apply_plate_appearance <- function(state, evt) {
  p <- evt$payload
  state$outs <- state$outs + as.integer(p$outs_on_play %||% 0L)
  # advance the batting order and reset the count for the next batter
  team <- state$batting_team
  state$batting_index[[team]] <- state$batting_index[[team]] + 1L
  state <- reset_count(state)
  if (state$outs >= 3L) state <- advance_half(state) else state <- .set_current_batter(state)
  state
}
apply_substitution <- function(state, evt) state  # replaced in Task 7
