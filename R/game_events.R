EVENT_TYPES <- c("game_start", "plate_appearance", "substitution",
                 "count_override", "inning_end")

new_event <- function(type, payload, seq = NA_integer_, ts = NA_character_) {
  list(seq = as.integer(seq), type = type, ts = ts, payload = payload %||% list())
}

make_player <- function(player_id, name, gender,
                        jersey_number = NA_integer_, order_slot = NA_integer_,
                        position = NA_integer_) {
  list(player_id = player_id, name = name, gender = gender,
       jersey_number = as.integer(jersey_number),
       order_slot = as.integer(order_slot),
       position = if (is.na(position)) NA_integer_ else as.integer(position))
}

make_advance <- function(runner_id, from, to, scored = FALSE, out = FALSE, earned = TRUE) {
  list(runner_id = runner_id, from = as.integer(from), to = as.integer(to),
       scored = isTRUE(scored), out = isTRUE(out), earned = isTRUE(earned))
}

validate_event <- function(evt) {
  errors <- character()
  add <- function(m) errors <<- c(errors, m)
  if (!evt$type %in% EVENT_TYPES) add(paste("unknown event type:", evt$type))
  if (identical(evt$type, "plate_appearance")) {
    o <- evt$payload$outcome
    if (is.null(o) || !o %in% c(APP_CONFIG$outcome_codes)) add(paste("bad outcome:", o %||% "NULL"))
    if (!evt$payload$team %in% c("home", "away")) add("plate_appearance needs team home/away")
  }
  if (identical(evt$type, "substitution")) {
    if (!evt$payload$kind %in% c("batting", "defensive", "courtesy_runner")) add("bad sub kind")
  }
  list(ok = length(errors) == 0, errors = errors)
}
