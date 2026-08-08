# Pinch- / courtesy-runner allowances. Pure: returns errors, never mutates state.
# The two concepts are unified — softball's "courtesy runner" and baseball's
# "pinch runner" are the same substitution as far as the scorebook is concerned.

# Batter ids of outs recorded by `team`, most recent first.
.recent_outs <- function(state, team) {
  outs <- Filter(function(r)
    identical(r$team, team) && as.integer(r$outs_on_play %||% 0L) > 0L,
    state$pa_log %||% list())
  rev(vapply(outs, function(r) r$batter_id %||% NA_character_, character(1)))
}

.gender_of <- function(state, team, player_id) {
  hit <- Filter(function(p) identical(p$player_id, player_id), state$lineups[[team]] %||% list())
  if (length(hit)) hit[[1]]$gender else NA_character_
}

evaluate_pinch_runner <- function(cfg, state, out_player, in_player) {
  pr <- cfg$pinch_runner
  team <- state$batting_team
  errors <- character()
  add <- function(m) errors <<- c(errors, m)

  log <- state$pinch_runner_log %||% list()
  for_team <- Filter(function(e) identical(e$team, team), log)

  n_inning <- sum(vapply(for_team, function(e)
    identical(e$inning, state$inning) && identical(e$half, state$half), logical(1)))
  if (!is.na(pr$max_per_inning) && n_inning >= pr$max_per_inning)
    add(sprintf("Only %d pinch runner(s) allowed per inning; %d already used.",
                pr$max_per_inning, n_inning))

  if (!is.na(pr$max_per_game) && length(for_team) >= pr$max_per_game)
    add(if (pr$max_per_game == 0L) "Pinch runners are not allowed under this ruleset."
        else sprintf("Only %d pinch runner(s) allowed per game; %d already used.",
                     pr$max_per_game, length(for_team)))

  n_player <- sum(vapply(for_team,
    function(e) identical(e$in_player_id, in_player$player_id), logical(1)))
  if (!is.na(pr$max_per_player_per_game) && n_player >= pr$max_per_player_per_game)
    add(sprintf("%s has already pinch run %d time(s) this game.", in_player$name, n_player))

  if (identical(pr$allowed_for, "pitcher_catcher") &&
      !isTRUE(as.character(out_player$position) %in% c("P", "C")))
    add("Only the pitcher or catcher may have a courtesy runner under this ruleset.")

  elig <- pr$eligibility
  if (identical(elig, "same_gender") && !identical(in_player$gender, out_player$gender))
    add(sprintf("The runner must be the same gender as %s.", out_player$name))

  if (elig %in% c("last_out", "last_same_gender_out")) {
    outs <- .recent_outs(state, team)
    if (identical(elig, "last_same_gender_out")) {
      want <- out_player$gender
      outs <- Filter(function(id) identical(.gender_of(state, team, id), want), outs)
    }
    expected <- if (length(outs)) outs[[1]] else NA_character_
    if (is.na(expected))
      add("No eligible previous out to run for yet.")
    else if (!identical(in_player$player_id, expected))
      add(sprintf("The runner must be the last %sout (%s).",
                  if (identical(elig, "last_same_gender_out")) "same-gender " else "",
                  expected))
  }

  list(ok = length(errors) == 0, errors = errors)
}
