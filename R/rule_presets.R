# Named rule presets. Each `config` is a partial ruleset merged over the defaults by
# preset_ruleset(); anything not named here takes its default_ruleset_config() value.

.USA_MERCY <- list(
  list(after_inning = 3L, differential = 20L),
  list(after_inning = 4L, differential = 15L),
  list(after_inning = 5L, differential = 10L)
)

# GameOn Summer and Spring are identical apart from the starting count, so the shared
# body lives here once.
.GAMEON_BASE <- list(
  foul_out_rule = "one_courtesy_foul",
  batting_gender_rule = list(type = "max_consecutive_males", n = 2L),
  innings = 7L,
  fielding = utils::modifyList(
    STANDARD_COED_FIELDING,
    list(fielder_count = 10L)
  ),
  home_run_rule = list(
    over_fence_limit = 3L,
    over_limit_result = "out",
    inside_park_counts = FALSE
  ),
  pinch_runner = list(max_per_inning = 1L, eligibility = "same_gender")
)

RULE_PRESETS <- list(
  anything_goes = list(
    id = "anything_goes",
    label = "Anything Goes",
    description = "Genderless default. 0-0 count, unlimited fouls, everyone bats, 7 innings, no caps or limits.",
    config = list()
  ),

  standard_baseball = list(
    id = "standard_baseball",
    label = "Standard Baseball",
    description = "9 innings, 9 fielders, 9 batters, unlimited fouls, no run cap or mercy rule.",
    config = list(
      innings = 9L,
      batting_size = 9L,
      batting_size_rule = "exact",
      fielding = list(fielder_count = 9L)
    )
  ),

  standard_slowpitch = list(
    id = "standard_slowpitch",
    label = "Standard Slowpitch Softball",
    description = "7 innings, 10 fielders, everyone bats, a foul with two strikes is an out, USA Softball mercy schedule.",
    config = list(
      innings = 7L,
      foul_out_rule = "out",
      fielding = list(fielder_count = 10L),
      mercy_rule = list(tiers = .USA_MERCY)
    )
  ),

  standard_fastpitch = list(
    id = "standard_fastpitch",
    label = "Standard Fastpitch Softball",
    description = "7 innings, 9 fielders, 9 batters, unlimited fouls, USA Softball mercy schedule, courtesy runner for the pitcher or catcher only.",
    config = list(
      innings = 7L,
      batting_size = 9L,
      batting_size_rule = "exact",
      fielding = list(fielder_count = 9L),
      mercy_rule = list(tiers = .USA_MERCY),
      pinch_runner = list(allowed_for = "pitcher_catcher")
    )
  ),

  gameon_summer = list(
    id = "gameon_summer",
    label = "GameOn Summer",
    description = "Coed: 0-0 count, one courtesy foul, no three males in a row, standard coed fielding, 3 home runs, one same-gender courtesy runner per inning.",
    config = utils::modifyList(
      .GAMEON_BASE,
      list(starting_count = list(balls = 0L, strikes = 0L))
    )
  ),

  gameon_spring = list(
    id = "gameon_spring",
    label = "GameOn Spring",
    description = "Coed: 1-1 count, one courtesy foul, no three males in a row, standard coed fielding, 3 home runs, one same-gender courtesy runner per inning.",
    config = utils::modifyList(
      .GAMEON_BASE,
      list(starting_count = list(balls = 1L, strikes = 1L))
    )
  )
)

preset_ruleset <- function(id) {
  p <- RULE_PRESETS[[id]]
  if (is.null(p)) {
    stop(sprintf("unknown preset: %s", id))
  }
  cfg <- coerce_ruleset_config(p$config)
  cfg$preset <- id
  cfg
}

# Named vector for selectInput: names are labels, values are ids.
preset_choices <- function() {
  stats::setNames(
    vapply(RULE_PRESETS, function(p) p$id, character(1)),
    vapply(RULE_PRESETS, function(p) p$label, character(1))
  )
}
