library(testthat)
library(shiny)
for (f in c(
  "app_config.R",
  "rules_engine.R",
  "game_events.R",
  "game_reducer.R",
  "rule_presets.R",
  "lineup_validation.R",
  "setup_module.R"
)) {
  source(file.path("R", f))
}

# setup_module.R is sourced into the global environment, so a global binding of
# this name shadows shiny's for every call made from inside the module -- which
# is how the tests below read back what the user was actually told.
.notices <- character()
showNotification <- function(ui, ..., type = "default", duration = NULL) {
  .notices <<- c(.notices, paste(as.character(ui), collapse = " "))
  invisible(NULL)
}
reset_notices <- function() .notices <<- character()

test_that("build_game_start_event assembles a valid event", {
  home <- list(
    team_id = "H",
    name = "Home",
    lineup = list(make_player("h1", "H1", "M", 1L, 1L, 6L))
  )
  away <- list(
    team_id = "A",
    name = "Away",
    lineup = list(make_player("a1", "A1", "F", 1L, 1L, 4L))
  )
  evt <- build_game_start_event(default_ruleset_config(), home, away, "away")
  expect_equal(evt$type, "game_start")
  expect_equal(evt$payload$first_bat, "away")
  expect_true(validate_event(evt)$ok)
})

test_that("collect_lineup reads rows, skips blanks, assigns order_slot", {
  input <- list(
    t_name_1 = "Sam",
    t_gender_1 = "F",
    t_jersey_1 = 9,
    t_pos_1 = "SS",
    t_name_2 = "",
    t_gender_2 = "M",
    t_jersey_2 = "",
    t_pos_2 = "", # blank name -> skipped
    t_name_3 = "Mo",
    t_gender_3 = "M",
    t_jersey_3 = "",
    t_pos_3 = "" # blank jersey string (what
    # textInput actually submits) -> NA
  )
  lu <- collect_lineup(input, "t", c(1, 2, 3))
  expect_equal(length(lu), 2L)
  expect_equal(lu[[1]]$name, "Sam")
  expect_equal(lu[[1]]$order_slot, 1L)
  expect_equal(lu[[1]]$position, "SS")
  expect_equal(lu[[1]]$jersey_number, 9L)
  expect_equal(lu[[2]]$name, "Mo")
  expect_equal(lu[[2]]$order_slot, 2L)
  expect_true(is.na(lu[[2]]$jersey_number))
  expect_true(is.na(lu[[2]]$position))
})

test_that("a jersey persisted as NA (not the live textInput blank string) is still NA", {
  input <- list(
    t_name_1 = "Sam",
    t_gender_1 = "F",
    t_jersey_1 = NA,
    t_pos_1 = ""
  )
  lu <- collect_lineup(input, "t", 1)
  expect_true(is.na(lu[[1]]$jersey_number))
})

test_that("a non-numeric jersey becomes NA without a warning", {
  input <- list(
    t_name_1 = "Sam",
    t_gender_1 = "F",
    t_jersey_1 = "oops",
    t_pos_1 = ""
  )
  expect_silent(lu <- collect_lineup(input, "t", 1))
  expect_true(is.na(lu[[1]]$jersey_number))
})

test_that("a jersey entered as a digit string is read as a number", {
  input <- list(
    t_name_1 = "Sam",
    t_gender_1 = "F",
    t_jersey_1 = "07",
    t_pos_1 = ""
  )
  lu <- collect_lineup(input, "t", 1)
  expect_equal(lu[[1]]$jersey_number, 7L)
})

test_that("collect_lineup defaults gender when the column is not rendered", {
  input <- list(t_name_1 = "Sam", t_jersey_1 = "9", t_pos_1 = "SS") # no t_gender_1
  lu <- collect_lineup(input, "t", 1, show_gender = FALSE)
  expect_equal(lu[[1]]$name, "Sam")
  expect_equal(lu[[1]]$gender, "M")
})

# A fake `input` whose `[[` throws if asked for a gender key. Used to prove
# collect_lineup(..., show_gender = FALSE) never *dereferences* the gender input
# at all -- not merely that a missing key happens to fall through to "M" via `%||%`.
`[[.poisoned_input` <- function(x, name) {
  if (grepl("_gender_", name, fixed = TRUE)) {
    stop("gender key must not be read when show_gender = FALSE")
  }
  unclass(x)[[name]]
}

test_that("collect_lineup never dereferences the gender input when show_gender = FALSE", {
  input <- structure(
    list(t_name_1 = "Sam", t_jersey_1 = "9", t_pos_1 = "SS", t_gender_1 = "F"),
    class = "poisoned_input"
  )
  # Sanity check the poison actually fires: reading gender (show_gender = TRUE) errors.
  expect_error(
    collect_lineup(input, "t", 1, show_gender = TRUE),
    "gender key must not be read"
  )
  # The real assertion: show_gender = FALSE must never touch the gender key.
  lu <- collect_lineup(input, "t", 1, show_gender = FALSE)
  expect_equal(lu[[1]]$gender, "M")
})

test_that("the lineup table renders a real table with the expected headers", {
  ns <- shiny::NS("setup")
  html <- as.character(.lineup_table_head(show_gender = TRUE))
  for (h in c("#", "Name", "Gender", "Jersey", "Position")) {
    expect_true(grepl(paste0(">", h, "<"), html, fixed = TRUE), info = h)
  }
})

test_that("the gender column disappears for a genderless ruleset", {
  html <- as.character(.lineup_table_head(show_gender = FALSE))
  expect_false(grepl(">Gender<", html, fixed = TRUE))
  expect_true(grepl(">Name<", html, fixed = TRUE))
})

test_that("a player row is a tr with cells and keeps its input ids", {
  ns <- shiny::NS("setup")
  html <- as.character(.player_row(
    ns,
    "away",
    3L,
    order = 2L,
    show_gender = TRUE
  ))
  expect_true(grepl("^<tr", html))
  expect_true(grepl('id="setup-away_name_3"', html, fixed = TRUE))
  expect_true(grepl('id="setup-away_pos_3"', html, fixed = TRUE))
  expect_true(grepl('id="setup-away_gender_3"', html, fixed = TRUE))
})

test_that("a genderless player row omits the gender input entirely", {
  ns <- shiny::NS("setup")
  html <- as.character(.player_row(
    ns,
    "away",
    3L,
    order = 1L,
    show_gender = FALSE
  ))
  expect_false(grepl("away_gender_3", html, fixed = TRUE))
  expect_true(grepl('id="setup-away_name_3"', html, fixed = TRUE))
})

test_that(".lineup_ui puts the row-container id on a <tbody>, so insertUI appends real rows", {
  ns <- shiny::NS("setup")
  html <- as.character(.lineup_ui(
    ns,
    "away",
    "Away lineup",
    show_gender = TRUE
  ))
  expect_true(
    grepl('<tbody id="setup-away_rows">', html, fixed = TRUE),
    info = "row-container id must be on the <tbody>, not a wrapping <div>"
  )
  # It must be a <tbody>, i.e. nested inside a <table>, not a bare element.
  expect_true(grepl("<table[^>]*>[\\s\\S]*<tbody", html, perl = TRUE))
})

test_that("collect_lineup returns an empty list when no rows have names", {
  expect_equal(length(collect_lineup(list(), "t", integer())), 0L)
})

test_that("build_game_start_event accepts an empty lineup (run-only team)", {
  home <- list(team_id = "H", name = "Home", lineup = list()) # empty
  away <- list(
    team_id = "A",
    name = "Away",
    lineup = list(make_player("a1", "A1", "F", 1L, 1L, "SS"))
  )
  evt <- build_game_start_event(default_ruleset_config(), home, away, "away")
  expect_true(validate_event(evt)$ok)
  expect_equal(length(evt$payload$home$lineup), 0L)
})

test_that("setup_server produces a game_start with a run-only home team", {
  library(shiny)
  testServer(setup_server, {
    session$flushReact() # warm-up: see Task 16 note re
    # testServer's ignoreInit-observer-fires-on-first-setInputs quirk (test-harness
    # artifact only; not a production bug)
    session$setInputs(away_add = 1) # one away row
    # Fill the away row (id 1), leave home empty, set required rule inputs, start.
    # The mercy inputs are the real per-tier ids (mercy_diff_1..3 / mercy_after_1..3);
    # an earlier version of this test set a nonexistent `mercy_diff`, which
    # collect_ruleset never reads -- a silent no-op that would have stayed green with
    # mercy handling deleted outright. Tier 2 is armed so it must appear in the event.
    session$setInputs(
      away_name_1 = "Sam",
      away_gender_1 = "F",
      away_jersey_1 = 9,
      away_pos_1 = "SS",
      away_name = "Away",
      home_name = "Home",
      start_balls = 1,
      start_strikes = 1,
      foul_out = "out",
      batting_size = "0",
      gender_rule = "none",
      innings = 7,
      run_cap = 0,
      fielding_preset = "none",
      mercy_after_1 = 3,
      mercy_diff_1 = 0,
      mercy_after_2 = 4,
      mercy_diff_2 = 12,
      mercy_after_3 = 5,
      mercy_diff_3 = 0,
      start = 1
    )
    gs <- session$returned()
    expect_equal(gs$type, "game_start")
    expect_equal(length(gs$payload$away$lineup), 1L)
    expect_equal(length(gs$payload$home$lineup), 0L) # run-only home
    expect_equal(
      gs$payload$ruleset$mercy_rule$tiers,
      list(list(after_inning = 4L, differential = 12L))
    )
  })
})

test_that("collect_ruleset coerces batting_size from the select string", {
  base_in <- list(
    fielding_preset = "none",
    start_balls = 1,
    start_strikes = 1,
    foul_out = "out",
    gender_rule = "none",
    gender_n = 2,
    innings = 7,
    run_cap = 0
  )
  expect_true(is.na(
    collect_ruleset(c(base_in, list(batting_size = "0")))$batting_size
  ))
  expect_equal(
    collect_ruleset(c(base_in, list(batting_size = "9")))$batting_size,
    9L
  )
  expect_equal(
    collect_ruleset(c(base_in, list(batting_size = "10")))$batting_size,
    10L
  )
})

# --- Finding 2: a cleared numeric box must produce a message, not a dead session ---

start_with_inputs <- function(extra) {
  reset_notices()
  returned <- NULL
  testServer(setup_server, {
    session$flushReact()
    session$setInputs(away_add = 1)
    args <- list(
      away_name_1 = "Sam",
      away_gender_1 = "F",
      away_jersey_1 = 9,
      away_pos_1 = "SS",
      away_name = "Away",
      home_name = "Home",
      start_balls = 0,
      start_strikes = 0,
      foul_out = "unlimited",
      batting_size = "0",
      gender_rule = "none",
      innings = 7,
      fielder_count = 0,
      run_cap = 0,
      fielding_preset = "none",
      mercy_after_1 = 3,
      mercy_diff_1 = 0,
      mercy_after_2 = 4,
      mercy_diff_2 = 0,
      mercy_after_3 = 5,
      mercy_diff_3 = 0
    )
    args[names(extra)] <- extra # overrides, not duplicates
    args$start <- 1
    do.call(session$setInputs, args)
    returned <<- session$returned()
  })
  list(event = returned, notices = .notices)
}

test_that("a cleared N on a gender rule reports the reason instead of killing the session", {
  # build_game_start_event() computes exactly the right message and then stop()s
  # with it inside an observer with no handler, so Shiny tore the session down
  # with a generic "An error has occurred" and the message went to the log.
  r <- start_with_inputs(list(
    gender_rule = "max_consecutive_males",
    gender_n = NA
  ))
  expect_null(r$event)
  expect_true(
    any(grepl(
      "max_consecutive_males batting rule requires n",
      r$notices,
      fixed = TRUE
    )),
    info = paste(r$notices, collapse = " | ")
  )
})

test_that("a cleared Innings / balls / strikes box reports the reason too", {
  # These are worse: validate_ruleset_config() ITSELF threw on the NA before it
  # could produce a message at all.
  cases <- list(
    list(inputs = list(innings = NA), want = "innings must be >= 1"),
    list(inputs = list(start_balls = NA), want = "starting balls must be 0-3"),
    list(
      inputs = list(start_strikes = NA),
      want = "starting strikes must be 0-2"
    )
  )
  for (case in cases) {
    r <- start_with_inputs(case$inputs)
    expect_null(r$event, info = case$want)
    expect_true(
      any(grepl(case$want, r$notices, fixed = TRUE)),
      info = paste(case$want, "::", paste(r$notices, collapse = " | "))
    )
  }
})

test_that("a valid start still returns an event and raises no error notice", {
  r <- start_with_inputs(list())
  expect_equal(r$event$type, "game_start")
  expect_false(any(grepl("Invalid ruleset", r$notices, fixed = TRUE)))
})

# --- Finding 3: a cleared fielding-gender box must not brick the game ---

test_that("a cleared fielding-gender box cannot produce an unloadable game_start", {
  # min_females / of_females / if_females were read with a bare %||%, so a cleared
  # box persisted NA into game_start. validate_ruleset_config validates nothing
  # under `fielding`, so Start game happily built the event -- and from then on
  # every .refresh_flags() threw inside evaluate_fielding(). Because loading is
  # fold_events(events()), the game could never be opened again.
  cfg <- collect_ruleset(list(
    fielding_preset = "custom",
    min_females = NA,
    max_males = NA,
    of_females = NA,
    if_females = NA,
    battery_mode = "any",
    fielder_count = NA,
    start_balls = 0,
    start_strikes = 0,
    foul_out = "unlimited",
    batting_size = "0",
    gender_rule = "none",
    innings = 7,
    run_cap = 0
  ))
  expect_true(validate_ruleset_config(cfg)$ok)

  lineup <- list(
    make_player("a1", "A1", "F", 1L, 1L, "P"),
    make_player("a2", "A2", "M", 2L, 2L, "C")
  )
  evt <- build_game_start_event(
    cfg,
    list(team_id = "H", name = "Home", lineup = lineup),
    list(team_id = "A", name = "Away", lineup = lineup),
    "away"
  )
  s <- fold_events(list(evt)) # must not error: this is the load path
  expect_equal(s$status, "in_progress")
  expect_equal(evaluate_fielding(cfg, lineup), list())
})

test_that("a zeroed fielding-gender box still means zero, not unlimited-by-NA", {
  cfg <- collect_ruleset(list(
    fielding_preset = "custom",
    min_females = 0,
    max_males = 0,
    of_females = 0,
    if_females = 0,
    battery_mode = "any",
    fielder_count = 0,
    start_balls = 0,
    start_strikes = 0,
    foul_out = "unlimited",
    batting_size = "0",
    gender_rule = "none",
    innings = 7,
    run_cap = 0
  ))
  expect_equal(cfg$fielding$min_females, 0L)
  expect_equal(cfg$fielding$tiers[[1]]$outfield, 0L)
  expect_equal(cfg$fielding$tiers[[1]]$infield, 0L)
})

# --- Finding 9: the persisted preset label must not be a lie ---

# Mirrors observeEvent(input$preset) in setup_server: the inputs the app writes
# when a preset is chosen. If the two ever drift, the round-trip below fails.
.na0 <- function(x) if (is.na(x)) 0L else x
preset_inputs <- function(id) {
  cfg <- preset_ruleset(id)
  ins <- list(
    preset = id,
    start_balls = cfg$starting_count$balls,
    start_strikes = cfg$starting_count$strikes,
    foul_out = cfg$foul_out_rule,
    batting_size = as.character(.na0(cfg$batting_size)),
    batting_size_rule = cfg$batting_size_rule,
    gender_rule = cfg$batting_gender_rule$type,
    gender_n = if (is.na(cfg$batting_gender_rule$n)) {
      1L
    } else {
      cfg$batting_gender_rule$n
    },
    innings = cfg$innings,
    fielder_count = .na0(cfg$fielding$fielder_count),
    run_cap = .na0(cfg$run_cap$per_inning),
    cap_same_play = cfg$run_cap$same_play_runs_count,
    cap_ends_half = cfg$run_cap$cap_ends_half,
    open_last = cfg$run_cap$open_last_inning,
    hr_limit = .na0(cfg$home_run_rule$over_fence_limit),
    hr_limit_m = cfg$home_run_rule$limit_by_gender$M %||% 0L,
    hr_limit_f = cfg$home_run_rule$limit_by_gender$F %||% 0L,
    hr_over = cfg$home_run_rule$over_limit_result,
    hr_itp_counts = cfg$home_run_rule$inside_park_counts,
    pr_inning = .na0(cfg$pinch_runner$max_per_inning),
    pr_game = .na0(cfg$pinch_runner$max_per_game),
    pr_player = .na0(cfg$pinch_runner$max_per_player_per_game),
    pr_elig = cfg$pinch_runner$eligibility,
    pr_for = cfg$pinch_runner$allowed_for,
    fielding_preset = if (length(cfg$fielding$tiers)) {
      "standard_coed"
    } else {
      "none"
    }
  )
  for (i in 1:3) {
    tiers <- cfg$mercy_rule$tiers
    t <- if (i <= length(tiers)) tiers[[i]] else NULL
    ins[[paste0("mercy_after_", i)]] <- if (is.null(t)) {
      c(3L, 4L, 5L)[i]
    } else {
      t$after_inning
    }
    ins[[paste0("mercy_diff_", i)]] <- if (is.null(t)) 0L else t$differential
  }
  ins
}

test_that("an untouched preset round-trips to its own id, not to custom", {
  for (id in names(RULE_PRESETS)) {
    got <- collect_ruleset(preset_inputs(id))
    expect_equal(got$preset, id, info = id)
    # And the collected config really is the preset, not merely labelled as it.
    want <- preset_ruleset(id)
    expect_equal(got, want, info = id)
  }
})

test_that("editing any exposed control flips the label to custom", {
  edits <- list(
    innings = 5,
    start_balls = 1,
    start_strikes = 1,
    foul_out = "out",
    batting_size = "10",
    gender_rule = "max_consecutive_same_gender",
    run_cap = 5,
    cap_same_play = FALSE,
    cap_ends_half = FALSE,
    open_last = FALSE,
    fielder_count = 11,
    hr_limit = 2,
    hr_limit_m = 1,
    hr_over = "single",
    hr_itp_counts = TRUE,
    pr_inning = 2,
    pr_game = 4,
    pr_player = 1,
    pr_elig = "last_out",
    pr_for = "pitcher_catcher",
    mercy_diff_1 = 20,
    fielding_preset = "standard_coed"
  )
  base <- preset_inputs("standard_baseball")
  for (k in names(edits)) {
    ins <- base
    ins[[k]] <- edits[[k]]
    expect_equal(collect_ruleset(ins)$preset, "custom", info = k)
  }
})

test_that("an unknown preset id is preserved rather than mislabelled", {
  ins <- preset_inputs("standard_baseball")
  ins$preset <- "some_saved_league_ruleset"
  expect_equal(collect_ruleset(ins)$preset, "some_saved_league_ruleset")
})

# --- Cleanup: ruleset()'s tryCatch was dead code because collect_ruleset never throws ---

test_that("collect_ruleset never throws, however empty or malformed the inputs", {
  # setup_server's `ruleset()` reactive used to wrap this in a tryCatch that could
  # never fire. Removing it is only safe while this invariant holds, so pin it.
  expect_no_error(collect_ruleset(list()))
  expect_no_error(collect_ruleset(list(preset = "nope_not_a_preset")))
  expect_no_error(collect_ruleset(list(
    innings = NA,
    start_balls = NA,
    start_strikes = NA,
    run_cap = NA,
    gender_rule = "min_females_per_n",
    gender_n = NA,
    batting_size = NA,
    fielding_preset = "custom",
    min_females = NA,
    foul_out = NA_character_
  )))
})
