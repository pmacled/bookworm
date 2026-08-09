# App-level wiring test: exercises bookworm_server's observers via testServer,
# covering the auth -> setup -> track navigation seam that unit tests don't reach.
library(testthat)
# Source global.R itself rather than re-implementing its R/ load order here: that
# order is a single source of truth (global.R's .r_first) instead of two copies that
# can drift apart -- e.g. rule_presets.R reads STANDARD_COED_FIELDING (from
# rules_engine.R) at source time, so any naive alphabetical sweep breaks.
suppressMessages(source("global.R"))

test_that("continue-as-guest wires storage and navigates without a nav error", {
  testServer(bookworm_server, {
    session$flushReact()
    session$setInputs(`auth-do_guest` = 1) # click "Continue as guest"
    # The identity observer must run to completion: it calls nav_select("screen",
    # "setup") — a bogus nav_hide() here previously crashed with "argument target
    # missing". If it still threw, store() would remain NULL.
    expect_false(is.null(store()))
    expect_true(is.function(store()$create_game))
    expect_true(is.function(store()$append_event))
  })
})

test_that("starting a game creates a game and advances to tracking", {
  testServer(bookworm_server, {
    session$flushReact()
    session$setInputs(`auth-do_guest` = 1)
    # Provide the setup form inputs, then click Start.
    session$setInputs(
      `setup-preset` = "anything_goes",
      `setup-start_balls` = 1,
      `setup-start_strikes` = 1,
      `setup-foul_out` = "out",
      `setup-gender_rule` = "none",
      `setup-gender_n` = 2,
      `setup-min_females` = 0,
      `setup-innings` = 7,
      `setup-fielder_count` = 0,
      `setup-run_cap` = 0,
      `setup-open_last` = TRUE,
      `setup-cap_same_play` = TRUE,
      `setup-cap_ends_half` = TRUE,
      `setup-mercy_diff_1` = 0,
      `setup-mercy_diff_2` = 0,
      `setup-mercy_diff_3` = 0,
      `setup-hr_limit` = 0,
      `setup-hr_limit_m` = 0,
      `setup-hr_limit_f` = 0,
      `setup-hr_over` = "out",
      `setup-hr_itp_counts` = FALSE,
      `setup-pr_inning` = 0,
      `setup-pr_game` = 0,
      `setup-pr_player` = 0,
      `setup-pr_elig` = "anyone",
      `setup-pr_for` = "anyone",
      `setup-away_name` = "Away",
      `setup-home_name` = "Home"
    )
    session$setInputs(`setup-start` = 1) # click "Start game"
    # The game_start observer must create a game and init tracking without error.
    lg <- store()$list_games()
    expect_equal(nrow(lg), 1L)
  })
})

test_that("an unrelated rules-panel edit does not re-render the lineup shells, but a rule change that flips genderless-ness does", {
  # testServer is server-only: it can't reproduce a browser losing typed input when
  # an output's HTML is replaced. The observable proxy is *how many times* the
  # lineup shell is actually regenerated -- renderUI(.lineup_ui(...)) calls
  # .lineup_ui() exactly once per invalidation, so counting calls to it stands in for
  # counting client-side re-renders.
  #
  # This drives the underlying gender_rule control directly rather than through
  # `setup-preset`: testServer's mock session does not simulate the client
  # round-trip that update*Input() relies on (choosing a preset sends update
  # messages, but nothing echoes the new value back into input$gender_rule the way
  # a real browser would), so `setup-preset` alone can never change collect_ruleset()
  # here. Setting the control it would have updated exercises the same debounce
  # logic in show_gender without depending on that untestable round-trip.
  assign(".lineup_render_count", 0L, envir = globalenv())
  trace(
    ".lineup_ui",
    tracer = quote(assign(
      ".lineup_render_count",
      get(".lineup_render_count", envir = globalenv()) + 1L,
      envir = globalenv()
    )),
    print = FALSE
  )
  on.exit(try(untrace(".lineup_ui"), silent = TRUE), add = TRUE)

  testServer(bookworm_server, {
    session$flushReact()
    session$setInputs(`auth-do_guest` = 1)
    session$setInputs(
      `setup-preset` = "anything_goes",
      `setup-gender_rule` = "none",
      `setup-fielding_preset` = "none"
    ) # genderless
    session$flushReact()
    n0 <- get(".lineup_render_count", envir = globalenv())
    expect_gt(n0, 0L) # sanity: the initial render actually happened

    # Typing in the rules panel (not touching gender-related fields) must not
    # invalidate the lineup shells.
    session$setInputs(`setup-start_balls` = 2)
    session$flushReact()
    expect_equal(get(".lineup_render_count", envir = globalenv()), n0)

    # A rule change that flips genderless-ness must re-render them.
    session$setInputs(
      `setup-gender_rule` = "max_consecutive_males",
      `setup-gender_n` = 2
    )
    session$flushReact()
    expect_gt(get(".lineup_render_count", envir = globalenv()), n0)
  })
})

test_that("the rendered lineup shell actually shows/hides the Gender column for each polarity", {
  # Pins the headline requirement at the server-rendered-output level (not just via the
  # pure .lineup_table_head()/.player_row() helpers, which take show_gender as a hand-passed
  # argument and would stay green even if setup_server() wired the reactive backwards).
  testServer(setup_server, {
    session$flushReact()
    session$setInputs(gender_rule = "none", fielding_preset = "none")
    session$flushReact()
    expect_false(grepl(">Gender<", output$away_lineup$html, fixed = TRUE))

    session$setInputs(gender_rule = "max_consecutive_males", gender_n = 2)
    session$flushReact()
    expect_true(grepl(">Gender<", output$away_lineup$html, fixed = TRUE))
  })
})

test_that("a ruleset change that flips genderless-ness clears stale rows so no phantom player reaches collect_lineup", {
  # Before the fix: output$away_lineup re-renders `.lineup_ui()` to an *empty* <tbody>
  # when show_gender flips, but rows$away() (and the row-scoped inputs) survived --
  # collect_lineup() would then resurrect the vanished rows as "M"-defaulted phantom
  # players: invisible in the UI, un-deletable, yet still saved and still sent to
  # game_start. This pins that the row list -- not just the on-screen table -- is
  # actually cleared.
  testServer(setup_server, {
    session$flushReact()
    session$setInputs(gender_rule = "none", fielding_preset = "none") # genderless
    session$flushReact()
    expect_false(isolate(show_gender()))

    session$setInputs(away_add = 1)
    session$flushReact()
    session$setInputs(away_add = 1)
    session$flushReact()
    session$setInputs(away_add = 1)
    session$flushReact()
    session$setInputs(
      away_name_1 = "A",
      away_jersey_1 = 1,
      away_pos_1 = "P",
      away_name_2 = "B",
      away_jersey_2 = 2,
      away_pos_2 = "C",
      away_name_3 = "C",
      away_jersey_3 = 3,
      away_pos_3 = "SS"
    )
    session$flushReact()
    expect_equal(length(isolate(rows$away())), 3L)

    # Flip to a gendered ruleset -- the historical bug scenario.
    session$setInputs(gender_rule = "max_consecutive_males", gender_n = 2)
    session$flushReact()
    expect_true(isolate(show_gender()))

    expect_equal(length(isolate(rows$away())), 0L)
    lu <- collect_lineup(
      input,
      "away",
      isolate(rows$away()),
      show_gender = isolate(show_gender())
    )
    expect_equal(length(lu), 0L)
  })
})

test_that("after starting a game, it appears in the home games list", {
  testServer(bookworm_server, {
    session$flushReact()
    session$setInputs(`auth-do_guest` = 1)
    session$setInputs(
      `setup-preset` = "anything_goes",
      `setup-start_balls` = 1,
      `setup-start_strikes` = 1,
      `setup-foul_out` = "out",
      `setup-gender_rule` = "none",
      `setup-gender_n` = 2,
      `setup-min_females` = 0,
      `setup-innings` = 7,
      `setup-fielder_count` = 0,
      `setup-run_cap` = 0,
      `setup-open_last` = TRUE,
      `setup-cap_same_play` = TRUE,
      `setup-cap_ends_half` = TRUE,
      `setup-mercy_diff_1` = 0,
      `setup-mercy_diff_2` = 0,
      `setup-mercy_diff_3` = 0,
      `setup-hr_limit` = 0,
      `setup-hr_limit_m` = 0,
      `setup-hr_limit_f` = 0,
      `setup-hr_over` = "out",
      `setup-hr_itp_counts` = FALSE,
      `setup-pr_inning` = 0,
      `setup-pr_game` = 0,
      `setup-pr_player` = 0,
      `setup-pr_elig` = "anyone",
      `setup-pr_for` = "anyone",
      `setup-away_name` = "Away",
      `setup-home_name` = "Home"
    )
    session$setInputs(`setup-start` = 1)
    # The guest storage is shared across the top-level server and the home
    # module, so the freshly created game is visible via list_games().
    expect_equal(nrow(store()$list_games()), 1L)
  })
})
