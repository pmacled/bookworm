build_game_start_event <- function(ruleset, home, away, first_bat = "away") {
  ruleset <- coerce_ruleset_config(ruleset)
  v <- validate_ruleset_config(ruleset)
  if (!v$ok) stop(paste("Invalid ruleset:", paste(v$errors, collapse = "; ")))
  new_event("game_start", list(ruleset = ruleset, first_bat = first_bat,
    home = home, away = away), seq = 1L)
}

collect_lineup <- function(input, prefix, row_ids) {
  players <- list()
  for (id in row_ids) {
    nm <- input[[paste0(prefix, "_name_", id)]] %||% ""
    nm <- trimws(nm)
    if (!nzchar(nm)) next
    jersey <- input[[paste0(prefix, "_jersey_", id)]]
    jersey <- if (is.null(jersey) || is.na(jersey)) 0L else as.integer(jersey)
    pos <- input[[paste0(prefix, "_pos_", id)]] %||% ""
    pos <- if (!nzchar(pos)) NA_character_ else pos
    slot <- length(players) + 1L
    players[[slot]] <- make_player(uuid::UUIDgenerate(), nm,
      input[[paste0(prefix, "_gender_", id)]] %||% "M",
      jersey_number = jersey, order_slot = slot, position = pos)
  }
  players
}

collect_ruleset <- function(input) {
  fielding <- switch(input$fielding_preset %||% "none",
    "standard_coed" = STANDARD_COED_FIELDING,
    "custom" = list(
      min_females = input$min_females %||% 0L,
      max_males = if ((input$max_males %||% 0) > 0) input$max_males else NA_integer_,
      tiers = list(list(females = 0L,
        outfield = input$of_females %||% 0L, infield = input$if_females %||% 0L,
        battery = input$battery_mode %||% "any")),
      position_requirements = list()),
    list(min_females = 0L, max_males = NA_integer_, tiers = list(), position_requirements = list()))
  coerce_ruleset_config(list(
    starting_count = list(balls = input$start_balls, strikes = input$start_strikes),
    foul_out_rule = input$foul_out,
    batting_gender_rule = list(type = input$gender_rule, n = input$gender_n),
    batting_size = if ((input$batting_size %||% 0) > 0) input$batting_size else NA_integer_,
    fielding = fielding,
    innings = input$innings,
    run_cap_per_inning = if ((input$run_cap %||% 0) > 0) input$run_cap else NA_integer_,
    mercy_rule = list(differential = if ((input$mercy_diff %||% 0) > 0) input$mercy_diff else NA_integer_,
                      after_inning = 1L)))
}

setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("New game"),
    layout_columns(col_widths = c(6,6),
      numericInput(ns("start_balls"), "Starting balls", 1, 0, 3),
      numericInput(ns("start_strikes"), "Starting strikes", 1, 0, 2)),
    selectInput(ns("foul_out"), "Foul with 2 strikes",
      c("Out" = "out", "One courtesy foul" = "one_courtesy_foul")),
    selectInput(ns("gender_rule"), "Batting gender rule",
      c("None" = "none", "No two males in a row" = "no_two_males_consecutive",
        "Every other" = "every_other", "At least one F every N" = "every_n")),
    numericInput(ns("gender_n"), "N (for 'every N')", 2, 2, 12),
    numericInput(ns("min_females"), "Min females in field", 0, 0, 10),
    layout_columns(col_widths = c(4,4,4),
      numericInput(ns("innings"), "Innings", 7, 1, 12),
      numericInput(ns("run_cap"), "Run cap/inning (0 = none)", 0, 0, 30),
      numericInput(ns("mercy_diff"), "Mercy differential (0 = none)", 0, 0, 50)),
    textInput(ns("away_name"), "Away team", "Away"),
    textInput(ns("home_name"), "Home team", "Home"),
    tags$p(class="text-muted small",
      "Add lineups on the next screen; a quick default lineup is created for now."),
    actionButton(ns("start"), "Start game", class = "btn-primary bw-outcome-btn")
  )
}

setup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    game_start <- reactiveVal(NULL)
    observeEvent(input$start, {
      cfg <- coerce_ruleset_config(list(
        starting_count = list(balls = input$start_balls, strikes = input$start_strikes),
        foul_out_rule = input$foul_out,
        batting_gender_rule = list(type = input$gender_rule, n = input$gender_n),
        fielding = list(min_females = input$min_females, position_requirements = list()),
        innings = input$innings,
        run_cap_per_inning = if (input$run_cap > 0) input$run_cap else NA_integer_,
        mercy_rule = list(differential = if (input$mercy_diff > 0) input$mercy_diff else NA_integer_,
                          after_inning = 1L)))
      mk <- function(prefix)
        lapply(1:4, function(i) make_player(uuid::UUIDgenerate(),
          paste(prefix, i), c("M","F","M","F")[i], i, i, i))
      home <- list(team_id = uuid::UUIDgenerate(), name = input$home_name, lineup = mk("Home"))
      away <- list(team_id = uuid::UUIDgenerate(), name = input$away_name, lineup = mk("Away"))
      game_start(build_game_start_event(cfg, home, away, "away"))
    })
    game_start
  })
}
