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

.lineup_ui <- function(ns, prefix, title) {
  tagList(
    tags$h5(title),
    tags$p(class = "text-muted small",
      "Leave this lineup empty to just record this team's runs each inning."),
    tags$div(id = ns(paste0(prefix, "_rows"))),
    actionButton(ns(paste0(prefix, "_add")), "Add player", class = "btn-sm btn-outline-secondary")
  )
}

setup_ui <- function(id) {
  ns <- NS(id)
  pos_choices <- c("(no position)" = "", stats::setNames(APP_CONFIG$positions, APP_CONFIG$positions))
  tagList(
    tags$h3("New game"),
    textInput(ns("away_name"), "Away team", "Away"),
    .lineup_ui(ns, "away", "Away lineup"),
    textInput(ns("home_name"), "Home team", "Home"),
    .lineup_ui(ns, "home", "Home lineup"),
    accordion(open = FALSE,
      accordion_panel("Rules",
        layout_columns(col_widths = c(6,6),
          numericInput(ns("start_balls"), "Starting balls", 1, 0, 3),
          numericInput(ns("start_strikes"), "Starting strikes", 1, 0, 2)),
        selectInput(ns("foul_out"), "Foul with 2 strikes",
          c("Out" = "out", "One courtesy foul" = "one_courtesy_foul",
            "Unlimited (never an out)" = "unlimited")),
        selectInput(ns("batting_size"), "Number of batters",
          c("Unlimited (everyone bats)" = "0", "9" = "9", "10" = "10")),
        selectInput(ns("gender_rule"), "Batting gender rule",
          c("None" = "none", "No two males in a row" = "no_two_males_consecutive",
            "Every other" = "every_other", "At least one F every N" = "every_n")),
        conditionalPanel(sprintf("input['%s'] == 'every_n'", ns("gender_rule")),
          numericInput(ns("gender_n"), "N (for 'every N')", 2, 2, 12)),
        layout_columns(col_widths = c(4,4,4),
          numericInput(ns("innings"), "Innings", 7, 1, 12),
          numericInput(ns("run_cap"), "Run cap/inning (0 = none)", 0, 0, 30),
          numericInput(ns("mercy_diff"), "Mercy differential (0 = none)", 0, 0, 50))),
      accordion_panel("Fielding gender rules",
        selectInput(ns("fielding_preset"), "Preset",
          c("None" = "none", "Standard coed (10-player)" = "standard_coed", "Custom" = "custom")),
        conditionalPanel(sprintf("input['%s'] == 'custom'", ns("fielding_preset")),
          layout_columns(col_widths = c(6,6),
            numericInput(ns("min_females"), "Min females in field", 0, 0, 10),
            numericInput(ns("max_males"), "Max males in field (0 = none)", 0, 0, 12)),
          layout_columns(col_widths = c(4,4,4),
            numericInput(ns("of_females"), "Min F outfield", 0, 0, 5),
            numericInput(ns("if_females"), "Min F infield", 0, 0, 5),
            selectInput(ns("battery_mode"), "Pitcher/Catcher",
              c("Any" = "any", "Opposite genders" = "one")))))
    ),
    actionButton(ns("start"), "Start game", class = "btn-primary bw-outcome-btn")
  )
}

.player_row <- function(ns, prefix, id) {
  pos_choices <- c("(pos)" = "", stats::setNames(APP_CONFIG$positions, APP_CONFIG$positions))
  tags$div(class = "d-flex gap-1 align-items-end mb-1", id = ns(paste0(prefix, "_row_", id)),
    textInput(ns(paste0(prefix, "_name_", id)), NULL, placeholder = "Name"),
    radioButtons(ns(paste0(prefix, "_gender_", id)), NULL, c("M","F"), inline = TRUE),
    numericInput(ns(paste0(prefix, "_jersey_", id)), NULL, value = NA, min = 0, max = 99),
    selectInput(ns(paste0(prefix, "_pos_", id)), NULL, pos_choices),
    actionButton(ns(paste0(prefix, "_del_", id)), "×", class = "btn-sm btn-outline-danger"))
}

setup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    game_start <- reactiveVal(NULL)
    rows <- list(away = reactiveVal(integer()), home = reactiveVal(integer()))
    counter <- reactiveVal(0L)

    add_row <- function(prefix) {
      counter(counter() + 1L); id <- counter()
      rows[[prefix]](c(rows[[prefix]](), id))
      insertUI(sprintf("#%s", ns(paste0(prefix, "_rows"))), where = "beforeEnd",
        ui = .player_row(ns, prefix, id))
      observeEvent(input[[paste0(prefix, "_del_", id)]], {
        removeUI(sprintf("#%s", ns(paste0(prefix, "_row_", id))))
        rows[[prefix]](setdiff(rows[[prefix]](), id))
      }, ignoreInit = TRUE, once = TRUE)
    }
    observeEvent(input$away_add, add_row("away"), ignoreInit = TRUE)
    observeEvent(input$home_add, add_row("home"), ignoreInit = TRUE)

    observeEvent(input$start, {
      cfg <- collect_ruleset(input)
      away <- list(team_id = uuid::UUIDgenerate(), name = input$away_name,
                   lineup = collect_lineup(input, "away", rows$away()))
      home <- list(team_id = uuid::UUIDgenerate(), name = input$home_name,
                   lineup = collect_lineup(input, "home", rows$home()))
      game_start(build_game_start_event(cfg, home, away, "away"))
    })
    game_start
  })
}
