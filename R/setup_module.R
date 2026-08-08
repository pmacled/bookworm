build_game_start_event <- function(ruleset, home, away, first_bat = "away") {
  ruleset <- coerce_ruleset_config(ruleset)
  v <- validate_ruleset_config(ruleset)
  if (!v$ok) stop(paste("Invalid ruleset:", paste(v$errors, collapse = "; ")))
  new_event("game_start", list(ruleset = ruleset, first_bat = first_bat,
    home = home, away = away), seq = 1L)
}

.parse_jersey <- function(x) {
  if (is.null(x) || length(x) != 1) return(NA_integer_)
  x <- trimws(as.character(x))
  if (!nzchar(x) || is.na(x) || !grepl("^[0-9]+$", x)) return(NA_integer_)
  as.integer(x)
}

collect_lineup <- function(input, prefix, row_ids, show_gender = TRUE) {
  players <- list()
  for (id in row_ids) {
    nm <- trimws(input[[paste0(prefix, "_name_", id)]] %||% "")
    if (!nzchar(nm)) next
    pos <- input[[paste0(prefix, "_pos_", id)]] %||% ""
    pos <- if (!nzchar(pos)) NA_character_ else pos
    gender <- if (show_gender) (input[[paste0(prefix, "_gender_", id)]] %||% "M") else "M"
    slot <- length(players) + 1L
    players[[slot]] <- make_player(uuid::UUIDgenerate(), nm, gender,
      jersey_number = .parse_jersey(input[[paste0(prefix, "_jersey_", id)]]),
      order_slot = slot, position = pos)
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
    batting_size = as.integer(input$batting_size %||% "0"),  # selectInput string; coerce maps 0/NA => unlimited
    fielding = fielding,
    innings = input$innings,
    run_cap = list(per_inning = if ((input$run_cap %||% 0) > 0) input$run_cap else NA_integer_),
    mercy_rule = list(tiers = if ((input$mercy_diff %||% 0) > 0)
      list(list(after_inning = 1L, differential = input$mercy_diff)) else list())))
}

.POS_CHOICES <- function()
  c("(pos)" = "", stats::setNames(APP_CONFIG$positions, APP_CONFIG$positions))

.lineup_table_head <- function(show_gender) {
  cols <- c("#", "Name", if (show_gender) "Gender", "Jersey", "Position", "")
  tags$thead(tags$tr(!!!lapply(cols, function(h) tags$th(scope = "col", h))))
}

.lineup_ui <- function(ns, prefix, title, show_gender = TRUE) {
  tagList(
    tags$h5(title),
    tags$p(class = "text-muted small",
      "Leave this lineup empty to just record this team's runs each inning."),
    tags$div(class = "bw-lineup-wrap",
      tags$table(class = "table table-sm bw-lineup",
        .lineup_table_head(show_gender),
        tags$tbody(id = ns(paste0(prefix, "_rows"))))),
    div(class = "d-flex gap-2",
      actionButton(ns(paste0(prefix, "_add")), "Add player",
                   class = "btn-sm btn-outline-secondary"),
      actionButton(ns(paste0(prefix, "_save")), "Save lineup",
                   class = "btn-sm btn-primary")),
    uiOutput(ns(paste0(prefix, "_validation"))))
}

setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("New game"),
    textInput(ns("away_name"), "Away team", "Away"),
    .lineup_ui(ns, "away", "Away lineup", show_gender = TRUE),
    textInput(ns("home_name"), "Home team", "Home"),
    .lineup_ui(ns, "home", "Home lineup", show_gender = TRUE),
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

.player_row <- function(ns, prefix, id, order, show_gender) {
  cell <- function(...) tags$td(class = "bw-cell", ...)
  tags$tr(id = ns(paste0(prefix, "_row_", id)),
    tags$td(class = "bw-order", order),
    cell(textInput(ns(paste0(prefix, "_name_", id)), NULL, placeholder = "Name")),
    if (show_gender)
      cell(selectInput(ns(paste0(prefix, "_gender_", id)), NULL,
                       c("M" = "M", "F" = "F"), width = "5rem")),
    cell(tagAppendAttributes(
      textInput(ns(paste0(prefix, "_jersey_", id)), NULL, placeholder = "#", width = "5rem"),
      inputmode = "numeric", .cssSelector = "input")),
    cell(selectInput(ns(paste0(prefix, "_pos_", id)), NULL, .POS_CHOICES(),
                     width = "7rem")),
    cell(actionButton(ns(paste0(prefix, "_del_", id)), "×",
                      class = "btn-sm btn-outline-danger")))
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
        ui = .player_row(ns, prefix, id, order = length(rows[[prefix]]()), show_gender = TRUE))
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
