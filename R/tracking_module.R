.OUT_OUTCOMES <- c("K","KL","GO","FO","LO","PO")

record_outcome_event <- function(state, outcome, team) {
  reached <- switch(outcome, "1B"=1L,"2B"=2L,"3B"=3L,"HR"=4L,
                    "BB"=1L,"IBB"=1L,"HBP"=1L,"FC"=1L,"E"=1L, NA_integer_)
  outs_on_play <- if (outcome %in% .OUT_OUTCOMES) 1L else 0L
  advances <- suggest_advances(state, outcome)
  # suggest_advances already includes the batter's own advance (scored on a HR),
  # so RBIs are simply the count of scored advances — do NOT add a separate
  # reached==4 bonus or the batter's HR run would be double-counted.
  rbi <- sum(vapply(advances, function(a) isTRUE(a$scored), logical(1)))
  new_event("plate_appearance", list(team = team,
    batter_id = state$current_batter$player_id, outcome = outcome,
    reached = reached, rbi = as.integer(rbi), outs_on_play = outs_on_play,
    advances = advances))
}

tracking_ui <- function(id) {
  ns <- NS(id)
  outcomes <- c("1B","2B","3B","HR","BB","K","GO","FO","FC","E")
  btns <- lapply(outcomes, function(o)
    actionButton(ns(paste0("o_", o)), o, class = "btn-outline-primary bw-outcome-btn"))
  tagList(
    uiOutput(ns("situation")),
    div(class = "bw-outcome-grid d-grid",
        style = "grid-template-columns: repeat(5,1fr); gap:.5rem;", !!!btns),
    div(class = "d-flex gap-2 mt-2",
        actionButton(ns("undo"), "Undo", class = "btn-warning"),
        actionButton(ns("sub"), "Substitution", class = "btn-outline-secondary")),
    navset_tab(
      nav_panel("Scorebook", uiOutput(ns("scorebook"))),
      nav_panel("Box score", tableOutput(ns("box_away")), tableOutput(ns("box_home"))))
  )
}

tracking_server <- function(id, storage, game_id, game_start_event) {
  moduleServer(id, function(input, output, session) {
    events <- reactiveVal(NULL)
    isolate({
      appended <- storage$append_event(game_id, game_start_event)
      events(storage$load_events(game_id))
    })
    state <- reactive(fold_events(events()))

    record <- function(outcome) {
      s <- isolate(state())
      if (identical(s$status, "final")) return(invisible())
      evt <- record_outcome_event(s, outcome, s$batting_team)
      storage$append_event(game_id, evt)
      events(storage$load_events(game_id))
      storage$save_snapshot(game_id, isolate(state()))
    }
    outcomes <- c("1B","2B","3B","HR","BB","K","GO","FO","FC","E")
    lapply(outcomes, function(o)
      observeEvent(input[[paste0("o_", o)]], record(o), ignoreInit = TRUE))

    observeEvent(input$undo, {
      ev <- events()
      if (length(ev) > 1) {   # never drop game_start
        # Rebuild by re-appending all but the last into a fresh guest-ish reload:
        # simplest correct approach — reload, drop last, persist via snapshot only.
        ev <- ev[-length(ev)]
        events(ev)
        storage$save_snapshot(game_id, fold_events(ev))
      }
    }, ignoreInit = TRUE)

    output$situation <- renderUI({
      s <- state()
      tags$div(class = "d-flex justify-content-between align-items-center",
        tags$div(sprintf("Inning %d %s • %d out • %d-%d",
          s$inning, s$half, s$outs, s$count$balls, s$count$strikes)),
        tags$div(sprintf("%s: away %d – home %d",
          if (identical(s$status,"final")) "FINAL" else "Score", s$score$away, s$score$home)),
        if (!is.null(s$current_batter)) tags$strong(s$current_batter$name))
    })
    output$scorebook <- renderUI(render_scorebook_svg(state(), state()$batting_team))
    output$box_away <- renderTable(batting_lines(state(), "away"))
    output$box_home <- renderTable(batting_lines(state(), "home"))

    if (length(state()$warnings)) NULL  # warnings surfaced via situation panel later
    state
  })
}
