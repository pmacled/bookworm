# Top-level app UI and server, extracted so the wiring can be exercised by
# shiny::testServer (see tests/test_app_flow.R). app.R just launches these.

bookworm_ui <- function() {
  page_fillable(
    title = APP_CONFIG$app_name, theme = app_theme, padding = 0,
    tags$head(tags$link(rel = "stylesheet", href = "css/app.css")),
    uiOutput("guest_banner"),
    navset_hidden(
      id = "screen",
      nav_panel_hidden("auth", div(class = "p-3", auth_ui("auth"))),
      nav_panel_hidden("setup", div(class = "p-3", setup_ui("setup"))),
      nav_panel_hidden("track", div(class = "p-2", tracking_ui("track")))
    )
  )
}

bookworm_server <- function(input, output, session) {
  identity <- auth_server("auth")
  store <- reactiveVal(NULL)
  game_start <- setup_server("setup")

  observeEvent(identity(), {
    req(!is.na(identity()$mode))
    sf <- storage_for_identity(identity())
    store(sf$storage)
    if (!is.null(sf$con)) onStop(function() DBI::dbDisconnect(sf$con))
    nav_select("screen", "setup")
  }, ignoreInit = TRUE)

  output$guest_banner <- renderUI({
    req(!is.null(identity()))
    if (identical(identity()$mode, "guest")) {
      div(
        class = "alert alert-warning m-2 py-2 small",
        "Guest mode: sign in to save. Refreshing will lose this game."
      )
    }
  })

  observeEvent(game_start(), {
    req(store(), game_start())
    gid <- store()$create_game(list(name = "Game"))
    tracking_server("track", store(), gid, game_start())
    nav_select("screen", "track")
  }, ignoreInit = TRUE)
}
