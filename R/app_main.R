# Top-level app UI and server, extracted so the wiring can be exercised by
# shiny::testServer (see tests/test_app_flow.R). app.R just launches these.

bookworm_ui <- function() {
  page_fillable(
    title = APP_CONFIG$app_name,
    theme = app_theme,
    padding = 0,
    tags$head(
      # Explicit viewport: bslib injects a responsive one, but we also want
      # viewport-fit=cover so the safe-area insets in app.css take effect on
      # notched phones, and we lock scaling for a stable one-hand scoring UI.
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1, viewport-fit=cover"
      ),
      # Tints the mobile browser chrome. Matches the manifest theme_color so
      # installed and in-browser chrome agree.
      tags$meta(
        name = "theme-color",
        content = BRAND_COLORS$primary %||% "#28406B"
      ),
      # PWA manifest + home-screen icons. iOS ignores the manifest for the
      # home-screen icon and uses apple-touch-icon instead.
      tags$link(rel = "manifest", href = "manifest.webmanifest"),
      tags$link(rel = "apple-touch-icon", href = "icons/apple-touch-icon.png"),
      tags$link(
        rel = "icon",
        type = "image/png",
        sizes = "32x32",
        href = "icons/favicon-32.png"
      ),
      # Lets iOS treat an added-to-home-screen instance as a standalone app.
      tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
      tags$meta(
        name = "apple-mobile-web-app-status-bar-style",
        content = "default"
      ),
      tags$link(rel = "stylesheet", href = "css/app.css")
    ),
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

  degraded <- reactiveVal(NULL)

  observeEvent(
    identity(),
    {
      req(!is.na(identity()$mode))
      sf <- storage_for_identity(identity())
      store(sf$storage)
      degraded(if (isTRUE(sf$degraded)) sf$reason else NULL)
      if (!is.null(sf$con)) {
        onStop(function() DBI::dbDisconnect(sf$con))
      }
      nav_select("screen", "setup")
    },
    ignoreInit = TRUE
  )

  output$guest_banner <- renderUI({
    req(!is.null(identity()))
    if (!is.null(degraded())) {
      div(class = "alert alert-danger m-2 py-2 small", degraded())
    } else if (identical(identity()$mode, "guest")) {
      div(
        class = "alert alert-warning m-2 py-2 small",
        "Guest mode: sign in to save. Refreshing will lose this game."
      )
    }
  })

  observeEvent(
    game_start(),
    {
      req(store(), game_start())
      gid <- store()$create_game(list(name = "Game"))
      tracking_server("track", store(), gid, game_start())
      nav_select("screen", "track")
    },
    ignoreInit = TRUE
  )
}
