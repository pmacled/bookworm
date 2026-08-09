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
      tags$link(rel = "stylesheet", href = "css/app.css"),
      tags$script(src = "js/persistent-login.js")
    ),
    uiOutput("guest_banner"),
    navset_hidden(
      id = "screen",
      nav_panel_hidden("auth", div(class = "p-3", auth_ui("auth"))),
      nav_panel_hidden("home", div(class = "p-3", home_ui("home"))),
      nav_panel_hidden("manage", div(class = "p-3", manage_ui("manage"))),
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
  # Bumped to prompt the home screen to re-fetch its games list (after creating
  # or opening/updating a game, or when navigating back home).
  home_refresh <- reactiveVal(0L)
  # The live DB connection when signed in with persistence (NULL for guest or
  # degraded mode); the management screen needs it directly.
  db_con <- reactiveVal(NULL)

  home <- home_server(
    "home",
    storage_r = store,
    identity_r = identity,
    refresh_r = home_refresh
  )

  manage_server(
    "manage",
    con_r = db_con,
    identity_r = identity
  )

  observeEvent(
    identity(),
    {
      req(!is.na(identity()$mode))
      sf <- storage_for_identity(identity())
      store(sf$storage)
      db_con(sf$con)
      degraded(if (isTRUE(sf$degraded)) sf$reason else NULL)
      if (!is.null(sf$con)) {
        onStop(function() DBI::dbDisconnect(sf$con))
      }
      home_refresh(isolate(home_refresh()) + 1L)
      nav_select("screen", "home")
    },
    ignoreInit = TRUE
  )

  # New game: go to the setup flow. Opening an existing game: bootstrap tracking
  # for that game (no start event, so events load as-is) and view final games
  # read-only.
  observeEvent(
    home$new_game(),
    {
      req(store())
      nav_select("screen", "setup")
    },
    ignoreInit = TRUE
  )

  observeEvent(
    home$manage(),
    {
      # Management needs a live DB connection; guests / degraded mode have none.
      if (is.null(db_con())) {
        showNotification(
          "Sign in with saving enabled to manage leagues and teams.",
          type = "warning",
          duration = 4
        )
        return(invisible())
      }
      nav_select("screen", "manage")
    },
    ignoreInit = TRUE
  )

  observeEvent(
    home$open_game(),
    {
      og <- home$open_game()
      req(store(), og)
      tracking_server(
        "track",
        store(),
        og$game_id,
        game_start_event = NULL,
        read_only = identical(og$status, "final")
      )
      nav_select("screen", "track")
    },
    ignoreInit = TRUE
  )

  output$guest_banner <- renderUI({
    req(!is.null(identity()))
    id <- identity()
    sign_out_btn <- actionButton(
      # Wired into the auth module's namespace so it triggers auth_server's
      # input$sign_out (which revokes the token and clears localStorage).
      NS("auth")("sign_out"),
      "Sign out",
      class = "btn-sm btn-outline-secondary ms-auto"
    )
    home_btn <- actionButton(
      "go_home",
      "Home",
      class = "btn-sm btn-outline-secondary"
    )
    if (!is.null(degraded())) {
      div(class = "alert alert-danger m-2 py-2 small", degraded())
    } else if (identical(id$mode, "guest")) {
      div(
        class = "d-flex align-items-center gap-2 m-2 py-2 px-2 alert alert-warning small",
        tags$span(
          class = "me-auto",
          "Guest mode: sign in to save. Refreshing will lose this game."
        ),
        home_btn
      )
    } else if (identical(id$mode, "user")) {
      div(
        class = "d-flex align-items-center gap-2 m-2 py-1 px-2 small text-muted",
        tags$span(
          "Signed in",
          if (!is.null(id$username)) paste0(" as ", id$username) else ""
        ),
        home_btn,
        sign_out_btn
      )
    }
  })

  # Sign-out flips identity mode back to NA; return to the auth screen.
  observeEvent(
    identity()$mode,
    {
      if (is.na(identity()$mode)) {
        store(NULL)
        db_con(NULL)
        nav_select("screen", "auth")
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    game_start(),
    {
      req(store(), game_start())
      gid <- store()$create_game(list(name = "Game"))
      tracking_server("track", store(), gid, game_start())
      home_refresh(isolate(home_refresh()) + 1L)
      nav_select("screen", "track")
    },
    ignoreInit = TRUE
  )

  # Return to the games list from anywhere.
  observeEvent(input$go_home, {
    home_refresh(isolate(home_refresh()) + 1L)
    nav_select("screen", "home")
  })
}
