# Home screen: lists the games a user can see and routes to "new game" (setup)
# or opens an existing game in the tracking view. Works for guests too, whose
# in-memory storage$list_games() returns their non-persistent games.
#
# Games are shown as cards (they don't fit one row on mobile), each showing the
# matchup (away @ home), status, relationship, and last-updated, with Open and
# (when permitted) Delete actions.

.RELATIONSHIP_LABELS <- c(
  owned = "Mine",
  league = "League",
  shared = "Shared"
)

.status_badge <- function(status) {
  final <- identical(status, "final")
  span(
    class = paste("badge", if (final) "bg-secondary" else "bg-success"),
    if (final) "Final" else "In progress"
  )
}

.relationship_badge <- function(rel) {
  span(
    class = "badge bg-light text-dark border",
    .RELATIONSHIP_LABELS[[rel]] %||% rel
  )
}

.matchup_title <- function(away, home) {
  a <- if (is.na(away) || !nzchar(away)) "Away" else away
  h <- if (is.na(home) || !nzchar(home)) "Home" else home
  paste0(a, " @ ", h)
}

# Normalize whatever list_games() returns into a stable data frame so both the
# guest and Supabase backends render identically.
.normalize_games <- function(df) {
  cols <- c(
    "game_id",
    "name",
    "status",
    "updated_at",
    "relationship",
    "home_team",
    "away_team",
    "can_delete"
  )
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    empty <- data.frame(
      game_id = character(),
      name = character(),
      status = character(),
      updated_at = character(),
      relationship = character(),
      home_team = character(),
      away_team = character(),
      can_delete = logical()
    )
    return(empty)
  }
  df$name <- as.character(df$name %||% "Game")
  df$status <- as.character(df$status %||% "in_progress")
  df$updated_at <- as.character(df$updated_at %||% NA_character_)
  df$relationship <- as.character(df$relationship %||% "owned")
  df$home_team <- as.character(df$home_team %||% NA_character_)
  df$away_team <- as.character(df$away_team %||% NA_character_)
  df$can_delete <- as.logical(df$can_delete %||% FALSE)
  df[, cols]
}

# Render a stored timestamp in Eastern time, truncated to the second, e.g.
# "2026-08-09 09:37:49 EDT". Falls back to the raw string if it can't be parsed.
.format_updated <- function(ts) {
  if (is.null(ts) || length(ts) != 1 || is.na(ts) || !nzchar(ts)) {
    return(NA_character_)
  }
  parsed <- suppressWarnings(as.POSIXct(ts, tz = "UTC"))
  if (is.na(parsed)) {
    return(as.character(ts))
  }
  format(parsed, "%Y-%m-%d %H:%M:%S %Z", tz = "America/New_York")
}

home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "d-flex align-items-center justify-content-between mb-3",
      tags$h3(class = "m-0", "Your games"),
      div(
        class = "d-flex gap-2",
        uiOutput(ns("manage_btn"), inline = TRUE),
        actionButton(ns("new_game"), "New game", class = "btn-primary")
      )
    ),
    uiOutput(ns("filter")),
    uiOutput(ns("games")),
    uiOutput(ns("empty"))
  )
}

home_server <- function(id, storage_r, identity_r, refresh_r = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    open_game <- reactiveVal(NULL)
    # Bumped locally after a delete so the list re-fetches without waiting on
    # the caller's refresh signal.
    local_refresh <- reactiveVal(0L)

    games <- reactive({
      req(storage_r())
      refresh_r()
      local_refresh()
      .normalize_games(storage_r()$list_games())
    })

    filtered <- reactive({
      g <- games()
      sel <- input$rel_filter %||% "all"
      if (identical(sel, "all")) {
        return(g)
      }
      g[g$relationship == sel, , drop = FALSE]
    })

    output$filter <- renderUI({
      rels <- unique(games()$relationship)
      if (length(rels) <= 1) {
        return(NULL)
      }
      choices <- c("All" = "all")
      for (r in c("owned", "league", "shared")) {
        if (r %in% rels) {
          choices[.RELATIONSHIP_LABELS[[r]]] <- r
        }
      }
      radioButtons(
        ns("rel_filter"),
        NULL,
        choices = choices,
        selected = "all",
        inline = TRUE
      )
    })

    # Management requires a persistent account (leagues/teams live in the DB),
    # so only surface the Manage button to signed-in users.
    output$manage_btn <- renderUI({
      if (identical(identity_r()$mode, "user")) {
        actionButton(ns("manage"), "Manage", class = "btn-outline-secondary")
      }
    })

    output$empty <- renderUI({
      if (nrow(games()) > 0) {
        return(NULL)
      }
      div(
        class = "text-muted mt-3",
        "No games yet. Tap \u201cNew game\u201d to start scoring."
      )
    })

    output$games <- renderUI({
      g <- filtered()
      if (nrow(g) == 0) {
        return(NULL)
      }
      cards <- lapply(seq_len(nrow(g)), function(i) {
        gid <- g$game_id[[i]]
        can_delete <- isTRUE(g$can_delete[[i]])
        updated <- .format_updated(g$updated_at[[i]])
        div(
          class = "card mb-2",
          div(
            class = "card-body py-2 px-3",
            div(
              class = "d-flex align-items-start justify-content-between gap-2",
              div(
                tags$h6(
                  class = "card-title mb-1",
                  .matchup_title(g$away_team[[i]], g$home_team[[i]])
                ),
                div(
                  class = "d-flex gap-1 mb-1",
                  .status_badge(g$status[[i]]),
                  .relationship_badge(g$relationship[[i]])
                ),
                if (!is.na(updated) && nzchar(updated)) {
                  div(class = "text-muted small", paste("Updated", updated))
                }
              ),
              div(
                class = "d-flex flex-column gap-1",
                actionButton(
                  ns(paste0("open_", gid)),
                  "Open",
                  class = "btn-sm btn-primary"
                ),
                if (can_delete) {
                  actionButton(
                    ns(paste0("del_", gid)),
                    "Delete",
                    class = "btn-sm btn-outline-danger"
                  )
                }
              )
            )
          )
        )
      })
      div(!!!cards)
    })

    # Per-card Open / Delete observers. renderUI regenerates the buttons on each
    # refresh; observers are (re)registered here for the current row set. Using
    # once = FALSE with an id-scoped input name keeps them stable across renders.
    observe({
      g <- filtered()
      for (i in seq_len(nrow(g))) {
        local({
          gid <- g$game_id[[i]]
          status <- g$status[[i]]
          rel <- g$relationship[[i]]
          away <- g$away_team[[i]]
          home <- g$home_team[[i]]
          observeEvent(
            input[[paste0("open_", gid)]],
            open_game(list(game_id = gid, status = status, relationship = rel)),
            ignoreInit = TRUE
          )
          observeEvent(
            input[[paste0("del_", gid)]],
            showModal(modalDialog(
              title = "Delete game?",
              sprintf(
                "Delete \u201c%s\u201d? This permanently removes the game and its scorebook.",
                .matchup_title(away, home)
              ),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(
                  ns(paste0("confirm_del_", gid)),
                  "Delete",
                  class = "btn-danger"
                )
              ),
              easyClose = TRUE
            )),
            ignoreInit = TRUE
          )
          observeEvent(
            input[[paste0("confirm_del_", gid)]],
            {
              removeModal()
              uid <- isolate(identity_r())$user_id
              ok <- tryCatch(
                storage_r()$delete_game(gid, uid),
                error = function(e) FALSE
              )
              if (isTRUE(ok)) {
                showNotification(
                  "Game deleted.",
                  type = "message",
                  duration = 3
                )
                local_refresh(isolate(local_refresh()) + 1L)
              } else {
                showNotification(
                  "Could not delete that game.",
                  type = "warning",
                  duration = 4
                )
              }
            },
            ignoreInit = TRUE
          )
        })
      }
    })

    list(
      new_game = reactive(input$new_game),
      manage = reactive(input$manage),
      open_game = open_game
    )
  })
}
