# League / team / player management. Drill-down: Leagues -> Teams -> Players.
# Requires a live DB connection (management is a persistent, signed-in feature;
# guest mode has no leagues), passed as a reactive `con_r` along with the
# signed-in `identity_r`. Permissions: a league owner manages everything; a team
# captain manages their own team's players. Access is enforced in manage_data.R;
# the UI simply hides controls the user can't use.

manage_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "p-1",
    uiOutput(ns("breadcrumb")),
    navset_hidden(
      id = ns("level"),
      nav_panel_hidden("leagues", uiOutput(ns("leagues_panel"))),
      nav_panel_hidden("teams", uiOutput(ns("teams_panel"))),
      nav_panel_hidden("players", uiOutput(ns("players_panel")))
    )
  )
}

manage_server <- function(id, con_r, identity_r, refresh_r = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    level <- reactiveVal("leagues")
    sel_league <- reactiveVal(NULL) # list(id, name, is_owner)
    sel_team <- reactiveVal(NULL) # list(id, name, can_manage)
    # Bumped after any mutation to re-fetch the current list.
    tick <- reactiveVal(0L)
    bump <- function() tick(isolate(tick()) + 1L)

    uid <- reactive(identity_r()$user_id)
    con <- reactive({
      req(con_r())
      con_r()
    })

    observeEvent(level(), nav_select(ns("level"), level()))

    # ---- Breadcrumb -------------------------------------------------------
    output$breadcrumb <- renderUI({
      crumbs <- list(
        actionLink(ns("crumb_leagues"), "Leagues")
      )
      if (!is.null(sel_league())) {
        crumbs <- c(
          crumbs,
          list(tags$span(class = "text-muted mx-1", "/")),
          list(actionLink(ns("crumb_teams"), sel_league()$name))
        )
      }
      if (!is.null(sel_team())) {
        crumbs <- c(
          crumbs,
          list(tags$span(class = "text-muted mx-1", "/")),
          list(tags$span(sel_team()$name))
        )
      }
      div(class = "mb-2 small", !!!crumbs)
    })
    observeEvent(input$crumb_leagues, {
      sel_league(NULL)
      sel_team(NULL)
      level("leagues")
    })
    observeEvent(input$crumb_teams, {
      sel_team(NULL)
      level("teams")
    })

    notify_result <- function(res, ok_msg) {
      if (isTRUE(res$ok)) {
        showNotification(ok_msg, type = "message", duration = 3)
        bump()
      } else {
        showNotification(res$error, type = "warning", duration = 5)
      }
    }

    # ---- Leagues level ----------------------------------------------------
    leagues <- reactive({
      refresh_r()
      tick()
      req(con())
      db_list_leagues(con(), uid())
    })

    output$leagues_panel <- renderUI({
      lg <- leagues()
      rows <- if (nrow(lg) == 0) {
        div(class = "text-muted", "No leagues yet. Create one to get started.")
      } else {
        div(!!!lapply(seq_len(nrow(lg)), function(i) {
          lid <- lg$id[[i]]
          owner <- isTRUE(lg$is_owner[[i]])
          div(
            class = "card mb-2",
            div(
              class = "card-body py-2 px-3 d-flex align-items-center justify-content-between gap-2",
              div(
                actionLink(ns(paste0("open_league_", lid)), lg$name[[i]], class = "fw-semibold"),
                tags$span(class = "badge bg-light text-dark border ms-2", lg$sport[[i]]),
                if (owner) tags$span(class = "badge bg-primary ms-1", "Owner")
              ),
              if (owner) {
                actionButton(ns(paste0("del_league_", lid)), "Delete", class = "btn-sm btn-outline-danger")
              }
            )
          )
        }))
      }
      tagList(
        div(
          class = "d-flex align-items-center justify-content-between mb-3",
          tags$h4(class = "m-0", "Leagues"),
          actionButton(ns("new_league"), "New league", class = "btn-primary btn-sm")
        ),
        rows
      )
    })

    observeEvent(input$new_league, {
      showModal(modalDialog(
        title = "New league",
        textInput(ns("league_name"), "Name"),
        selectInput(ns("league_sport"), "Sport", c("Softball" = "softball", "Baseball" = "baseball")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("league_create"), "Create", class = "btn-primary")
        )
      ))
    })
    observeEvent(input$league_create, {
      res <- db_create_league(con(), uid(), input$league_name, input$league_sport %||% "softball")
      if (isTRUE(res$ok)) {
        removeModal()
      }
      notify_result(res, "League created.")
    })

    # Dynamic per-league open / delete observers.
    observe({
      lg <- leagues()
      for (i in seq_len(nrow(lg))) {
        local({
          lid <- lg$id[[i]]
          nm <- lg$name[[i]]
          owner <- isTRUE(lg$is_owner[[i]])
          observeEvent(input[[paste0("open_league_", lid)]], {
            sel_league(list(id = lid, name = nm, is_owner = owner))
            sel_team(NULL)
            level("teams")
          }, ignoreInit = TRUE)
          observeEvent(input[[paste0("del_league_", lid)]], {
            showModal(modalDialog(
              title = "Delete league?",
              sprintf("Delete \u201c%s\u201d and all its teams and players? Games keep their history but lose the league link.", nm),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(ns(paste0("confirm_del_league_", lid)), "Delete", class = "btn-danger")
              )
            ))
          }, ignoreInit = TRUE)
          observeEvent(input[[paste0("confirm_del_league_", lid)]], {
            removeModal()
            notify_result(db_delete_league(con(), uid(), lid), "League deleted.")
          }, ignoreInit = TRUE)
        })
      }
    })

    # ---- Teams level ------------------------------------------------------
    teams <- reactive({
      req(sel_league())
      tick()
      db_list_teams(con(), sel_league()$id)
    })

    output$teams_panel <- renderUI({
      req(sel_league())
      owner <- isTRUE(sel_league()$is_owner)
      tm <- teams()
      rows <- if (nrow(tm) == 0) {
        div(class = "text-muted", "No teams yet.")
      } else {
        div(!!!lapply(seq_len(nrow(tm)), function(i) {
          tid <- tm$id[[i]]
          cap <- tm$captain_username[[i]]
          div(
            class = "card mb-2",
            div(
              class = "card-body py-2 px-3 d-flex align-items-center justify-content-between gap-2",
              div(
                actionLink(ns(paste0("open_team_", tid)), tm$name[[i]], class = "fw-semibold"),
                if (!is.na(cap) && nzchar(cap)) {
                  tags$span(class = "text-muted small ms-2", paste("Captain:", cap))
                }
              ),
              if (owner) {
                actionButton(ns(paste0("del_team_", tid)), "Delete", class = "btn-sm btn-outline-danger")
              }
            )
          )
        }))
      }
      tagList(
        div(
          class = "d-flex align-items-center justify-content-between mb-3",
          tags$h4(class = "m-0", sel_league()$name),
          if (owner) actionButton(ns("new_team"), "Add team", class = "btn-primary btn-sm")
        ),
        rows
      )
    })

    observeEvent(input$new_team, {
      showModal(modalDialog(
        title = "Add team",
        textInput(ns("team_name"), "Team name"),
        textInput(ns("team_captain"), "Captain username (optional)"),
        tags$p(class = "text-muted small", "Adding a captain lets that user manage this team's players and view the league's games."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("team_create"), "Add", class = "btn-primary")
        )
      ))
    })
    observeEvent(input$team_create, {
      res <- db_create_team(con(), uid(), sel_league()$id, input$team_name, input$team_captain)
      if (isTRUE(res$ok)) {
        removeModal()
      }
      notify_result(res, "Team added.")
    })

    observe({
      req(sel_league())
      tm <- teams()
      owner <- isTRUE(sel_league()$is_owner)
      for (i in seq_len(nrow(tm))) {
        local({
          tid <- tm$id[[i]]
          nm <- tm$name[[i]]
          # A user can manage a team if they own the league or captain the team.
          observeEvent(input[[paste0("open_team_", tid)]], {
            can <- owner || db_user_manages_team(con(), uid(), tid)
            sel_team(list(id = tid, name = nm, can_manage = can))
            level("players")
          }, ignoreInit = TRUE)
          observeEvent(input[[paste0("del_team_", tid)]], {
            showModal(modalDialog(
              title = "Delete team?",
              sprintf("Delete \u201c%s\u201d and its players?", nm),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(ns(paste0("confirm_del_team_", tid)), "Delete", class = "btn-danger")
              )
            ))
          }, ignoreInit = TRUE)
          observeEvent(input[[paste0("confirm_del_team_", tid)]], {
            removeModal()
            notify_result(db_delete_team(con(), uid(), tid), "Team deleted.")
          }, ignoreInit = TRUE)
        })
      }
    })

    # ---- Players level ----------------------------------------------------
    players <- reactive({
      req(sel_team())
      tick()
      db_list_players(con(), sel_team()$id)
    })

    output$players_panel <- renderUI({
      req(sel_team())
      can <- isTRUE(sel_team()$can_manage)
      pl <- players()
      rows <- if (nrow(pl) == 0) {
        div(class = "text-muted", "No players yet.")
      } else {
        div(!!!lapply(seq_len(nrow(pl)), function(i) {
          pid <- pl$id[[i]]
          jersey <- pl$jersey_number[[i]]
          gender <- pl$gender[[i]]
          meta <- paste(c(
            if (!is.na(jersey)) paste0("#", jersey),
            if (!is.na(gender) && nzchar(gender)) gender
          ), collapse = " \u00b7 ")
          div(
            class = "card mb-2",
            div(
              class = "card-body py-2 px-3 d-flex align-items-center justify-content-between gap-2",
              div(
                tags$span(class = "fw-semibold", pl$name[[i]]),
                if (nzchar(meta)) tags$span(class = "text-muted small ms-2", meta)
              ),
              if (can) {
                div(
                  class = "d-flex gap-1",
                  actionButton(ns(paste0("edit_player_", pid)), "Edit", class = "btn-sm btn-outline-secondary"),
                  actionButton(ns(paste0("del_player_", pid)), "Remove", class = "btn-sm btn-outline-danger")
                )
              }
            )
          )
        }))
      }
      tagList(
        div(
          class = "d-flex align-items-center justify-content-between mb-3",
          tags$h4(class = "m-0", sel_team()$name),
          if (can) actionButton(ns("new_player"), "Add player", class = "btn-primary btn-sm")
        ),
        rows
      )
    })

    player_form_modal <- function(title, confirm_id, prefill = list()) {
      modalDialog(
        title = title,
        textInput(ns("player_name"), "Name", value = prefill$name %||% ""),
        selectInput(
          ns("player_gender"), "Gender",
          c("\u2014" = "", "M" = "M", "F" = "F"),
          selected = prefill$gender %||% ""
        ),
        numericInput(ns("player_jersey"), "Jersey number", value = prefill$jersey %||% NA, min = 0, max = 999),
        numericInput(ns("player_pos"), "Default position (1-10)", value = prefill$pos %||% NA, min = 1, max = 10),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns(confirm_id), "Save", class = "btn-primary")
        )
      )
    }

    observeEvent(input$new_player, {
      showModal(player_form_modal("Add player", "player_add"))
    })
    observeEvent(input$player_add, {
      res <- db_add_player(
        con(), uid(), sel_team()$id,
        input$player_name, input$player_gender, input$player_jersey, input$player_pos
      )
      if (isTRUE(res$ok)) {
        removeModal()
      }
      notify_result(res, "Player added.")
    })

    editing_player <- reactiveVal(NULL)
    observe({
      req(sel_team())
      pl <- players()
      for (i in seq_len(nrow(pl))) {
        local({
          pid <- pl$id[[i]]
          row <- pl[i, ]
          observeEvent(input[[paste0("edit_player_", pid)]], {
            editing_player(pid)
            showModal(player_form_modal("Edit player", "player_save", list(
              name = row$name[[1]],
              gender = row$gender[[1]],
              jersey = row$jersey_number[[1]],
              pos = row$default_position[[1]]
            )))
          }, ignoreInit = TRUE)
          observeEvent(input[[paste0("del_player_", pid)]], {
            notify_result(db_delete_player(con(), uid(), pid), "Player removed.")
          }, ignoreInit = TRUE)
        })
      }
    })
    observeEvent(input$player_save, {
      pid <- isolate(editing_player())
      req(pid)
      res <- db_update_player(
        con(), uid(), pid,
        input$player_name, input$player_gender, input$player_jersey, input$player_pos
      )
      if (isTRUE(res$ok)) {
        removeModal()
      }
      notify_result(res, "Player updated.")
    })

    # Expose current level/selection for tests and callers.
    list(
      level = level,
      sel_league = sel_league,
      sel_team = sel_team
    )
  })
}
