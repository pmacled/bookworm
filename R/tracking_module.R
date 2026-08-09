.OUT_OUTCOMES <- c("K", "KL", "GO", "FO", "LO", "PO")

partition_warnings <- function(warnings) {
  warnings <- warnings %||% list()
  msg <- function(sev) {
    unlist(
      lapply(warnings, function(x) {
        if (identical(x$severity, sev)) x$message else NULL
      }),
      use.names = FALSE
    ) %||%
      character()
  }
  list(violations = msg("violation"), notices = msg("notice"))
}
.violation_codes <- function(warnings) {
  sort(
    unlist(
      lapply(warnings, function(x) {
        if (identical(x$severity, "violation")) x$code else NULL
      }),
      use.names = FALSE
    ) %||%
      character()
  )
}

record_half_runs_event <- function(state, runs) {
  new_event(
    "half_runs",
    list(team = state$batting_team, runs = as.integer(runs %||% 0L))
  )
}

.batting_team_has_lineup <- function(state) {
  lu <- state$lineups[[state$batting_team]]
  length(Filter(function(p) !is.na(p$order_slot), lu)) > 0
}

record_outcome_event <- function(state, outcome, team) {
  reached <- switch(
    outcome,
    "1B" = 1L,
    "2B" = 2L,
    "3B" = 3L,
    "HR" = 4L,
    "ITPHR" = 4L,
    "BB" = 1L,
    "IBB" = 1L,
    "HBP" = 1L,
    "FC" = 1L,
    "E" = 1L,
    NA_integer_
  )
  outs_on_play <- if (outcome %in% .OUT_OUTCOMES) 1L else 0L
  advances <- suggest_advances(state, outcome)
  # suggest_advances already includes the batter's own advance (scored on a HR),
  # so RBIs are simply the count of scored advances — do NOT add a separate
  # reached==4 bonus or the batter's HR run would be double-counted.
  rbi <- sum(vapply(advances, function(a) isTRUE(a$scored), logical(1)))
  new_event(
    "plate_appearance",
    list(
      team = team,
      batter_id = state$current_batter$player_id,
      outcome = outcome,
      reached = reached,
      rbi = as.integer(rbi),
      outs_on_play = outs_on_play,
      advances = advances
    )
  )
}

# Category order and labels for the outcome button groups, matching the Help tab so
# the vocabulary has one presentation. All 18 documented codes get a button.
.OUTCOME_BTN_GROUPS <- c(
  hit = "Hits",
  on_base = "Reached base",
  out = "Outs",
  other = "Other"
)

outcome_button_grid <- function(ns) {
  groups <- lapply(names(.OUTCOME_BTN_GROUPS), function(cat) {
    codes <- names(Filter(
      function(m) identical(m$category, cat),
      APP_CONFIG$outcome_meta
    ))
    if (!length(codes)) {
      return(NULL)
    }
    btns <- lapply(codes, function(o) {
      actionButton(
        ns(paste0("o_", o)),
        o,
        class = "btn-outline-primary bw-outcome-btn",
        title = APP_CONFIG$outcome_meta[[o]]$label
      )
    })
    tags$div(
      class = "bw-outcome-group",
      tags$div(
        class = "bw-outcome-group-label text-muted small",
        .OUTCOME_BTN_GROUPS[[cat]]
      ),
      tags$div(
        class = "bw-outcome-grid d-grid",
        style = "grid-template-columns: repeat(5,1fr); gap:.5rem;",
        !!!btns
      )
    )
  })
  tags$div(class = "bw-outcome-panel", !!!Filter(Negate(is.null), groups))
}

tracking_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("situation")),
    uiOutput(ns("action_panel")),
    uiOutput(ns("secondary_actions")),
    navset_tab(
      nav_panel(
        "Scorebook",
        div(
          class = "p-2",
          radioButtons(
            ns("sb_team"),
            NULL,
            c("Away" = "away", "Home" = "home"),
            selected = "away",
            inline = TRUE
          ),
          uiOutput(ns("scorebook"))
        )
      ),
      nav_panel(
        "Box score",
        div(
          class = "p-2",
          uiOutput(ns("box_away_hdr")),
          DT::DTOutput(ns("box_away")),
          uiOutput(ns("box_home_hdr")),
          DT::DTOutput(ns("box_home"))
        )
      ),
      nav_panel("Help", outcome_help_ui())
    )
  )
}

tracking_server <- function(
  id,
  storage,
  game_id,
  game_start_event = NULL,
  read_only = FALSE
) {
  moduleServer(id, function(input, output, session) {
    events <- reactiveVal(NULL)
    isolate({
      existing <- storage$load_events(game_id)
      # New game: seed the start event, but only if the log is empty so a
      # resume/open path never double-appends. Resume/view: load as-is.
      if (!is.null(game_start_event) && length(existing) == 0) {
        storage$append_event(game_id, game_start_event)
        existing <- storage$load_events(game_id)
      }
      events(existing)
    })
    state <- reactive(fold_events(events()))

    # Secondary actions and all mutation paths are suppressed in read-only mode
    # (used when a game is final / not editable by the viewer).
    output$secondary_actions <- renderUI({
      if (isTRUE(read_only)) {
        return(NULL)
      }
      ns <- session$ns
      div(
        class = "d-flex gap-2 mt-2 bw-secondary-actions",
        actionButton(ns("undo"), "Undo", class = "btn-sm btn-outline-warning"),
        actionButton(
          ns("sub"),
          "Substitution",
          class = "btn-sm btn-outline-secondary"
        ),
        actionButton(
          ns("edit_lineup"),
          "Edit lineup",
          class = "btn-sm btn-outline-secondary"
        )
      )
    })

    shown_violation_sig <- reactiveVal("")
    shown_notice_codes <- reactiveVal(character())
    observeEvent(
      state()$warnings,
      {
        w <- state()$warnings
        p <- partition_warnings(w)
        sig <- paste(.violation_codes(w), collapse = "|")
        if (length(p$violations) && !identical(sig, shown_violation_sig())) {
          showModal(modalDialog(
            title = "Rule violation",
            tags$ul(!!!lapply(p$violations, tags$li)),
            easyClose = TRUE,
            footer = modalButton("Got it")
          ))
        }
        shown_violation_sig(sig)
        notice_items <- Filter(function(x) identical(x$severity, "notice"), w)
        notice_codes <- vapply(notice_items, function(x) x$code, character(1))
        new_codes <- setdiff(notice_codes, shown_notice_codes())
        for (it in notice_items) {
          if (it$code %in% new_codes) {
            showNotification(it$message, type = "message", duration = 4)
          }
        }
        shown_notice_codes(notice_codes) # track current set; a notice re-toasts only if it clears then recurs
      },
      ignoreInit = FALSE
    )

    pending <- reactiveVal(NULL) # list(outcome =, rows =, prefill =)

    commit_payload <- function(payload) {
      evt <- new_event("plate_appearance", payload)
      appended <- storage$append_event(game_id, evt)
      events(c(isolate(events()), list(appended)))
      storage$save_snapshot(game_id, isolate(state()))
    }

    record <- function(outcome) {
      if (isTRUE(read_only)) {
        return(invisible())
      }
      s <- isolate(state())
      if (identical(s$status, "final")) {
        return(invisible())
      }
      res <- resolve_outcome(s$ruleset, s, outcome)
      if (!is.null(res$warning)) {
        showNotification(res$warning$message, type = "warning", duration = 6)
      }
      outcome <- res$outcome
      rows <- disposition_rows(s)
      # Bases empty, or a home run (everyone scores): nothing to ask, commit directly.
      if (length(rows) <= 1L || outcome %in% c("HR", "ITPHR")) {
        prefill <- disposition_prefill(s, outcome)
        commit_payload(disposition_payload(s, outcome, prefill))
        return(invisible())
      }
      prefill <- disposition_prefill(s, outcome)
      pending(list(outcome = outcome, rows = rows, prefill = prefill))
      showModal(disposition_modal_ui(session$ns, rows, prefill, outcome))
    }

    # All outcome codes are reachable, grouped by category as in the Help tab.
    outcomes <- APP_CONFIG$outcome_codes
    lapply(outcomes, function(o) {
      observeEvent(input[[paste0("o_", o)]], record(o), ignoreInit = TRUE)
    })

    observeEvent(
      input$disp_commit,
      {
        pd <- isolate(pending())
        req(pd)
        s <- isolate(state())
        choices <- read_disposition_choices(input, pd$rows)
        v <- validate_disposition(pd$rows, choices)
        if (!v$ok) {
          showModal(disposition_modal_ui(
            session$ns,
            pd$rows,
            choices,
            pd$outcome,
            v$errors
          ))
          return(invisible())
        }
        removeModal()
        pending(NULL)
        commit_payload(disposition_payload(
          s,
          pd$outcome,
          choices
        ))
      },
      ignoreInit = TRUE
    )

    output$action_panel <- renderUI({
      s <- state()
      if (isTRUE(read_only)) {
        return(div(
          class = "p-2 text-muted small",
          "This game is read-only."
        ))
      }
      if (.batting_team_has_lineup(s)) {
        outcome_button_grid(session$ns)
      } else {
        div(
          class = "p-2",
          tags$p(
            class = "text-muted small",
            sprintf(
              "%s is tracked by runs only (no lineup entered).",
              s$batting_team
            )
          ),
          numericInput(
            session$ns("half_runs_n"),
            "Runs this inning",
            value = 0,
            min = 0,
            max = 50
          ),
          actionButton(
            session$ns("half_runs_go"),
            "End half-inning",
            class = "btn-primary bw-outcome-btn"
          )
        )
      }
    })

    # The substitution modal renders every kind's section once and toggles them
    # client-side (conditionalPanel on sub_kind), so opening it is a single
    # showModal with no re-render on type change — no echo-guard needed.
    observeEvent(
      input$sub,
      showModal(substitution_modal_ui(session$ns, isolate(state()), "full")),
      ignoreInit = TRUE
    )

    observeEvent(
      input$sub_commit,
      {
        s <- isolate(state())
        kind <- input$sub_kind %||% "full"
        res <- build_substitution_event(input, s, kind)
        if (!is.null(res$errors)) {
          showModal(substitution_modal_ui(session$ns, s, kind, res$errors))
          return(invisible())
        }
        removeModal()
        appended <- storage$append_event(game_id, res)
        events(c(isolate(events()), list(appended)))
        storage$save_snapshot(game_id, isolate(state()))
      },
      ignoreInit = TRUE
    )

    lineup_team <- reactiveVal("away")
    LINEUP_ROWS <- 12L

    observeEvent(
      input$edit_lineup,
      {
        s <- isolate(state())
        team <- s$batting_team
        lineup_team(team)
        showModal(lineup_modal_ui(
          session$ns,
          s,
          team,
          show_gender = !ruleset_is_genderless(s$ruleset),
          n_rows = LINEUP_ROWS
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$lu_commit,
      {
        s <- isolate(state())
        team <- isolate(lineup_team())
        n <- max(LINEUP_ROWS, length(s$lineups[[team]]) + 3L)
        evt <- build_lineup_set_event(
          input,
          team,
          seq_len(n),
          show_gender = !ruleset_is_genderless(s$ruleset)
        )
        removeModal()
        appended <- storage$append_event(game_id, evt)
        events(c(isolate(events()), list(appended)))
        storage$save_snapshot(game_id, isolate(state()))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$half_runs_go,
      {
        s <- isolate(state())
        if (identical(s$status, "final")) {
          return(invisible())
        }
        evt <- record_half_runs_event(s, input$half_runs_n)
        appended <- storage$append_event(game_id, evt)
        events(c(isolate(events()), list(appended)))
        storage$save_snapshot(game_id, isolate(state()))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$undo,
      {
        ev <- events()
        if (length(ev) > 1) {
          # never drop game_start
          # Rebuild by re-appending all but the last into a fresh guest-ish reload:
          # simplest correct approach — reload, drop last, persist via snapshot only.
          ev <- ev[-length(ev)]
          events(ev)
          storage$save_snapshot(game_id, fold_events(ev))
        }
      },
      ignoreInit = TRUE
    )

    output$situation <- renderUI(situation_card_ui(state()))
    output$scorebook <- renderUI({
      s <- state()
      team <- input$sb_team %||% "away"
      cb <- if (identical(team, s$batting_team)) {
        s$current_batter$player_id
      } else {
        NULL
      }
      render_scorebook_svg(s, team, current_batter_id = cb)
    })

    observeEvent(
      state()$teams,
      {
        t <- state()$teams
        updateRadioButtons(
          session,
          "sb_team",
          choiceNames = c(t$away$name %||% "Away", t$home$name %||% "Home"),
          choiceValues = c("away", "home"),
          selected = isolate(input$sb_team) %||% "away",
          inline = TRUE
        )
      },
      ignoreInit = TRUE,
      once = TRUE
    )
    .box_dt <- function(team) {
      DT::renderDT({
        DT::datatable(
          batting_lines(state(), team),
          rownames = FALSE,
          class = "compact stripe",
          options = list(
            dom = "t",
            paging = FALSE,
            ordering = TRUE,
            order = list(list(0, "asc"))
          )
        )
      })
    }
    output$box_away <- .box_dt("away")
    output$box_home <- .box_dt("home")

    .team_name <- function(team) state()$teams[[team]]$name %||% team
    output$box_away_hdr <- renderUI(tags$h5(class = "mt-2", .team_name("away")))
    output$box_home_hdr <- renderUI(tags$h5(class = "mt-3", .team_name("home")))

    state
  })
}
