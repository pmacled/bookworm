# Substitution modal: full, batting, defensive, and pinch/courtesy runner.
# The modal is rendered ONCE with every kind's section present; switching type
# toggles sections client-side via conditionalPanel keyed on input$sub_kind, so
# changing type never re-renders the modal or clears the other fields.
# Validation for pinch runners delegates to evaluate_pinch_runner().

.SUB_KINDS <- c(
  "Full substitution" = "full",
  "Batting substitution" = "batting",
  "Defensive substitution" = "defensive",
  "Pinch / courtesy runner" = "courtesy_runner"
)

.fielding_team <- function(state) {
  if (identical(state$batting_team, "away")) "home" else "away"
}

.occupied_bases <- function(state) {
  occ <- c(
    first = state$bases$first,
    second = state$bases$second,
    third = state$bases$third
  )
  occ[!is.na(occ)]
}

.player_choices <- function(lineup) {
  if (!length(lineup)) {
    return(character(0))
  }
  stats::setNames(
    vapply(lineup, function(p) p$player_id, character(1)),
    vapply(
      lineup,
      function(p) {
        paste0(
          if (!is.na(p$jersey_number)) {
            paste0("#", p$jersey_number, " ")
          } else {
            ""
          },
          p$name
        )
      },
      character(1)
    )
  )
}

.team_choices <- function(state) {
  stats::setNames(
    c("away", "home"),
    c(
      state$teams$away$name %||% "Away",
      state$teams$home$name %||% "Home"
    )
  )
}

# Incoming-player fields. `prefix` namespaces the inputs per kind so the four
# sections never share input ids (which would let a hidden section overwrite the
# active one). show_position adds a position select.
.incoming_fields <- function(ns, prefix, show_position) {
  tagList(
    tags$h6("Incoming player"),
    layout_columns(
      col_widths = c(6, 3, 3),
      textInput(ns(paste0(prefix, "_name")), "Name"),
      selectInput(
        ns(paste0(prefix, "_gender")),
        "Gender",
        c("M" = "M", "F" = "F")
      ),
      textInput(ns(paste0(prefix, "_jersey")), "Jersey")
    ),
    if (show_position) {
      selectInput(
        ns(paste0(prefix, "_pos")),
        "Position",
        c(
          "(no position)" = "",
          stats::setNames(APP_CONFIG$positions, APP_CONFIG$positions)
        )
      )
    }
  )
}

# conditionalPanel needs the *namespaced* input name in its JS condition. shiny
# exposes session$ns via ns(); the condition compares input['track-sub_kind'].
.when_kind <- function(ns, kind, ...) {
  conditionalPanel(
    condition = sprintf("input['%s'] == '%s'", ns("sub_kind"), kind),
    ...
  )
}

.section_full <- function(ns, state) {
  # A full sub applies to whichever team the chosen player is on; default the
  # player list to the batting team but let either be picked.
  tagList(
    tags$p(
      class = "text-muted small",
      "Replaces the player everywhere \u2014 same batting-order slot and their spot in the field."
    ),
    selectInput(ns("full_team"), "Team", .team_choices(state)),
    selectInput(
      ns("full_out"),
      "Player coming out",
      .player_choices(state$lineups[[state$batting_team]])
    ),
    .incoming_fields(ns, "full", show_position = TRUE)
  )
}

.section_batting <- function(ns, state) {
  slots <- sort(unique(unlist(lapply(c("away", "home"), function(t) {
    vapply(
      Filter(function(p) !is.na(p$order_slot), state$lineups[[t]]),
      function(p) p$order_slot,
      integer(1)
    )
  }))))
  tagList(
    selectInput(ns("sub_team"), "Team", .team_choices(state)),
    selectInput(
      ns("sub_slot"),
      "Batting order slot",
      stats::setNames(as.character(slots), paste("Slot", slots))
    ),
    .incoming_fields(ns, "sub", show_position = TRUE)
  )
}

.section_defensive <- function(ns, state) {
  ft <- .fielding_team(state)
  tagList(
    tags$p(
      class = "text-muted small",
      sprintf("Fielding team: %s", state$teams[[ft]]$name %||% ft)
    ),
    selectInput(
      ns("sub_out"),
      "Player coming out",
      .player_choices(state$lineups[[ft]])
    ),
    .incoming_fields(ns, "sub", show_position = TRUE)
  )
}

.section_courtesy <- function(ns, state) {
  occ <- .occupied_bases(state)
  if (!length(occ)) {
    return(tags$p(class = "text-muted", "No runners on base."))
  }
  tagList(
    selectInput(
      ns("sub_base"),
      "Runner",
      stats::setNames(
        names(occ),
        paste0(
          names(occ),
          " \u2014 ",
          vapply(
            occ,
            function(id) {
              p <- Filter(
                function(q) identical(q$player_id, id),
                state$lineups[[state$batting_team]]
              )
              if (length(p)) p[[1]]$name else id
            },
            character(1)
          )
        )
      )
    ),
    .incoming_fields(ns, "sub", show_position = FALSE)
  )
}

substitution_modal_ui <- function(
  ns,
  state,
  kind = "full",
  errors = character()
) {
  modalDialog(
    title = "Substitution",
    selectInput(ns("sub_kind"), "Type", .SUB_KINDS, selected = kind),
    tags$hr(),
    .when_kind(ns, "full", .section_full(ns, state)),
    .when_kind(ns, "batting", .section_batting(ns, state)),
    .when_kind(ns, "defensive", .section_defensive(ns, state)),
    .when_kind(ns, "courtesy_runner", .section_courtesy(ns, state)),
    if (length(errors)) {
      tags$div(
        class = "alert alert-danger py-2 small mt-2",
        tags$ul(class = "mb-0", !!!lapply(errors, tags$li))
      )
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("sub_commit"), "Make substitution", class = "btn-primary")
    ),
    easyClose = FALSE
  )
}

# Reads incoming-player fields for a given prefix into a make_player().
.incoming_player <- function(input, prefix, show_position = TRUE) {
  nm <- trimws(input[[paste0(prefix, "_name")]] %||% "")
  if (!nzchar(nm)) {
    return(NULL)
  }
  make_player(
    uuid::UUIDgenerate(),
    nm,
    input[[paste0(prefix, "_gender")]] %||% "M",
    jersey_number = .parse_jersey(input[[paste0(prefix, "_jersey")]]),
    order_slot = NA_integer_,
    position = if (show_position) {
      p <- input[[paste0(prefix, "_pos")]] %||% ""
      if (nzchar(p)) p else NA_character_
    } else {
      NA_character_
    }
  )
}

build_substitution_event <- function(input, state, kind) {
  if (identical(kind, "full")) {
    incoming <- .incoming_player(input, "full")
    if (is.null(incoming)) {
      return(list(errors = "Enter the incoming player's name."))
    }
    team <- input$full_team %||% state$batting_team
    out_id <- input$full_out %||% ""
    if (!nzchar(out_id)) {
      return(list(errors = "Choose the player coming out."))
    }
    return(new_event(
      "substitution",
      list(
        team = team,
        kind = "full",
        out_player_id = out_id,
        position = {
          p <- input$full_pos %||% ""
          if (nzchar(p)) p else NA_character_
        },
        in_player = incoming
      )
    ))
  }

  if (identical(kind, "batting")) {
    incoming <- .incoming_player(input, "sub")
    if (is.null(incoming)) {
      return(list(errors = "Enter the incoming player's name."))
    }
    team <- input$sub_team %||% state$batting_team
    slot <- suppressWarnings(as.integer(input$sub_slot %||% NA))
    if (is.na(slot)) {
      return(list(errors = "Choose a batting order slot."))
    }
    return(new_event(
      "substitution",
      list(
        team = team,
        kind = "batting",
        order_slot = slot,
        in_player = incoming
      )
    ))
  }

  if (identical(kind, "defensive")) {
    incoming <- .incoming_player(input, "sub")
    if (is.null(incoming)) {
      return(list(errors = "Enter the incoming player's name."))
    }
    ft <- .fielding_team(state)
    out_id <- input$sub_out %||% ""
    if (!nzchar(out_id)) {
      return(list(errors = "Choose the player coming out."))
    }
    return(new_event(
      "substitution",
      list(
        team = ft,
        kind = "defensive",
        out_player_id = out_id,
        position = {
          p <- input$sub_pos %||% ""
          if (nzchar(p)) p else NA_character_
        },
        in_player = incoming
      )
    ))
  }

  # courtesy_runner
  incoming <- .incoming_player(input, "sub", show_position = FALSE)
  if (is.null(incoming)) {
    return(list(errors = "Enter the incoming player's name."))
  }
  base <- input$sub_base %||% ""
  out_id <- state$bases[[base]] %||% NA_character_
  if (!nzchar(base) || is.na(out_id)) {
    return(list(errors = "Choose a runner to replace."))
  }
  out_player <- Filter(
    function(p) identical(p$player_id, out_id),
    state$lineups[[state$batting_team]]
  )
  out_player <- if (length(out_player)) {
    out_player[[1]]
  } else {
    make_player(out_id, out_id, "M", NA_integer_, NA_integer_, NA_character_)
  }

  v <- evaluate_pinch_runner(state$ruleset, state, out_player, incoming)
  if (!v$ok) {
    return(list(errors = vapply(v$items, function(i) i$message, character(1))))
  }

  new_event(
    "substitution",
    list(
      team = state$batting_team,
      kind = "courtesy_runner",
      out_player_id = out_id,
      in_player = incoming
    )
  )
}
