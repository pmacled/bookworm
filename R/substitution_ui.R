# Substitution modal: batting, defensive, and pinch/courtesy runner.
# Validation for pinch runners delegates to evaluate_pinch_runner().

.SUB_KINDS <- c("Batting substitution" = "batting",
                "Defensive substitution" = "defensive",
                "Pinch / courtesy runner" = "courtesy_runner")

.fielding_team <- function(state)
  if (identical(state$batting_team, "away")) "home" else "away"

.occupied_bases <- function(state) {
  occ <- c(first = state$bases$first, second = state$bases$second, third = state$bases$third)
  occ[!is.na(occ)]
}

.player_choices <- function(lineup) {
  if (!length(lineup)) return(character(0))
  stats::setNames(vapply(lineup, function(p) p$player_id, character(1)),
                  vapply(lineup, function(p) paste0(
                    if (!is.na(p$jersey_number)) paste0("#", p$jersey_number, " ") else "",
                    p$name), character(1)))
}

.incoming_fields <- function(ns, show_position) {
  tagList(
    tags$h6("Incoming player"),
    layout_columns(col_widths = c(6, 3, 3),
      textInput(ns("sub_name"), "Name"),
      selectInput(ns("sub_gender"), "Gender", c("M" = "M", "F" = "F")),
      textInput(ns("sub_jersey"), "Jersey")),
    if (show_position)
      selectInput(ns("sub_pos"), "Position",
        c("(no position)" = "", stats::setNames(APP_CONFIG$positions, APP_CONFIG$positions))))
}

substitution_modal_ui <- function(ns, state, kind, errors = character()) {
  body <- if (identical(kind, "batting")) {
    slots <- sort(unique(unlist(lapply(c("away", "home"), function(t)
      vapply(Filter(function(p) !is.na(p$order_slot), state$lineups[[t]]),
             function(p) p$order_slot, integer(1))))))
    tagList(
      selectInput(ns("sub_team"), "Team",
        stats::setNames(c("away", "home"),
          c(state$teams$away$name %||% "Away", state$teams$home$name %||% "Home"))),
      selectInput(ns("sub_slot"), "Batting order slot",
        stats::setNames(as.character(slots), paste("Slot", slots))),
      .incoming_fields(ns, show_position = TRUE))
  } else if (identical(kind, "defensive")) {
    ft <- .fielding_team(state)
    tagList(
      tags$p(class = "text-muted small",
        sprintf("Fielding team: %s", state$teams[[ft]]$name %||% ft)),
      selectInput(ns("sub_out"), "Player coming out", .player_choices(state$lineups[[ft]])),
      .incoming_fields(ns, show_position = TRUE))
  } else {
    occ <- .occupied_bases(state)
    if (!length(occ))
      tags$p(class = "text-muted", "No runners on base.")
    else tagList(
      selectInput(ns("sub_base"), "Runner",
        stats::setNames(names(occ), paste0(names(occ), " — ",
          vapply(occ, function(id) {
            p <- Filter(function(q) identical(q$player_id, id),
                        state$lineups[[state$batting_team]])
            if (length(p)) p[[1]]$name else id
          }, character(1))))),
      .incoming_fields(ns, show_position = FALSE))
  }

  modalDialog(
    title = names(.SUB_KINDS)[.SUB_KINDS == kind],
    selectInput(ns("sub_kind"), "Type", .SUB_KINDS, selected = kind),
    tags$hr(), body,
    if (length(errors))
      tags$div(class = "alert alert-danger py-2 small mt-2",
        tags$ul(class = "mb-0", !!!lapply(errors, tags$li))),
    footer = tagList(modalButton("Cancel"),
      actionButton(ns("sub_commit"), "Make substitution", class = "btn-primary")),
    easyClose = FALSE)
}

build_substitution_event <- function(input, state, kind) {
  nm <- trimws(input$sub_name %||% "")
  if (!nzchar(nm)) return(list(errors = "Enter the incoming player's name."))
  incoming <- make_player(uuid::UUIDgenerate(), nm, input$sub_gender %||% "M",
    jersey_number = .parse_jersey(input$sub_jersey),
    order_slot = NA_integer_,
    position = { p <- input$sub_pos %||% ""; if (nzchar(p)) p else NA_character_ })

  if (identical(kind, "batting")) {
    team <- input$sub_team %||% state$batting_team
    slot <- suppressWarnings(as.integer(input$sub_slot %||% NA))
    if (is.na(slot)) return(list(errors = "Choose a batting order slot."))
    return(new_event("substitution", list(team = team, kind = "batting",
      order_slot = slot, in_player = incoming)))
  }

  if (identical(kind, "defensive")) {
    ft <- .fielding_team(state)
    out_id <- input$sub_out %||% ""
    if (!nzchar(out_id)) return(list(errors = "Choose the player coming out."))
    return(new_event("substitution", list(team = ft, kind = "defensive",
      out_player_id = out_id,
      position = { p <- input$sub_pos %||% ""; if (nzchar(p)) p else NA_character_ },
      in_player = incoming)))
  }

  base <- input$sub_base %||% ""
  out_id <- state$bases[[base]] %||% NA_character_
  if (!nzchar(base) || is.na(out_id))
    return(list(errors = "Choose a runner to replace."))
  out_player <- Filter(function(p) identical(p$player_id, out_id),
                       state$lineups[[state$batting_team]])
  out_player <- if (length(out_player)) out_player[[1]] else
    make_player(out_id, out_id, "M", NA_integer_, NA_integer_, NA_character_)

  v <- evaluate_pinch_runner(state$ruleset, state, out_player, incoming)
  # evaluate_pinch_runner returns list(ok =, items = list(list(severity=, code=, message=))).
  if (!v$ok) return(list(errors = vapply(v$items, function(i) i$message, character(1))))

  new_event("substitution", list(team = state$batting_team, kind = "courtesy_runner",
    out_player_id = out_id, in_player = incoming))
}
