build_game_start_event <- function(ruleset, home, away, first_bat = "away") {
  ruleset <- coerce_ruleset_config(ruleset)
  v <- validate_ruleset_config(ruleset)
  if (!v$ok) {
    stop(paste("Invalid ruleset:", paste(v$errors, collapse = "; ")))
  }
  new_event(
    "game_start",
    list(ruleset = ruleset, first_bat = first_bat, home = home, away = away),
    seq = 1L
  )
}

.parse_jersey <- function(x) {
  if (is.null(x) || length(x) != 1) {
    return(NA_integer_)
  }
  x <- trimws(as.character(x))
  if (!nzchar(x) || is.na(x) || !grepl("^[0-9]+$", x)) {
    return(NA_integer_)
  }
  as.integer(x)
}

collect_lineup <- function(input, prefix, row_ids, show_gender = TRUE) {
  players <- list()
  for (id in row_ids) {
    nm <- trimws(input[[paste0(prefix, "_name_", id)]] %||% "")
    if (!nzchar(nm)) {
      next
    }
    pos <- input[[paste0(prefix, "_pos_", id)]] %||% ""
    pos <- if (!nzchar(pos)) NA_character_ else pos
    gender <- if (show_gender) {
      (input[[paste0(prefix, "_gender_", id)]] %||% "M")
    } else {
      "M"
    }
    slot <- length(players) + 1L
    players[[slot]] <- make_player(
      uuid::UUIDgenerate(),
      nm,
      gender,
      jersey_number = .parse_jersey(input[[paste0(prefix, "_jersey_", id)]]),
      order_slot = slot,
      position = pos
    )
  }
  players
}

# A numeric box where 0 means "no limit": empty or non-positive reads as NA.
.pos_or_na <- function(x) {
  if (is.null(x) || length(x) != 1 || is.na(x) || x <= 0) {
    NA_integer_
  } else {
    as.integer(x)
  }
}

# A numeric box where 0 is a real value and means "no requirement" (the fielding
# gender minimums). A cleared numericInput submits NA, and these three used to be
# read with a bare `%||%`, which only catches NULL -- so the NA sailed through
# collect_ruleset, past validate_ruleset_config (which validates nothing under
# `fielding`), and into the persisted game_start event, where it made every
# subsequent evaluate_fielding() throw and the game permanently unloadable.
.count_or_zero <- function(x) {
  if (is.null(x) || length(x) != 1 || is.na(x) || x < 0) 0L else as.integer(x)
}

collect_ruleset <- function(input) {
  fielding <- switch(
    input$fielding_preset %||% "none",
    "standard_coed" = STANDARD_COED_FIELDING,
    "custom" = list(
      min_females = .count_or_zero(input$min_females),
      max_males = .pos_or_na(input$max_males),
      tiers = list(list(
        females = 0L,
        outfield = .count_or_zero(input$of_females),
        infield = .count_or_zero(input$if_females),
        battery = input$battery_mode %||% "any"
      )),
      position_requirements = list()
    ),
    list(
      min_females = 0L,
      max_males = NA_integer_,
      tiers = list(),
      position_requirements = list()
    )
  )
  fielding$fielder_count <- .pos_or_na(input$fielder_count)

  mercy <- Filter(
    Negate(is.null),
    lapply(1:3, function(i) {
      d <- .pos_or_na(input[[paste0("mercy_diff_", i)]])
      if (is.na(d)) {
        return(NULL)
      }
      list(
        after_inning = as.integer(input[[paste0("mercy_after_", i)]] %||% 1L),
        differential = d
      )
    })
  )

  by_gender <- Filter(
    Negate(is.na),
    list(
      M = .pos_or_na(input$hr_limit_m),
      F = .pos_or_na(input$hr_limit_f)
    )
  )

  gender_type <- input$gender_rule %||% "none"
  preset_id <- input$preset %||% "anything_goes"
  cfg <- coerce_ruleset_config(list(
    preset = preset_id,
    starting_count = list(
      balls = input$start_balls,
      strikes = input$start_strikes
    ),
    foul_out_rule = input$foul_out,
    batting_gender_rule = list(
      type = gender_type,
      n = if (identical(gender_type, "none")) {
        NA_integer_
      } else {
        (input$gender_n %||% 1L)
      }
    ),
    batting_size = as.integer(input$batting_size %||% "0"), # selectInput string; coerce maps 0/NA => unlimited
    batting_size_rule = input$batting_size_rule %||% "max",
    fielding = fielding,
    innings = input$innings,
    run_cap = list(
      per_inning = .pos_or_na(input$run_cap),
      open_last_inning = isTRUE(input$open_last),
      same_play_runs_count = isTRUE(input$cap_same_play),
      cap_ends_half = isTRUE(input$cap_ends_half)
    ),
    mercy_rule = list(tiers = mercy),
    home_run_rule = list(
      over_fence_limit = .pos_or_na(input$hr_limit),
      limit_by_gender = by_gender,
      over_limit_result = input$hr_over %||% "out",
      inside_park_counts = isTRUE(input$hr_itp_counts)
    ),
    pinch_runner = list(
      max_per_inning = .pos_or_na(input$pr_inning),
      max_per_game = .pos_or_na(input$pr_game),
      max_per_player_per_game = .pos_or_na(input$pr_player),
      eligibility = input$pr_elig %||% "anyone",
      allowed_for = input$pr_for %||% "anyone"
    )
  ))

  # `preset` is persisted into game_start and the README's roadmap ("presents it
  # as a diff against the closest built-in preset") depends on it being true.
  # Recording the picker's value verbatim makes it a lie the moment the user edits
  # any Advanced control, so compare what was actually collected against the named
  # preset and downgrade to "custom" when they differ. An unrecognised id (e.g. a
  # saved league ruleset) has nothing to compare against, so it passes through.
  base <- tryCatch(preset_ruleset(preset_id), error = function(e) NULL)
  if (!is.null(base)) {
    a <- cfg
    a$preset <- NULL
    b <- base
    b$preset <- NULL
    if (!identical(a, b)) cfg$preset <- "custom"
  }
  cfg
}

.POS_CHOICES <- function() {
  c("(pos)" = "", stats::setNames(APP_CONFIG$positions, APP_CONFIG$positions))
}

.lineup_table_head <- function(show_gender) {
  cols <- c(
    "#",
    "Name",
    if (show_gender) "Gender",
    "Jersey",
    "Position",
    "",
    ""
  )
  tags$thead(tags$tr(!!!lapply(cols, function(h) tags$th(scope = "col", h))))
}

.lineup_ui <- function(ns, prefix, title, show_gender = TRUE) {
  tagList(
    tags$h5(title),
    tags$p(
      class = "text-muted small",
      "Leave this lineup empty to just record this team's runs each inning."
    ),
    tags$div(
      class = "bw-lineup-wrap",
      tags$table(
        class = "table table-sm bw-lineup",
        .lineup_table_head(show_gender),
        tags$tbody(id = ns(paste0(prefix, "_rows")))
      )
    ),
    div(
      class = "d-flex gap-2",
      actionButton(
        ns(paste0(prefix, "_add")),
        "Add player",
        class = "btn-sm btn-outline-secondary"
      ),
      actionButton(
        ns(paste0(prefix, "_save")),
        "Save lineup",
        class = "btn-sm btn-primary"
      )
    ),
    uiOutput(ns(paste0(prefix, "_validation")))
  )
}

setup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("New game"),

    tags$h4(class = "mt-3", "1. Rules"),
    selectInput(
      ns("preset"),
      "Ruleset",
      preset_choices(),
      selected = "anything_goes"
    ),
    uiOutput(ns("preset_desc")),
    tags$div(
      class = "bw-rules",
      accordion(
        open = FALSE,
        accordion_panel(
          "Advanced rules",
          layout_columns(
            col_widths = c(6, 6),
            numericInput(ns("start_balls"), "Starting balls", 0, 0, 3),
            numericInput(ns("start_strikes"), "Starting strikes", 0, 0, 2)
          ),
          selectInput(
            ns("foul_out"),
            "Foul with 2 strikes",
            c(
              "Out" = "out",
              "One courtesy foul" = "one_courtesy_foul",
              "Unlimited (never an out)" = "unlimited"
            ),
            selected = "unlimited"
          ),
          selectInput(
            ns("batting_size"),
            "Number of batters",
            c("Unlimited (everyone bats)" = "0", "9" = "9", "10" = "10")
          ),
          conditionalPanel(
            sprintf("input['%s'] != '0'", ns("batting_size")),
            selectInput(
              ns("batting_size_rule"),
              "Batting size enforcement",
              c(
                "Maximum (short lineups allowed)" = "max",
                "Exact (must match exactly)" = "exact"
              )
            )
          ),
          selectInput(
            ns("gender_rule"),
            "Batting gender rule",
            c(
              "None" = "none",
              "Max males in a row" = "max_consecutive_males",
              "Max of either gender in a row" = "max_consecutive_same_gender"
            )
          ),
          conditionalPanel(
            sprintf("input['%s'] != 'none'", ns("gender_rule")),
            numericInput(ns("gender_n"), "N", 1, 1, 12)
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            numericInput(ns("innings"), "Innings", 7, 1, 12),
            numericInput(ns("fielder_count"), "Fielders (0 = any)", 0, 0, 12),
            numericInput(ns("run_cap"), "Run cap/inning (0 = none)", 0, 0, 30)
          ),
          layout_columns(
            col_widths = c(6, 6),
            checkboxInput(
              ns("cap_same_play"),
              "Runs on the same play all count",
              TRUE
            ),
            checkboxInput(
              ns("cap_ends_half"),
              "Reaching the cap ends the half-inning",
              TRUE
            )
          ),
          checkboxInput(ns("open_last"), "No cap in the last inning", TRUE)
        ),

        accordion_panel(
          "Mercy rule",
          tags$p(
            class = "text-muted small",
            "The game ends as soon as any row is satisfied. Leave a differential at 0 to disable that row."
          ),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(ns("mercy_after_1"), "After inning", 3, 1, 12),
            numericInput(
              ns("mercy_diff_1"),
              "Run differential (0 = off)",
              0,
              0,
              50
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(ns("mercy_after_2"), "After inning", 4, 1, 12),
            numericInput(
              ns("mercy_diff_2"),
              "Run differential (0 = off)",
              0,
              0,
              50
            )
          ),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(ns("mercy_after_3"), "After inning", 5, 1, 12),
            numericInput(
              ns("mercy_diff_3"),
              "Run differential (0 = off)",
              0,
              0,
              50
            )
          )
        ),

        accordion_panel(
          "Home runs",
          layout_columns(
            col_widths = c(4, 4, 4),
            numericInput(
              ns("hr_limit"),
              "Over-the-fence limit (0 = none)",
              0,
              0,
              30
            ),
            numericInput(
              ns("hr_limit_m"),
              "Limit for men (0 = use overall)",
              0,
              0,
              30
            ),
            numericInput(
              ns("hr_limit_f"),
              "Limit for women (0 = use overall)",
              0,
              0,
              30
            )
          ),
          selectInput(
            ns("hr_over"),
            "A home run past the limit is",
            c(
              "An out" = "out",
              "A ground-rule double" = "ground_rule_double",
              "A single" = "single"
            )
          ),
          checkboxInput(
            ns("hr_itp_counts"),
            "Inside-the-park home runs count toward the limit",
            FALSE
          )
        ),

        accordion_panel(
          "Pinch / courtesy runners",
          layout_columns(
            col_widths = c(4, 4, 4),
            numericInput(
              ns("pr_inning"),
              "Max per inning (0 = unlimited)",
              0,
              0,
              12
            ),
            numericInput(
              ns("pr_game"),
              "Max per game (0 = unlimited)",
              0,
              0,
              30
            ),
            numericInput(
              ns("pr_player"),
              "Max per player (0 = unlimited)",
              0,
              0,
              12
            )
          ),
          selectInput(
            ns("pr_elig"),
            "Who may run",
            c(
              "Anyone" = "anyone",
              "Same gender" = "same_gender",
              "The last out" = "last_out",
              "The last same-gender out" = "last_same_gender_out"
            )
          ),
          selectInput(
            ns("pr_for"),
            "Who may be run for",
            c(
              "Anyone" = "anyone",
              "Pitcher or catcher only" = "pitcher_catcher"
            )
          )
        ),

        accordion_panel(
          "Fielding gender rules",
          selectInput(
            ns("fielding_preset"),
            "Preset",
            c(
              "None" = "none",
              "Standard coed (10-player)" = "standard_coed",
              "Custom" = "custom"
            )
          ),
          conditionalPanel(
            sprintf("input['%s'] == 'custom'", ns("fielding_preset")),
            layout_columns(
              col_widths = c(6, 6),
              numericInput(ns("min_females"), "Min females in field", 0, 0, 10),
              numericInput(
                ns("max_males"),
                "Max males in field (0 = none)",
                0,
                0,
                12
              )
            ),
            layout_columns(
              col_widths = c(4, 4, 4),
              numericInput(ns("of_females"), "Min F outfield", 0, 0, 5),
              numericInput(ns("if_females"), "Min F infield", 0, 0, 5),
              selectInput(
                ns("battery_mode"),
                "Pitcher/Catcher",
                c("Any" = "any", "Opposite genders" = "one")
              )
            )
          )
        )
      )
    ),

    tags$h4(class = "mt-4", "2. Teams"),
    textInput(ns("away_name"), "Away team", "Away"),
    uiOutput(ns("away_lineup")),
    textInput(ns("home_name"), "Home team", "Home"),
    uiOutput(ns("home_lineup")),

    actionButton(
      ns("start"),
      "Start game",
      class = "btn-primary bw-outcome-btn mt-3"
    )
  )
}

.player_row <- function(ns, prefix, id, order, show_gender, values = NULL) {
  v <- values %||% list()
  cell <- function(...) tags$td(class = "bw-cell", ...)
  jersey_val <- if (is.null(v$jersey) || is.na(v$jersey)) {
    ""
  } else {
    as.character(v$jersey)
  }
  pos_val <- if (is.null(v$position) || is.na(v$position)) {
    ""
  } else {
    as.character(v$position)
  }
  tags$tr(
    id = ns(paste0(prefix, "_row_", id)),
    tags$td(class = "bw-order", order),
    cell(textInput(
      ns(paste0(prefix, "_name_", id)),
      NULL,
      value = v$name %||% "",
      placeholder = "Name"
    )),
    if (show_gender) {
      cell(selectInput(
        ns(paste0(prefix, "_gender_", id)),
        NULL,
        c("M" = "M", "F" = "F"),
        selected = v$gender %||% "M",
        width = "5rem"
      ))
    },
    cell(tagAppendAttributes(
      textInput(
        ns(paste0(prefix, "_jersey_", id)),
        NULL,
        value = jersey_val,
        placeholder = "#",
        width = "5rem"
      ),
      inputmode = "numeric",
      .cssSelector = "input"
    )),
    cell(selectInput(
      ns(paste0(prefix, "_pos_", id)),
      NULL,
      .POS_CHOICES(),
      selected = pos_val,
      width = "7rem"
    )),
    cell(actionButton(
      ns(paste0(prefix, "_del_", id)),
      "×",
      class = "btn-sm btn-outline-danger"
    )),
    cell(tags$div(
      class = "bw-move btn-group-vertical",
      actionButton(
        ns(paste0(prefix, "_up_", id)),
        HTML("&#9650;"),
        class = "btn-sm btn-outline-secondary bw-move-btn"
      ),
      actionButton(
        ns(paste0(prefix, "_down_", id)),
        HTML("&#9660;"),
        class = "btn-sm btn-outline-secondary bw-move-btn"
      )
    ))
  )
}

setup_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    game_start <- reactiveVal(NULL)
    rows <- list(away = reactiveVal(integer()), home = reactiveVal(integer()))
    counter <- reactiveVal(0L)

    # The chosen ruleset, read live so the lineup table can drop its gender column.
    # No tryCatch: collect_ruleset() reads every input through an NA-safe helper and
    # hands the result to coerce_ruleset_config(), which merges rather than
    # validates -- it has no throwing path. (test_setup_module.R pins that.)
    ruleset <- reactive(collect_ruleset(input))

    # show_gender is exposed as a reactiveVal, not a plain reactive(), and only ever
    # *set* when the boolean actually flips. A plain `reactive(!ruleset_is_genderless(ruleset()))`
    # invalidates its dependents every time ruleset() re-evaluates -- i.e. on every
    # keystroke anywhere in the rules panel -- which would blow away names already
    # typed into the lineup tables when the shells below re-render. reactiveVal only
    # notifies dependents when the stored value changes, so gating the write behind
    # this reactive keeps the lineup shells stable across unrelated rule edits.
    show_gender_val <- reactiveVal(
      !ruleset_is_genderless(default_ruleset_config())
    )
    observe({
      new_val <- !ruleset_is_genderless(ruleset())
      if (!identical(new_val, isolate(show_gender_val()))) {
        # The lineup shells (output$away_lineup / output$home_lineup, below) are about
        # to re-render because the gender column is appearing or disappearing. That
        # renderUI() call regenerates `.lineup_ui()` -- an *empty* `<tbody>` -- and any
        # rows added since via insertUI() live only in the tbody that is about to be
        # thrown away. `rows[[prefix]]()` and the row-scoped inputs (away_name_3, ...)
        # would otherwise survive the re-render as phantoms: invisible, un-deletable,
        # yet still read by collect_lineup() the next time it runs off the *old*
        # row-id list, silently defaulting a stale player's gender via `%||% "M"`.
        # Reset both row lists so no stale id is ever collected again, and tell the
        # user their entries were cleared rather than silently keep or drop them.
        had_rows <- length(isolate(rows$away())) > 0L ||
          length(isolate(rows$home())) > 0L
        show_gender_val(new_val)
        rows$away(integer())
        rows$home(integer())
        if (had_rows) {
          showNotification(
            "The ruleset change altered the lineup table's columns, so both lineups were cleared. Please re-enter the players.",
            type = "warning",
            duration = 8
          )
        }
      }
    })
    show_gender <- show_gender_val

    output$preset_desc <- renderUI({
      p <- RULE_PRESETS[[input$preset %||% "anything_goes"]]
      req(p)
      tags$p(class = "text-muted small", p$description)
    })

    # Applying a preset writes its values into the advanced controls, so the two views
    # never disagree. Editing a control afterwards simply changes the effective ruleset.
    observeEvent(
      input$preset,
      {
        cfg <- preset_ruleset(input$preset %||% "anything_goes")
        updateNumericInput(
          session,
          "start_balls",
          value = cfg$starting_count$balls
        )
        updateNumericInput(
          session,
          "start_strikes",
          value = cfg$starting_count$strikes
        )
        updateSelectInput(session, "foul_out", selected = cfg$foul_out_rule)
        updateSelectInput(
          session,
          "batting_size",
          selected = as.character(
            if (is.na(cfg$batting_size)) 0L else cfg$batting_size
          )
        )
        updateSelectInput(
          session,
          "batting_size_rule",
          selected = cfg$batting_size_rule %||% "max"
        )
        updateSelectInput(
          session,
          "gender_rule",
          selected = cfg$batting_gender_rule$type
        )
        updateNumericInput(
          session,
          "gender_n",
          value = if (is.na(cfg$batting_gender_rule$n)) {
            1L
          } else {
            cfg$batting_gender_rule$n
          }
        )
        updateNumericInput(session, "innings", value = cfg$innings)
        updateNumericInput(
          session,
          "fielder_count",
          value = if (is.na(cfg$fielding$fielder_count)) {
            0L
          } else {
            cfg$fielding$fielder_count
          }
        )
        updateNumericInput(
          session,
          "run_cap",
          value = if (is.na(cfg$run_cap$per_inning)) {
            0L
          } else {
            cfg$run_cap$per_inning
          }
        )
        updateCheckboxInput(
          session,
          "cap_same_play",
          value = cfg$run_cap$same_play_runs_count
        )
        updateCheckboxInput(
          session,
          "cap_ends_half",
          value = cfg$run_cap$cap_ends_half
        )
        updateCheckboxInput(
          session,
          "open_last",
          value = cfg$run_cap$open_last_inning
        )
        for (i in 1:3) {
          tiers <- cfg$mercy_rule$tiers
          # Guard the index: an empty (or short) tiers list is the common case (most
          # presets have no mercy rule), and `tiers[[i]]` on an out-of-range index
          # throws "subscript out of bounds" rather than returning NULL -- so %||%
          # never gets a chance to supply the fallback.
          t <- if (i <= length(tiers)) tiers[[i]] else NULL
          updateNumericInput(
            session,
            paste0("mercy_after_", i),
            value = if (is.null(t)) c(3L, 4L, 5L)[i] else t$after_inning
          )
          updateNumericInput(
            session,
            paste0("mercy_diff_", i),
            value = if (is.null(t)) 0L else t$differential
          )
        }
        hr <- cfg$home_run_rule
        updateNumericInput(
          session,
          "hr_limit",
          value = if (is.na(hr$over_fence_limit)) 0L else hr$over_fence_limit
        )
        updateNumericInput(
          session,
          "hr_limit_m",
          value = hr$limit_by_gender$M %||% 0L
        )
        updateNumericInput(
          session,
          "hr_limit_f",
          value = hr$limit_by_gender$F %||% 0L
        )
        updateSelectInput(session, "hr_over", selected = hr$over_limit_result)
        updateCheckboxInput(
          session,
          "hr_itp_counts",
          value = hr$inside_park_counts
        )
        pr <- cfg$pinch_runner
        pr_map <- list(
          pr_inning = "max_per_inning",
          pr_game = "max_per_game",
          pr_player = "max_per_player_per_game"
        )
        for (input_id in names(pr_map)) {
          v <- pr[[pr_map[[input_id]]]]
          updateNumericInput(session, input_id, value = if (is.na(v)) 0L else v)
        }
        updateSelectInput(session, "pr_elig", selected = pr$eligibility)
        updateSelectInput(session, "pr_for", selected = pr$allowed_for)
        updateSelectInput(
          session,
          "fielding_preset",
          selected = if (length(cfg$fielding$tiers)) "standard_coed" else "none"
        )
      },
      ignoreInit = FALSE
    )

    # Lineup shells re-render only when the gender column's presence changes, so typing
    # in the rules panel does not wipe entered names.
    output$away_lineup <- renderUI(.lineup_ui(
      ns,
      "away",
      "Away lineup",
      show_gender()
    ))
    output$home_lineup <- renderUI(.lineup_ui(
      ns,
      "home",
      "Home lineup",
      show_gender()
    ))

    # Reads a row's currently-entered values so the tbody can be re-rendered without
    # losing what the user typed (re-rendering a Shiny input resets it otherwise).
    .row_values <- function(prefix, id) {
      list(
        name = input[[paste0(prefix, "_name_", id)]] %||% "",
        gender = input[[paste0(prefix, "_gender_", id)]] %||% "M",
        jersey = .parse_jersey(input[[paste0(prefix, "_jersey_", id)]]),
        position = {
          p <- input[[paste0(prefix, "_pos_", id)]] %||% ""
          if (nzchar(p)) p else NA_character_
        }
      )
    }

    # Re-renders the whole tbody in the current id order, pre-filled from live inputs.
    .rerender_rows <- function(prefix) {
      ids <- rows[[prefix]]()
      sg <- isolate(show_gender())
      body <- lapply(seq_along(ids), function(i) {
        .player_row(
          ns,
          prefix,
          ids[i],
          order = i,
          show_gender = sg,
          values = .row_values(prefix, ids[i])
        )
      })
      removeUI(
        sprintf("#%s > *", ns(paste0(prefix, "_rows"))),
        multiple = TRUE,
        immediate = TRUE
      )
      insertUI(
        sprintf("#%s", ns(paste0(prefix, "_rows"))),
        where = "beforeEnd",
        ui = tagList(!!!body),
        immediate = TRUE
      )
    }

    # Swaps a row with its neighbour in the given direction and re-renders.
    .move_row <- function(prefix, id, dir) {
      ids <- rows[[prefix]]()
      i <- match(id, ids)
      j <- i + dir
      if (is.na(i) || j < 1L || j > length(ids)) {
        return(invisible())
      }
      ids[c(i, j)] <- ids[c(j, i)]
      rows[[prefix]](ids)
      .rerender_rows(prefix)
    }

    add_row <- function(prefix) {
      counter(counter() + 1L)
      id <- counter()
      rows[[prefix]](c(rows[[prefix]](), id))
      insertUI(
        sprintf("#%s", ns(paste0(prefix, "_rows"))),
        where = "beforeEnd",
        ui = .player_row(
          ns,
          prefix,
          id,
          order = length(rows[[prefix]]()),
          show_gender = isolate(show_gender())
        )
      )
      observeEvent(
        input[[paste0(prefix, "_del_", id)]],
        {
          rows[[prefix]](setdiff(rows[[prefix]](), id))
          .rerender_rows(prefix)
        },
        ignoreInit = TRUE
      )
      observeEvent(
        input[[paste0(prefix, "_up_", id)]],
        .move_row(prefix, id, -1L),
        ignoreInit = TRUE
      )
      observeEvent(
        input[[paste0(prefix, "_down_", id)]],
        .move_row(prefix, id, 1L),
        ignoreInit = TRUE
      )
    }
    observeEvent(input$away_add, add_row("away"), ignoreInit = TRUE)
    observeEvent(input$home_add, add_row("home"), ignoreInit = TRUE)

    .validation_ui <- function(prefix, label) {
      cfg <- ruleset()
      lu <- collect_lineup(
        input,
        prefix,
        rows[[prefix]](),
        show_gender = show_gender()
      )
      r <- validate_lineup(cfg, lu, label)
      cls <- function(sev) {
        switch(
          sev,
          violation = "text-danger",
          notice = "text-warning",
          "text-muted"
        )
      }
      div(
        class = "small mt-1",
        tags$div(
          class = if (r$ok) "text-success" else "text-danger",
          sprintf(
            "%d batter(s) saved.%s",
            length(lu),
            if (r$ok) "" else " Rule problems below."
          )
        ),
        !!!lapply(r$items, function(i) {
          tags$div(class = cls(i$severity), i$message)
        })
      )
    }
    observeEvent(
      input$away_save,
      output$away_validation <- renderUI(.validation_ui(
        "away",
        input$away_name %||% "Away"
      )),
      ignoreInit = TRUE
    )
    observeEvent(
      input$home_save,
      output$home_validation <- renderUI(.validation_ui(
        "home",
        input$home_name %||% "Home"
      )),
      ignoreInit = TRUE
    )

    observeEvent(input$start, {
      cfg <- collect_ruleset(input)
      sg <- show_gender()
      away <- list(
        team_id = uuid::UUIDgenerate(),
        name = input$away_name,
        lineup = collect_lineup(input, "away", rows$away(), show_gender = sg)
      )
      home <- list(
        team_id = uuid::UUIDgenerate(),
        name = input$home_name,
        lineup = collect_lineup(input, "home", rows$home(), show_gender = sg)
      )
      output$away_validation <- renderUI(.validation_ui(
        "away",
        input$away_name %||% "Away"
      ))
      output$home_validation <- renderUI(.validation_ui(
        "home",
        input$home_name %||% "Home"
      ))
      # An unhandled error here does not just fail the click: Shiny tears the whole
      # session down with a generic "An error has occurred", and the perfectly good
      # message build_game_start_event() computed goes to the log where the scorer
      # will never see it. A cleared numeric box is enough to trigger it, so the
      # reason has to come back to the user instead.
      evt <- tryCatch(
        build_game_start_event(cfg, home, away, "away"),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 12)
          NULL
        }
      )
      if (!is.null(evt)) game_start(evt)
    })
    game_start
  })
}
