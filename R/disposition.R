# Runner disposition: what happened to every runner on the play.
# Pure — no Shiny. The tracking module renders these rows and collects the choices;
# every rule about what a legal disposition is lives here.

DISPOSITION_LEVELS <- c("1", "2", "3", "H", "OUT")

# Outcomes where the batter is out unless the scorer says otherwise. SF and SAC are
# outs for the batter even though they are not in .OUT_OUTCOMES (they are not at-bats).
.BATTER_OUT_OUTCOMES <- c("K", "KL", "GO", "FO", "LO", "PO", "SF", "SAC")

.BASE_FROM <- c(first = 1L, second = 2L, third = 3L)

.lookup_player <- function(state, team, player_id) {
  hit <- Filter(
    function(p) identical(p$player_id, player_id),
    state$lineups[[team]] %||% list()
  )
  if (length(hit)) hit[[1]] else NULL
}

# Lead runner first (third, second, first), batter last with from = 0.
# Lead-first matters: it is the order a scorer reads the field in.
disposition_rows <- function(state) {
  team <- state$batting_team
  rows <- list()
  for (base in c("third", "second", "first")) {
    id <- state$bases[[base]]
    if (is.null(id) || is.na(id)) {
      next
    }
    p <- .lookup_player(state, team, id)
    rows[[length(rows) + 1L]] <- list(
      runner_id = id,
      from = unname(.BASE_FROM[[base]]),
      name = p$name %||% id,
      jersey = p$jersey_number %||% NA_integer_
    )
  }
  b <- state$current_batter
  if (!is.null(b)) {
    rows[[length(rows) + 1L]] <- list(
      runner_id = b$player_id,
      from = 0L,
      name = b$name,
      jersey = b$jersey_number %||% NA_integer_
    )
  }
  rows
}

# Pre-fill from suggest_advances(). That function is no longer authoritative — it only
# saves the scorer taps on the common case.
disposition_prefill <- function(state, outcome) {
  by_id <- list()
  for (a in suggest_advances(state, outcome)) {
    by_id[[a$runner_id]] <- if (isTRUE(a$scored)) "H" else as.character(a$to)
  }

  rows <- disposition_rows(state)
  out <- character(0)
  for (r in rows) {
    lvl <- by_id[[r$runner_id]]
    if (is.null(lvl)) {
      lvl <- if (r$from == 0L) {
        if (outcome %in% .BATTER_OUT_OUTCOMES) "OUT" else "1"
      } else {
        as.character(r$from)
      } # runners hold
    }
    out[[r$runner_id]] <- lvl
  }
  out
}

# Indexing a named character vector by an absent name throws "subscript out of
# bounds" rather than returning NULL, so read through a guarded accessor.
.choice_of <- function(choices, id) {
  if (is.null(choices)) {
    return(NULL)
  }
  if (!id %in% names(choices)) {
    return(NULL)
  }
  choices[[id]]
}

validate_disposition <- function(rows, choices) {
  errors <- character()
  add <- function(m) errors <<- c(errors, m)

  for (r in rows) {
    lvl <- .choice_of(choices, r$runner_id)
    if (is.null(lvl) || is.na(lvl) || !nzchar(lvl)) {
      add(sprintf("Say what happened to %s.", r$name))
      next
    }
    if (!lvl %in% DISPOSITION_LEVELS) {
      add(sprintf("%s: \"%s\" is not a valid destination.", r$name, lvl))
      next
    }
    if (lvl %in% c("1", "2", "3") && as.integer(lvl) < r$from) {
      add(sprintf(
        "%s cannot go back from %s to %s.",
        r$name,
        c("home", "first", "second", "third")[r$from + 1L],
        c("first", "second", "third")[as.integer(lvl)]
      ))
    }
  }
  if (length(errors)) {
    return(list(ok = FALSE, errors = errors))
  }

  # Two runners may both score and both be out, but only one may occupy a base.
  placed <- vapply(
    rows,
    function(r) .choice_of(choices, r$runner_id) %||% "",
    character(1)
  )
  on_base <- placed[placed %in% c("1", "2", "3")]
  for (b in unique(on_base[duplicated(on_base)])) {
    add(sprintf(
      "Two runners cannot end on the same base (%s).",
      c("first", "second", "third")[as.integer(b)]
    ))
  }

  list(ok = length(errors) == 0, errors = errors)
}

# Builds the plate_appearance payload. `rbi` defaults to the number of runners who
# scored; the scorer can override it for errors and sacrifices.
disposition_payload <- function(state, outcome, choices, rbi = NULL) {
  rows <- disposition_rows(state)
  advances <- lapply(rows, function(r) {
    lvl <- choices[[r$runner_id]]
    if (identical(lvl, "OUT")) {
      make_advance(r$runner_id, r$from, r$from, out = TRUE)
    } else if (identical(lvl, "H")) {
      make_advance(r$runner_id, r$from, 4L, scored = TRUE)
    } else {
      make_advance(r$runner_id, r$from, as.integer(lvl))
    }
  })
  outs <- sum(vapply(advances, function(a) isTRUE(a$out), logical(1)))
  runs <- sum(vapply(advances, function(a) isTRUE(a$scored), logical(1)))

  batter <- Filter(function(a) identical(a$from, 0L), advances)
  reached <- if (!length(batter)) {
    NA_integer_
  } else if (isTRUE(batter[[1]]$out)) {
    NA_integer_
  } else {
    as.integer(batter[[1]]$to)
  }

  list(
    team = state$batting_team,
    batter_id = state$current_batter$player_id,
    outcome = outcome,
    reached = reached,
    rbi = as.integer(rbi %||% runs),
    outs_on_play = as.integer(outs),
    advances = advances
  )
}

# Applies rule rewrites that depend on game state before the outcome is committed.
# Today that is only the home-run limit.
resolve_outcome <- function(cfg, state, outcome) {
  batter <- state$current_batter
  if (is.null(batter)) {
    return(list(outcome = outcome, warning = NULL))
  }
  evaluate_home_run_limit(cfg, state, batter, outcome)
}
