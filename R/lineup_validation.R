# Validates one team's lineup against a ruleset, for the setup screen's Save button.
# Pure: no Shiny, no state. Returns severity-tagged items for inline display.
# Nothing here blocks the user -- a scorer may knowingly field an illegal lineup.

validate_lineup <- function(cfg, lineup, team_label) {
  items <- list()
  add <- function(severity, message)
    items[[length(items) + 1]] <<- list(severity = severity, message = message)

  batters <- Filter(function(p) !is.na(p$order_slot), lineup)
  if (length(batters) == 0L) {
    add("notice", sprintf("%s has no lineup — it will be tracked by runs per inning.",
                          team_label))
    return(list(ok = TRUE, items = items))
  }
  batters <- batters[order(vapply(batters, function(p) p$order_slot, integer(1)))]

  bs <- cfg$batting_size
  if (!is.na(bs) && length(batters) != bs)
    add("notice", sprintf("%d batters entered; this ruleset expects %d.",
                          length(batters), bs))

  jerseys <- vapply(batters, function(p) p$jersey_number, integer(1))
  dupe_j <- unique(jerseys[!is.na(jerseys) & duplicated(jerseys)])
  for (j in dupe_j) add("notice", sprintf("Jersey number %d is used more than once.", j))

  # Dedupe names case-insensitively (a scorer typing "sam" and "Sam" almost certainly
  # means the same collision), but report the name using the first entry's original
  # casing rather than the lowercased comparison key.
  raw_names <- trimws(vapply(batters, function(p) p$name, character(1)))
  names_ <- tolower(raw_names)
  for (n in unique(names_[duplicated(names_)])) {
    original <- raw_names[match(n, names_)]
    add("notice", sprintf("More than one player is named \"%s\".", original))
  }

  # Batting-order gender rule, checked forwards and then around the turn, because the
  # order repeats: the last batter is followed by the first.
  if (!identical(cfg$batting_gender_rule$type, "none")) {
    g <- vapply(batters, function(p) p$gender, character(1))
    seen <- character()
    for (i in seq_along(g)) {
      if (!next_batter_gender_ok(cfg, seen, g[i])) {
        add("violation", sprintf("Batting order: %s (slot %d) breaks the gender rule.",
                                 batters[[i]]$name, i))
        break
      }
      seen <- c(seen, g[i])
    }
    # Wrap-around: replay the tail of the order into the head.
    wrapped <- c(g, g)
    seen2 <- character(); flagged <- FALSE
    for (i in seq_along(wrapped)) {
      if (!next_batter_gender_ok(cfg, seen2, wrapped[i]) && i > length(g)) {
        add("violation", sprintf(
          "Batting order: the order breaks the gender rule where it wraps around (slot %d back to slot 1).",
          length(g)))
        flagged <- TRUE
        break
      }
      seen2 <- c(seen2, wrapped[i])
      if (flagged) break
    }
  }

  fielders <- Filter(function(p) !is.na(.position_category(p$position)), lineup)
  fc <- cfg$fielding$fielder_count
  if (!is.na(fc) && length(fielders) > 0L && length(fielders) != fc)
    add("notice", sprintf("%d fielders assigned; this ruleset expects %d.",
                          length(fielders), fc))

  for (v in evaluate_fielding(cfg, lineup)) add(v$severity, v$message)

  list(ok = !any(vapply(items, function(i) identical(i$severity, "violation"), logical(1))),
       items = items)
}
