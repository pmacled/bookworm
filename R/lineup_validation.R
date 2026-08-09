# Validates one team's lineup against a ruleset, for the setup screen's Save button.
# Pure: no Shiny, no state. Returns severity-tagged items for inline display.
# Nothing here blocks the user -- a scorer may knowingly field an illegal lineup.

# Items are {severity, code, message}, matching evaluate_fielding() and
# evaluate_pinch_runner(). `code` is the stable identity the notice machinery
# dedupes on; `message` is user-facing copy and may be reworded freely.
validate_lineup <- function(cfg, lineup, team_label) {
  items <- list()
  add <- function(severity, code, message)
    items[[length(items) + 1]] <<- list(severity = severity, code = code, message = message)

  batters <- Filter(function(p) !is.na(p$order_slot), lineup)
  if (length(batters) == 0L) {
    add("notice", "run_only",
        sprintf("%s has no lineup — it will be tracked by runs per inning.", team_label))
    return(list(ok = TRUE, items = items))
  }
  batters <- batters[order(vapply(batters, function(p) p$order_slot, integer(1)))]

  bs <- cfg$batting_size
  if (!is.na(bs) && length(batters) != bs)
    add("notice", "batting_size", sprintf("%d batters entered; this ruleset expects %d.",
                                          length(batters), bs))

  jerseys <- vapply(batters, function(p) p$jersey_number, integer(1))
  dupe_j <- unique(jerseys[!is.na(jerseys) & duplicated(jerseys)])
  for (j in dupe_j)
    add("notice", "duplicate_jersey", sprintf("Jersey number %d is used more than once.", j))

  # Dedupe names case-insensitively (a scorer typing "sam" and "Sam" almost certainly
  # means the same collision), but report the name using the first entry's original
  # casing rather than the lowercased comparison key.
  raw_names <- trimws(vapply(batters, function(p) p$name, character(1)))
  names_ <- tolower(raw_names)
  for (n in unique(names_[duplicated(names_)])) {
    original <- raw_names[match(n, names_)]
    add("notice", "duplicate_name", sprintf("More than one player is named \"%s\".", original))
  }

  # Batting-order gender rule, checked forwards and then around the turn, because the
  # order repeats: the last batter is followed by the first.
  if (!identical(cfg$batting_gender_rule$type, "none")) {
    g <- vapply(batters, function(p) p$gender, character(1))
    seen <- character()
    for (i in seq_along(g)) {
      if (!next_batter_gender_ok(cfg, seen, g[i])) {
        add("violation", "batting_gender_order",
            sprintf("Batting order: %s (slot %d) breaks the gender rule.",
                    batters[[i]]$name, i))
        break
      }
      seen <- c(seen, g[i])
    }
    # Wrap-around: seed the window with the TAIL of the order and re-check only the
    # head slots whose circular context differs from their forward context. The
    # previous version replayed the whole doubled sequence into one accumulator, so
    # any forward break simply recurred past index length(g) and was re-reported as
    # a wrap break -- on M M M F that produced a second violation claiming "slot 4
    # back to slot 1", which is F -> M and perfectly legal.
    # Slots past `n` have their full lookback inside `g` already, so the forward
    # pass has covered them; only the first `n` can differ.
    n <- cfg$batting_gender_rule$n
    if (!is.na(n) && n >= 1L) {
      seen2 <- utils::tail(g, n)
      for (i in seq_len(min(as.integer(n), length(g)))) {
        if (!next_batter_gender_ok(cfg, seen2, g[i])) {
          add("violation", "batting_gender_wrap", sprintf(
            "Batting order: %s (slot %d) breaks the gender rule where the order wraps around from slot %d.",
            batters[[i]]$name, i, length(g)))
          break
        }
        seen2 <- c(seen2, g[i])
      }
    }
  }

  fielders <- Filter(function(p) !is.na(.position_category(p$position)), lineup)
  fc <- cfg$fielding$fielder_count
  if (!is.na(fc) && length(fielders) > 0L && length(fielders) != fc)
    add("notice", "fielder_count", sprintf("%d fielders assigned; this ruleset expects %d.",
                                           length(fielders), fc))

  for (v in evaluate_fielding(cfg, lineup)) add(v$severity, v$code, v$message)

  list(ok = !any(vapply(items, function(i) identical(i$severity, "violation"), logical(1))),
       items = items)
}
