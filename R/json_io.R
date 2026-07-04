game_to_json <- function(events, meta = list()) {
  jsonlite::toJSON(list(version = 1L, meta = meta, events = events),
                   auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE)
}

game_from_json <- function(txt) {
  raw <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  events <- lapply(raw$events, function(e) {
    e$seq <- if (is.null(e$seq)) NA_integer_ else as.integer(e$seq)
    e
  })
  list(version = raw$version %||% 1L, meta = raw$meta %||% list(), events = events)
}
