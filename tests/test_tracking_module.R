library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R","tracking_module.R"))
  source(file.path("R", f))
mk_lineup <- function(prefix) lapply(1:4, function(i)
  make_player(paste0(prefix,i), paste(prefix,i), c("M","F","M","F")[i], i, i, i))
start <- new_event("game_start", list(ruleset=default_ruleset_config(), first_bat="away",
  home=list(team_id="H",name="Home",lineup=mk_lineup("h")),
  away=list(team_id="A",name="Away",lineup=mk_lineup("a"))), seq=1L)

test_that("record_outcome_event for 1B puts batter on first with reached=1", {
  s <- fold_events(list(start))
  e <- record_outcome_event(s, "1B", "away")
  expect_equal(e$payload$reached, 1L)
  expect_equal(e$payload$outs_on_play, 0L)
  s2 <- fold_events(list(start, e))
  expect_equal(s2$bases$first, "a1")
})

test_that("record_outcome_event for K records one out", {
  s <- fold_events(list(start))
  e <- record_outcome_event(s, "K", "away")
  expect_equal(e$payload$outs_on_play, 1L)
  expect_true(is.na(e$payload$reached))
})
