library(testthat)
library(shiny)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R","storage.R","tracking_module.R"))
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

test_that("undo then record does not resurrect the undone event", {
  st <- make_storage("guest")
  gid <- st$create_game(list(name = "T"))
  mk <- function(prefix) lapply(1:4, function(i)
    make_player(paste0(prefix, i), paste(prefix, i), c("M","F","M","F")[i], i, i, i))
  gs <- new_event("game_start", list(ruleset = default_ruleset_config(), first_bat = "away",
    home = list(team_id = "H", name = "Home", lineup = mk("h")),
    away = list(team_id = "A", name = "Away", lineup = mk("a"))), seq = 1L)
  shiny::testServer(tracking_server,
    args = list(storage = st, game_id = gid, game_start_event = gs), {
      session$flushReact()          # let ignoreInit observers do their initial skip
                                    # (testServer defers this to the first setInputs otherwise)
      session$setInputs(o_1B = 1)   # a1 to first
      session$setInputs(o_GO = 1)   # a2 grounds out (1 out)
      session$setInputs(undo = 1)   # undo the GO
      session$setInputs(o_K = 1)    # a2 strikes out instead (1 out)
      s <- state()
      expect_equal(s$outs, 1L)            # only ONE out — GO was undone, not double-applied
      expect_equal(s$bases$first, "a1")   # a1 still on first
    })
})
