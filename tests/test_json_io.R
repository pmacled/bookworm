library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R","json_io.R"))
  source(file.path("R", f))
mk_lineup <- function(prefix) lapply(1:4, function(i)
  make_player(paste0(prefix,i), paste(prefix,i), c("M","F","M","F")[i], i, i, i))
evts <- list(
  new_event("game_start", list(ruleset = default_ruleset_config(), first_bat="away",
    home=list(team_id="H",name="Home",lineup=mk_lineup("h")),
    away=list(team_id="A",name="Away",lineup=mk_lineup("a"))), seq=1L),
  new_event("plate_appearance", list(team="away",batter_id="a1",outcome="1B",
    reached=1L,rbi=0L,outs_on_play=0L,advances=list()), seq=2L))

test_that("round trip preserves game outcome", {
  txt <- game_to_json(evts)
  back <- game_from_json(txt)
  s1 <- fold_events(evts); s2 <- fold_events(back$events)
  expect_equal(s2$bases$first, s1$bases$first)
  expect_equal(s2$score, s1$score)
  expect_equal(back$events[[2]]$seq, 2L)  # seq restored as integer
})

test_that("round trip survives an out event (reached NA becomes null then back)", {
  evts_with_out <- c(evts, list(
    new_event("plate_appearance", list(team = "away", batter_id = "a2", outcome = "K",
      reached = NA_integer_, rbi = 0L, outs_on_play = 1L, advances = list()), seq = 3L)))
  back <- game_from_json(game_to_json(evts_with_out))
  s <- fold_events(back$events)     # must NOT error
  expect_equal(s$outs, 1L)
  expect_equal(s$bases$first, "a1")
})
