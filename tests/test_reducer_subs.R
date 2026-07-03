library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R"))
  source(file.path("R", f))
mk_lineup <- function(prefix, genders = c("M","F","M","F"))
  lapply(seq_along(genders), function(i)
    make_player(paste0(prefix,i), paste(prefix,i), genders[i], i, i, i))
start_evt <- function() new_event("game_start", list(
  ruleset = default_ruleset_config(), first_bat = "away",
  home = list(team_id="H", name="Home", lineup = mk_lineup("h")),
  away = list(team_id="A", name="Away", lineup = mk_lineup("a"))), seq = 1L)

test_that("batting sub replaces the player in the order slot", {
  sub <- new_event("substitution", list(team="away", kind="batting",
    out_player_id="a1", order_slot=1L,
    in_player = make_player("x9","Pinch","F",99L,1L,NA_integer_)), seq = 2L)
  s <- fold_events(list(start_evt(), sub))
  slot1 <- Filter(function(p) p$order_slot==1L, s$lineups$away)[[1]]
  expect_equal(slot1$player_id, "x9")
})

test_that("courtesy runner swaps the runner on base", {
  pa1 <- new_event("plate_appearance", list(team="away", batter_id="a1",
    outcome="1B", reached=1L, rbi=0L, outs_on_play=0L, advances=list()), seq = 2L)
  cr <- new_event("substitution", list(team="away", kind="courtesy_runner",
    out_player_id="a1", in_player = make_player("cr1","Runner","M",50L,NA_integer_,NA_integer_)),
    seq = 3L)
  s <- fold_events(list(start_evt(), pa1, cr))
  expect_equal(s$bases$first, "cr1")
})
