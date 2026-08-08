library(testthat)
for (f in c("app_config.R","rules_engine.R","game_events.R","game_reducer.R","boxscore.R"))
  source(file.path("R", f))
mk_lineup <- function(prefix, genders = c("M","F","M","F"))
  lapply(seq_along(genders), function(i)
    make_player(paste0(prefix,i), paste(prefix,i), genders[i], i, i, i))
start_evt <- function() new_event("game_start", list(
  ruleset = default_ruleset_config(), first_bat = "away",
  home = list(team_id="H", name="Home", lineup = mk_lineup("h")),
  away = list(team_id="A", name="Away", lineup = mk_lineup("a"))), seq = 1L)
pa <- function(b,o,r,seq,rbi=0L,outs=0L,adv=list()) new_event("plate_appearance",
  list(team="away",batter_id=b,outcome=o,reached=r,rbi=rbi,outs_on_play=outs,advances=adv), seq=seq)

test_that("batting lines count H, AB, K, BB", {
  s <- fold_events(list(start_evt(),
    pa("a1","1B",1L,2L),
    pa("a2","K",NA_integer_,3L,outs=1L),
    pa("a3","BB",1L,4L)))
  bl <- batting_lines(s, "away")
  a1 <- bl[bl$Player=="a 1",]; a2 <- bl[bl$Player=="a 2",]; a3 <- bl[bl$Player=="a 3",]
  expect_equal(a1$H, 1L); expect_equal(a1$AB, 1L)
  expect_equal(a2$K, 1L); expect_equal(a2$AB, 1L)
  expect_equal(a3$BB, 1L); expect_equal(a3$AB, 0L)  # walks are not at-bats
})

test_that("line score exposes R/H/E totals", {
  s <- fold_events(list(start_evt(), pa("a1","1B",1L,2L)))
  ls <- line_score(s)
  expect_true(is.list(ls$away))
  expect_true(!is.null(ls$away$H))
})

test_that("batting_lines exposes no player_id and is ordered by batting slot", {
  lu <- list(make_player("a1", "Ann", "F", 7L, 2L, "SS"),
             make_player("a2", "Bo",  "M", 3L, 1L, "P"))
  st <- initial_game_state()
  st$lineups$away <- lu
  df <- batting_lines(st, "away")
  expect_false("player_id" %in% names(df))
  expect_identical(names(df), c("Order", "Player", "AB", "R", "H", "RBI", "BB", "K"))
  expect_equal(df$Order, c(1L, 2L))
  expect_equal(df$Player, c("Bo", "Ann"))
})

test_that("a player with no order_slot still appears, after the batters", {
  lu <- list(make_player("a1", "Ann", "F", 7L, 1L, "SS"),
             make_player("a2", "Sub", "M", 3L, NA_integer_, NA_character_))
  st <- initial_game_state()
  st$lineups$away <- lu
  df <- batting_lines(st, "away")
  expect_equal(nrow(df), 2L)
  expect_equal(df$Player[2], "Sub")
  expect_true(is.na(df$Order[2]))
})
