library(testthat)
# brand_colors.R must be sourced (scorebook_render.R uses BRAND_COLORS); it reads _brand.yml
# relative to the project root, so run this test from the project root.
for (f in c("app_config.R","brand_colors.R","rules_engine.R","game_events.R","game_reducer.R","scorebook_render.R"))
  source(file.path("R", f))

test_that("cell svg contains a diamond polygon and the outcome text", {
  rec <- list(inning=1L, half="top", team="away", batter_id="a1", outcome="1B",
              fielding=NA_character_, rbi=0L, outs_on_play=0L, reached=1L,
              bases_after=list(first="a1",second=NA,third=NA))
  frag <- scorebook_cell_svg(rec, x = 0, y = 0, cell = 60)
  expect_true(grepl("polygon", frag))
  expect_true(grepl("1B", frag))
})

test_that("render_scorebook_svg returns an svg with a row per batter", {
  mk <- lapply(1:3, function(i) make_player(paste0("a",i), paste("A",i), "M", i, i, i))
  st <- initial_game_state(); st$lineups$away <- mk
  st$teams <- list(away = list(team_id="A", name="Away"))
  st$pa_log <- list(list(inning=1L,half="top",team="away",batter_id="a1",outcome="1B",
    fielding=NA_character_,rbi=0L,outs_on_play=0L,reached=1L,
    bases_after=list(first="a1",second=NA,third=NA)))
  html <- as.character(render_scorebook_svg(st, "away"))
  expect_true(grepl("<svg", html))
  expect_true(grepl("A 1", html) || grepl("A1", html))
})
