library(testthat)
# brand_colors.R must be sourced (scorebook_render.R uses BRAND_COLORS); it reads _brand.yml
# relative to the project root, so run this test from the project root.
for (f in c("app_config.R","brand_colors.R","rules_engine.R","rule_presets.R",
            "game_events.R","game_reducer.R","runner_paths.R","scorebook_render.R"))
  source(file.path("R", f))

mk_path <- function(bases_reached, scored = FALSE, out_at = NA_integer_,
                    out_number = NA_integer_, rbi = 0L, outcome = "1B",
                    fielding = NA_character_)
  list(pa_index = 1L, team = "away", inning = 1L, half = "top", pa_index_in_half = 1L,
       batter_id = "a1", outcome = outcome, fielding = fielding, rbi = rbi,
       bases_reached = bases_reached, scored = scored,
       out_at = out_at, out_number = out_number)

n_heavy <- function(frag) lengths(regmatches(frag, gregexpr('stroke-width="3"', frag)))

test_that("the cell draws one heavy leg per base reached", {
  expect_equal(n_heavy(scorebook_cell_svg(mk_path(0L), 0, 0, 64)), 0L)
  expect_equal(n_heavy(scorebook_cell_svg(mk_path(1L), 0, 0, 64)), 1L)
  expect_equal(n_heavy(scorebook_cell_svg(mk_path(2L), 0, 0, 64)), 2L)
  expect_equal(n_heavy(scorebook_cell_svg(mk_path(3L), 0, 0, 64)), 3L)
})

test_that("a scored trip draws all four legs and fills the diamond", {
  frag <- scorebook_cell_svg(mk_path(4L, scored = TRUE), 0, 0, 64)
  expect_equal(n_heavy(frag), 4L)
  expect_true(grepl("polygon", frag))
  expect_true(grepl(BRAND_COLORS$primary_light, frag, fixed = TRUE))
})

test_that("an unscored cell is not filled", {
  frag <- scorebook_cell_svg(mk_path(3L), 0, 0, 64)
  expect_false(grepl(BRAND_COLORS$primary_light, frag, fixed = TRUE))
})

test_that("the outcome and fielding notation are rendered", {
  frag <- scorebook_cell_svg(mk_path(0L, outcome = "GO", fielding = "6-3"), 0, 0, 64)
  expect_true(grepl("GO", frag, fixed = TRUE))
  expect_true(grepl("6-3", frag, fixed = TRUE))
})

test_that("the out number renders as a circled numeral", {
  frag <- scorebook_cell_svg(mk_path(0L, out_number = 2L, outcome = "K"), 0, 0, 64)
  expect_true(grepl("circle", frag))
  expect_true(grepl(">2<", frag, fixed = TRUE))
})

test_that("RBI dots are drawn, one per RBI, capped at four", {
  n_dots <- function(f) lengths(regmatches(f, gregexpr("bw-rbi", f)))
  expect_equal(n_dots(scorebook_cell_svg(mk_path(4L, scored = TRUE, rbi = 2L), 0, 0, 64)), 2L)
  expect_equal(n_dots(scorebook_cell_svg(mk_path(4L, scored = TRUE, rbi = 9L), 0, 0, 64)), 4L)
  expect_equal(n_dots(scorebook_cell_svg(mk_path(1L, rbi = 0L), 0, 0, 64)), 0L)
})

test_that("an out on the basepaths marks the base", {
  frag <- scorebook_cell_svg(mk_path(2L, out_at = 2L, out_number = 1L), 0, 0, 64)
  expect_true(grepl("bw-out-mark", frag, fixed = TRUE))
})

test_that("a batter out at the plate has no base mark", {
  frag <- scorebook_cell_svg(mk_path(0L, out_at = 0L, out_number = 1L, outcome = "K"),
                             0, 0, 64)
  expect_false(grepl("bw-out-mark", frag, fixed = TRUE))
})

test_that("the cell is positioned at the requested offset", {
  frag <- scorebook_cell_svg(mk_path(1L), x = 120, y = 40, cell = 64)
  nums <- as.numeric(regmatches(frag, gregexpr("[0-9]+\\.?[0-9]*", frag))[[1]])
  expect_true(all(nums[nums > 100 & nums < 400] >= 40))
})

# ---- Task 3: grid ----

mk_lineup <- function(prefix, n = 3L) lapply(seq_len(n), function(i)
  make_player(paste0(prefix, i), paste("A", i), "M", i, i, i))

state_with <- function(events) {
  start <- new_event("game_start", list(ruleset = default_ruleset_config(),
    first_bat = "away",
    home = list(team_id = "H", name = "Home", lineup = mk_lineup("h")),
    away = list(team_id = "A", name = "Away", lineup = mk_lineup("a"))), seq = 1L)
  fold_events(c(list(start), events))
}
PA <- function(batter, outcome, advances, outs = 0L, reached = NA_integer_, seq = 2L)
  new_event("plate_appearance", list(team = "away", batter_id = batter, outcome = outcome,
    reached = reached, rbi = 0L, outs_on_play = outs, advances = advances), seq = seq)
Adv <- function(id, from, to, ...) make_advance(id, from, to, ...)

test_that("the grid renders an svg with a row label per batter", {
  html <- as.character(render_scorebook_svg(state_with(list()), "away"))
  expect_true(grepl("<svg", html))
  for (i in 1:3) expect_true(grepl(paste("A", i), html, fixed = TRUE))
})

test_that("a jersey-less player has no stray leading hash or space", {
  st <- state_with(list())
  st$lineups$away[[1]]$jersey_number <- NA_integer_
  html <- as.character(render_scorebook_svg(st, "away"))
  expect_false(grepl(">#NA", html, fixed = TRUE))
})

test_that("the current batter's row label is bold", {
  st <- state_with(list())
  html <- as.character(render_scorebook_svg(st, "away", current_batter_id = "a2"))
  expect_true(grepl("font-weight=\"700\"", html, fixed = TRUE) ||
              grepl("bw-current", html, fixed = TRUE))
})

test_that("no row is bold when no current batter is passed", {
  html <- as.character(render_scorebook_svg(state_with(list()), "away"))
  expect_false(grepl("bw-current", html, fixed = TRUE))
})

test_that("batting around widens the inning into sub-columns", {
  st <- state_with(list(
    PA("a1", "1B", list(Adv("a1", 0L, 1L)), reached = 1L, seq = 2L),
    PA("a2", "1B", list(Adv("a1", 1L, 2L), Adv("a2", 0L, 1L)), reached = 1L, seq = 3L),
    PA("a3", "1B", list(Adv("a1", 2L, 3L), Adv("a2", 1L, 2L), Adv("a3", 0L, 1L)),
       reached = 1L, seq = 4L),
    PA("a1", "K", list(Adv("a1", 0L, 0L, out = TRUE)), outs = 1L, seq = 5L)))
  html <- as.character(render_scorebook_svg(st, "away"))
  narrow <- as.character(render_scorebook_svg(state_with(list(
    PA("a1", "K", list(Adv("a1", 0L, 0L, out = TRUE)), outs = 1L, seq = 2L))), "away"))
  w <- function(h) as.numeric(sub('.*<svg width="([0-9]+)".*', "\\1", h))
  expect_gt(w(html), w(narrow))
})

test_that("a team with no plate appearances still renders its lineup", {
  html <- as.character(render_scorebook_svg(state_with(list()), "home"))
  expect_true(grepl("<svg", html))
})

test_that("the grid shows every inning that has been played", {
  st <- state_with(list(
    PA("a1", "K", list(Adv("a1", 0L, 0L, out = TRUE)), outs = 1L, seq = 2L),
    PA("a2", "K", list(Adv("a2", 0L, 0L, out = TRUE)), outs = 1L, seq = 3L),
    PA("a3", "K", list(Adv("a3", 0L, 0L, out = TRUE)), outs = 1L, seq = 4L)))
  expect_equal(st$half, "bottom")
  html <- as.character(render_scorebook_svg(st, "away"))
  expect_true(grepl(">1<", html, fixed = TRUE))
})
