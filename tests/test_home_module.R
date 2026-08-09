library(testthat)
suppressMessages(source("global.R"))

.stub_storage <- function(df, on_delete = function(id, uid) TRUE) {
  list(
    list_games = function() df,
    delete_game = function(game_id, user_id = NULL) on_delete(game_id, user_id)
  )
}

.games_df <- function() {
  data.frame(
    game_id = c("g1", "g2", "g3"),
    name = c("Game A", "Game B", "Game C"),
    status = c("in_progress", "final", "in_progress"),
    updated_at = c("2026-08-01", "2026-08-02", "2026-08-03"),
    relationship = c("owned", "league", "shared"),
    home_team = c("Hawks", "Owls", NA_character_),
    away_team = c("Jays", "Robins", NA_character_),
    can_delete = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

test_that("home_server surfaces the games list from storage", {
  testServer(
    home_server,
    args = list(
      storage_r = reactive(.stub_storage(.games_df())),
      identity_r = reactive(list(mode = "user", user_id = "u1"))
    ),
    {
      session$flushReact()
      expect_equal(nrow(games()), 3L)
      expect_setequal(games()$game_id, c("g1", "g2", "g3"))
      expect_true("home_team" %in% names(games()))
      expect_true("can_delete" %in% names(games()))
    }
  )
})

test_that("relationship filter narrows the list", {
  testServer(
    home_server,
    args = list(
      storage_r = reactive(.stub_storage(.games_df())),
      identity_r = reactive(list(mode = "user", user_id = "u1"))
    ),
    {
      session$flushReact()
      session$setInputs(rel_filter = "league")
      expect_equal(nrow(filtered()), 1L)
      expect_equal(filtered()$game_id, "g2")
    }
  )
})

test_that("clicking a card's Open button sets open_game with id and status", {
  testServer(
    home_server,
    args = list(
      storage_r = reactive(.stub_storage(.games_df())),
      identity_r = reactive(list(mode = "user", user_id = "u1"))
    ),
    {
      session$flushReact()
      session$setInputs(open_g2 = 1) # the final game
      og <- session$returned$open_game()
      expect_equal(og$game_id, "g2")
      expect_equal(og$status, "final")
    }
  )
})

test_that("confirming delete calls storage$delete_game and refreshes", {
  deleted <- NULL
  storage <- .stub_storage(
    .games_df(),
    on_delete = function(id, uid) {
      deleted <<- list(id = id, uid = uid)
      TRUE
    }
  )
  testServer(
    home_server,
    args = list(
      storage_r = reactive(storage),
      identity_r = reactive(list(mode = "user", user_id = "u1"))
    ),
    {
      session$flushReact()
      session$setInputs(del_g1 = 1) # opens confirm modal
      session$setInputs(confirm_del_g1 = 1) # confirm
      session$flushReact()
      expect_equal(deleted$id, "g1")
      expect_equal(deleted$uid, "u1")
    }
  )
})

test_that("matchup title falls back to Away @ Home when team names are missing", {
  expect_equal(.matchup_title(NA_character_, NA_character_), "Away @ Home")
  expect_equal(.matchup_title("Jays", "Hawks"), "Jays @ Hawks")
})

test_that("updated timestamp renders in EDT to the second", {
  out <- .format_updated("2026-08-09 13:37:49.123456+00")
  expect_equal(out, "2026-08-09 09:37:49 EDT")
  expect_true(is.na(.format_updated(NA_character_)))
})
