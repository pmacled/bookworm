library(testthat)
library(shiny)
library(DBI)
suppressMessages(source("global.R"))

# A fake connection that dispatches dbGetQuery by matching the SQL text, so the
# module's data-driven query order doesn't matter. `handlers` is a named list of
# c(pattern -> data.frame). dbExecute is a no-op returning 1.
setClass("MgFakeConn", contains = "DBIConnection",
         representation(handlers = "list"))
mgfake <- function(handlers) new("MgFakeConn", handlers = handlers)
setMethod("dbGetQuery", signature("MgFakeConn", "character"),
  function(conn, statement, ...) {
    for (pat in names(conn@handlers)) {
      if (grepl(pat, statement, ignore.case = TRUE)) {
        return(conn@handlers[[pat]])
      }
    }
    data.frame()
  })
setMethod("dbExecute", signature("MgFakeConn", "character"),
  function(conn, statement, ...) 1L)
setMethod("dbDisconnect", "MgFakeConn", function(conn, ...) invisible(TRUE))

leagues_df <- data.frame(
  id = "lg-1", name = "Summer", sport = "softball", is_owner = TRUE,
  stringsAsFactors = FALSE
)
teams_df <- data.frame(
  id = "tm-1", name = "Hawks", captain_user_id = NA_character_,
  captain_username = NA_character_, stringsAsFactors = FALSE
)
players_df <- data.frame(
  id = "pl-1", name = "Sam", gender = "M", jersey_number = 7L,
  default_position = 4L, stringsAsFactors = FALSE
)

con <- mgfake(list(
  "from leagues l" = leagues_df,
  "from teams t" = teams_df,
  "from players" = players_df,
  # ownership/manage checks: return a row so the user manages the team
  "l.owner_id = \\$2 or t.captain_user_id" = data.frame(x = 1)
))

test_that("manage starts at the leagues level and lists leagues", {
  testServer(
    manage_server,
    args = list(
      con_r = reactive(con),
      identity_r = reactive(list(mode = "user", user_id = "u1"))
    ),
    {
      session$flushReact()
      expect_equal(level(), "leagues")
      expect_equal(nrow(leagues()), 1L)
    }
  )
})

test_that("opening a league drills into teams; opening a team drills into players", {
  testServer(
    manage_server,
    args = list(
      con_r = reactive(con),
      identity_r = reactive(list(mode = "user", user_id = "u1"))
    ),
    {
      session$flushReact()
      session$setInputs(`open_league_lg-1` = 1)
      expect_equal(level(), "teams")
      expect_equal(sel_league()$id, "lg-1")
      expect_equal(nrow(teams()), 1L)

      session$setInputs(`open_team_tm-1` = 1)
      expect_equal(level(), "players")
      expect_equal(sel_team()$id, "tm-1")
      expect_true(isTRUE(sel_team()$can_manage))
      expect_equal(nrow(players()), 1L)
    }
  )
})

test_that("breadcrumb links navigate back up the hierarchy", {
  testServer(
    manage_server,
    args = list(
      con_r = reactive(con),
      identity_r = reactive(list(mode = "user", user_id = "u1"))
    ),
    {
      session$flushReact()
      session$setInputs(`open_league_lg-1` = 1)
      session$setInputs(`open_team_tm-1` = 1)
      expect_equal(level(), "players")

      session$setInputs(crumb_teams = 1)
      expect_equal(level(), "teams")
      expect_null(sel_team())

      session$setInputs(crumb_leagues = 1)
      expect_equal(level(), "leagues")
      expect_null(sel_league())
    }
  )
})
