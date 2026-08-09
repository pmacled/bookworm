library(testthat)
library(DBI)
source(file.path("R", "app_config.R"))
source(file.path("R", "supabase_client.R")) # for .valid_username, %||%
source(file.path("R", "manage_data.R"))

# A fake connection that returns scripted dbGetQuery results in order and counts
# dbExecute calls. `queries` is a list of data.frames; each dbGetQuery pops the
# next one. dbExecute returns `exec_rows` (default 1) so ownership-gated deletes
# report success unless a test overrides it.
setClass(
  "MFakeConn",
  contains = "DBIConnection",
  representation(queries = "list", env = "environment", exec_rows = "numeric")
)
mfake <- function(queries = list(), exec_rows = 1) {
  new("MFakeConn", queries = queries, env = new.env(), exec_rows = exec_rows)
}
setMethod("dbGetQuery", signature("MFakeConn", "character"), function(conn, statement, ...) {
  conn@env$i <- (conn@env$i %||% 0) + 1
  q <- conn@queries[[conn@env$i]]
  if (is.null(q)) data.frame() else q
})
setMethod("dbExecute", signature("MFakeConn", "character"), function(conn, statement, ...) {
  conn@env$exec <- (conn@env$exec %||% 0) + 1
  conn@exec_rows
})
setMethod("dbDisconnect", "MFakeConn", function(conn, ...) invisible(TRUE))

test_that("db_user_owns_league is TRUE when a row comes back", {
  con <- mfake(list(data.frame(x = 1)))
  expect_true(db_user_owns_league(con, "u1", "l1"))
})

test_that("db_user_owns_league is FALSE with no row", {
  con <- mfake(list(data.frame()))
  expect_false(db_user_owns_league(con, "u1", "l1"))
})

test_that("db_find_user_by_username rejects malformed usernames without a query", {
  con <- mfake(list())
  res <- db_find_user_by_username(con, "no")
  expect_false(res$ok)
  expect_match(res$error, "3-30")
})

test_that("db_find_user_by_username returns user_id on an exact match", {
  con <- mfake(list(data.frame(id = "u-9")))
  res <- db_find_user_by_username(con, "mike")
  expect_true(res$ok)
  expect_equal(res$user_id, "u-9")
})

test_that("db_find_user_by_username reports not-found for zero rows", {
  con <- mfake(list(data.frame(id = character())))
  res <- db_find_user_by_username(con, "ghost")
  expect_false(res$ok)
  expect_match(res$error, "No user")
})

test_that("db_create_league rejects a blank name before any query", {
  con <- mfake(list())
  res <- db_create_league(con, "u1", "   ")
  expect_false(res$ok)
  expect_match(res$error, "enter a name")
})

test_that("db_create_league returns the new id", {
  con <- mfake(list(data.frame(id = "lg-1")))
  res <- db_create_league(con, "u1", "Summer League")
  expect_true(res$ok)
  expect_equal(res$league_id, "lg-1")
})

test_that("db_create_team requires league ownership", {
  # First query is the ownership check -> no row -> not permitted.
  con <- mfake(list(data.frame()))
  res <- db_create_team(con, "u1", "l1", "Hawks")
  expect_false(res$ok)
  expect_match(res$error, "permission")
})

test_that("db_create_team with a captain resolves the username and inserts", {
  con <- mfake(list(
    data.frame(x = 1), # ownership check passes
    data.frame(id = "cap-1"), # username lookup
    data.frame(id = "team-1") # insert returning id
  ))
  res <- db_create_team(con, "u1", "l1", "Hawks", captain_username = "cindy")
  expect_true(res$ok)
  expect_equal(res$team_id, "team-1")
  # ownership + username lookup queries, plus the insert query = 3 dbGetQuery,
  # and the membership grant is a dbExecute.
  expect_true((con@env$exec %||% 0) >= 1)
})

test_that("db_create_team surfaces an unknown captain username", {
  con <- mfake(list(
    data.frame(x = 1), # ownership passes
    data.frame(id = character()) # username not found
  ))
  res <- db_create_team(con, "u1", "l1", "Hawks", captain_username = "ghost")
  expect_false(res$ok)
  expect_match(res$error, "No user")
})

test_that("db_user_manages_team is TRUE for owner or captain match", {
  con <- mfake(list(data.frame(x = 1)))
  expect_true(db_user_manages_team(con, "u1", "t1"))
})

test_that("db_add_player is blocked when the user does not manage the team", {
  con <- mfake(list(data.frame())) # manage check -> no row
  res <- db_add_player(con, "u1", "t1", "Sam")
  expect_false(res$ok)
  expect_match(res$error, "permission")
})

test_that("db_add_player inserts when permitted", {
  con <- mfake(list(
    data.frame(x = 1), # manage check
    data.frame(id = "p-1") # insert returning id
  ))
  res <- db_add_player(con, "u1", "t1", "Sam", gender = "m", jersey_number = 7)
  expect_true(res$ok)
  expect_equal(res$player_id, "p-1")
})

test_that("db_delete_league reports not-permitted when zero rows are deleted", {
  con <- mfake(list(), exec_rows = 0)
  res <- db_delete_league(con, "u1", "l1")
  expect_false(res$ok)
  expect_match(res$error, "permission")
})

test_that("db_delete_league succeeds when a row is deleted", {
  con <- mfake(list(), exec_rows = 1)
  res <- db_delete_league(con, "u1", "l1")
  expect_true(res$ok)
})

test_that(".clean_gender and .clean_int normalize inputs", {
  expect_equal(.clean_gender("m"), "M")
  expect_true(is.na(.clean_gender("x")))
  expect_equal(.clean_int(7, 0L, 999L), 7L)
  expect_true(is.na(.clean_int(1000, 0L, 999L)))
  expect_true(is.na(.clean_int(11, 1L, 10L)))
})
