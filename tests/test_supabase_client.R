library(testthat)
library(DBI)
source(file.path("R", "app_config.R"))
source(file.path("R", "supabase_client.R"))

# A fake DBI connection whose query results are supplied per-test. `queries`
# is a list of data.frames returned in order by dbGetQuery(). We register S4
# methods so DBI's generics dispatch to it without a real database.
setClass(
  "FakeConn",
  contains = "DBIConnection",
  representation(queries = "list", env = "environment")
)
fake_conn <- function(queries) {
  new("FakeConn", queries = queries, env = new.env())
}
setMethod(
  "dbGetQuery",
  signature("FakeConn", "character"),
  function(conn, statement, ...) {
    conn@env$i <- (conn@env$i %||% 0) + 1
    conn@queries[[conn@env$i]]
  }
)
setMethod(
  "dbExecute",
  signature("FakeConn", "character"),
  function(conn, statement, ...) 1L
)
setMethod("dbDisconnect", "FakeConn", function(conn, ...) invisible(TRUE))

test_that("db_sign_in returns identity on a matching credential row", {
  con <- fake_conn(list(data.frame(id = "u-123", is_admin = FALSE)))
  res <- db_sign_in("mike", "hunter2", connect = function() con)
  expect_true(res$ok)
  expect_equal(res$user_id, "u-123")
  expect_false(res$is_admin)
})

test_that("db_sign_in rejects a non-matching credential (no row)", {
  con <- fake_conn(list(data.frame(id = character(), is_admin = logical())))
  res <- db_sign_in("mike", "wrong", connect = function() con)
  expect_false(res$ok)
  expect_match(res$error, "not correct")
  expect_true(is.na(res$user_id))
})

test_that("db_sign_in rejects malformed usernames before touching the database", {
  connected <- FALSE
  res <- db_sign_in("no", "hunter2", connect = function() {
    connected <<- TRUE
    stop("should not connect")
  })
  expect_false(res$ok)
  expect_false(connected)
})

test_that("db_sign_up rejects a taken username", {
  con <- fake_conn(list(data.frame(x = 1))) # existence check returns a row
  res <- db_sign_up("mike", "hunter2", connect = function() con)
  expect_false(res$ok)
  expect_match(res$error, "already taken")
})

test_that("db_sign_up rejects short passwords", {
  res <- db_sign_up("mike", "short", connect = function() stop("no"))
  expect_false(res$ok)
  expect_match(res$error, "6 characters")
})

test_that("db_sign_up creates a user and returns identity", {
  con <- fake_conn(list(
    data.frame(x = integer()), # existence check: none
    data.frame(id = "u-9", is_admin = FALSE) # insert ... returning
  ))
  res <- db_sign_up("mike", "hunter2", connect = function() con)
  expect_true(res$ok)
  expect_equal(res$user_id, "u-9")
})

test_that("a connection failure returns an error result instead of throwing", {
  res <- db_sign_in("mike", "hunter2", connect = function() {
    stop("simulated connection failure")
  })
  expect_false(res$ok)
  expect_equal(res$error, "Could not reach the sign-in service.")
  expect_true(is.na(res$user_id))
})

test_that("friendly_auth_error rewrites known messages and passes others through", {
  expect_match(
    friendly_auth_error("invalid_credentials"),
    "username or password"
  )
  expect_match(friendly_auth_error("username_taken"), "already taken")
  # Unknown messages pass through unchanged.
  expect_equal(friendly_auth_error("teapot"), "teapot")
  # Empty or missing collapses to a generic message.
  expect_true(nzchar(friendly_auth_error("")))
  expect_true(nzchar(friendly_auth_error(NA_character_)))
})
