library(testthat)
for (f in c(
  "app_config.R",
  "game_events.R",
  "rules_engine.R",
  "game_reducer.R",
  "json_io.R",
  "storage.R",
  "supabase_client.R",
  "session_flow.R"
)) {
  source(file.path("R", f))
}

test_that("guest identity yields a guest storage and no connection", {
  sf <- storage_for_identity(list(
    mode = "guest",
    user_id = NA_character_,
    access_token = NA_character_
  ))
  expect_null(sf$con)
  expect_true(is.function(sf$storage$create_game))
  expect_true(is.function(sf$storage$append_event))
})

test_that("user identity without supabase config falls back to guest (no DB needed)", {
  # supabase_configured() is FALSE with no env vars, so storage_for_identity must not try to connect.
  sf <- storage_for_identity(list(
    mode = "user",
    user_id = "u1",
    access_token = "t1"
  ))
  expect_null(sf$con)
  expect_true(is.function(sf$storage$create_game))
})

test_that("guest identity yields guest storage and is not degraded", {
  sf <- storage_for_identity(list(mode = "guest", user_id = NA_character_))
  expect_true(is.function(sf$storage$create_game))
  expect_null(sf$con)
  expect_false(sf$degraded)
})

test_that("a failing database connection falls back to guest storage", {
  # supabase_connect is looked up in the calling environment, so a local shadow works.
  sf <- storage_for_identity(
    list(mode = "user", user_id = "u1"),
    configured = function() TRUE,
    connect = function() stop("could not connect to server")
  )
  expect_true(is.function(sf$storage$append_event))
  expect_null(sf$con)
  expect_true(sf$degraded)
  expect_true(nzchar(sf$reason))
})

test_that("a working database connection is used and is not degraded", {
  fake_con <- structure(list(), class = "FakeConn")
  sf <- storage_for_identity(
    list(mode = "user", user_id = "u1"),
    configured = function() TRUE,
    connect = function() fake_con
  )
  expect_identical(sf$con, fake_con)
  expect_false(sf$degraded)
})

test_that("a throwing configured() falls back to guest storage instead of propagating", {
  sf <- storage_for_identity(
    list(mode = "user", user_id = "u1"),
    configured = function() stop("boom")
  )
  expect_true(is.function(sf$storage$append_event))
  expect_null(sf$con)
  expect_true(sf$degraded)
  expect_true(nzchar(sf$reason))
})

test_that("a missing RPostgres driver reports a driver problem, not a network problem, and never calls connect()", {
  connect_called <- FALSE
  sf <- storage_for_identity(
    list(mode = "user", user_id = "u1"),
    configured = function() TRUE,
    driver_available = function() FALSE,
    connect = function() {
      connect_called <<- TRUE
      stop("should never be reached")
    }
  )
  expect_false(connect_called)
  expect_null(sf$con)
  expect_true(sf$degraded)
  expect_match(sf$reason, "driver")
  expect_no_match(sf$reason, "reach the database")
})

test_that("a reachability failure (driver present, connect() throws) still reports the network message", {
  sf <- storage_for_identity(
    list(mode = "user", user_id = "u1"),
    configured = function() TRUE,
    driver_available = function() TRUE,
    connect = function() stop("could not connect to server")
  )
  expect_null(sf$con)
  expect_true(sf$degraded)
  expect_match(sf$reason, "reach the database")
  expect_no_match(sf$reason, "driver")
})
