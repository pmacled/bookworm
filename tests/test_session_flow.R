library(testthat)
for (f in c("app_config.R","game_events.R","rules_engine.R","game_reducer.R","json_io.R","storage.R","supabase_client.R","session_flow.R"))
  source(file.path("R", f))

test_that("guest identity yields a guest storage and no connection", {
  sf <- storage_for_identity(list(mode = "guest", user_id = NA_character_, access_token = NA_character_))
  expect_null(sf$con)
  expect_true(is.function(sf$storage$create_game))
  expect_true(is.function(sf$storage$append_event))
})

test_that("user identity without supabase config falls back to guest (no DB needed)", {
  # supabase_configured() is FALSE with no env vars, so storage_for_identity must not try to connect.
  sf <- storage_for_identity(list(mode = "user", user_id = "u1", access_token = "t1"))
  expect_null(sf$con)
  expect_true(is.function(sf$storage$create_game))
})
