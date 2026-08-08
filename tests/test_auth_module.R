library(testthat); library(shiny)
source(file.path("R","app_config.R")); source(file.path("R","auth_module.R"))

test_that("successful sign-in yields a user identity", {
  fake_ok <- function(email, password) list(ok=TRUE, user_id="u1", access_token="t1", error=NA)
  testServer(auth_server, args = list(sign_in = fake_ok), {
    session$setInputs(email = "a@b.com", password = "pw", do_sign_in = 1)
    expect_equal(identity()$mode, "user")
    expect_equal(identity()$user_id, "u1")
  })
})

test_that("continue as guest yields guest identity", {
  testServer(auth_server, {
    session$setInputs(do_guest = 1)
    expect_equal(identity()$mode, "guest")
  })
})

test_that("a sign-in function that throws surfaces a message instead of crashing", {
  boom <- function(email, password) stop("connection refused")
  testServer(auth_server, args = list(sign_in = boom, sign_up = boom), {
    session$setInputs(email = "a@b.c", password = "x", do_sign_in = 1)
    expect_true(nzchar(output$err))
    expect_true(is.na(identity()$mode))   # still unauthenticated
  })
})

test_that("guest mode is unaffected by a broken sign-in backend", {
  boom <- function(email, password) stop("connection refused")
  testServer(auth_server, args = list(sign_in = boom, sign_up = boom), {
    session$setInputs(do_guest = 1)
    expect_equal(identity()$mode, "guest")
  })
})
