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
