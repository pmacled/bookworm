library(testthat)
library(shiny)
source(file.path("R", "app_config.R"))
source(file.path("R", "supabase_client.R"))
source(file.path("R", "auth_module.R"))

.SUPABASE_VARS <- c(
  "SUPABASE_DB_HOST",
  "SUPABASE_DB_PORT",
  "SUPABASE_DB_NAME",
  "SUPABASE_DB_USER",
  "SUPABASE_DB_PASSWORD"
)
# Runs `expr` with all Supabase env vars forced to either present or absent, then
# restores whatever was there before — so these tests don't leak state to others.
.with_supabase_configured <- function(configured, expr) {
  old <- Sys.getenv(.SUPABASE_VARS, unset = NA)
  on.exit(
    {
      for (n in names(old)) {
        if (is.na(old[[n]])) {
          Sys.unsetenv(n)
        } else {
          do.call(Sys.setenv, setNames(list(old[[n]]), n))
        }
      }
    },
    add = TRUE
  )
  if (configured) {
    do.call(
      Sys.setenv,
      setNames(as.list(rep("x", length(.SUPABASE_VARS))), .SUPABASE_VARS)
    )
  } else {
    for (n in .SUPABASE_VARS) {
      Sys.unsetenv(n)
    }
  }
  force(expr)
}

test_that("successful sign-in yields a user identity", {
  fake_ok <- function(username, password) {
    list(ok = TRUE, user_id = "u1", is_admin = FALSE, error = NA)
  }
  testServer(auth_server, args = list(sign_in = fake_ok), {
    session$setInputs(username = "mike", password = "pw", do_sign_in = 1)
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
  boom <- function(username, password) stop("connection refused")
  testServer(auth_server, args = list(sign_in = boom, sign_up = boom), {
    session$setInputs(username = "mike", password = "x", do_sign_in = 1)
    expect_true(nzchar(output$err))
    expect_true(is.na(identity()$mode)) # still unauthenticated
  })
})

test_that("guest mode is unaffected by a broken sign-in backend", {
  boom <- function(username, password) stop("connection refused")
  testServer(auth_server, args = list(sign_in = boom, sign_up = boom), {
    session$setInputs(do_guest = 1)
    expect_equal(identity()$mode, "guest")
  })
})

# Extracts one button's opening tag, then strips quoted attribute values (so the
# literal word "disabled" inside class="... disabled" can't masquerade as the real
# HTML boolean attribute, which htmltools renders bare — `disabled` with no `=`).
.button_tag_attrs_only <- function(html, id) {
  tag <- sub(paste0('.*(<button[^>]*id="', id, '"[^>]*>).*'), "\\1", html)
  gsub('="[^"]*"', "", tag)
}

test_that("sign in/create account are keyboard-unsubmittable (real disabled attr) when unconfigured", {
  .with_supabase_configured(FALSE, {
    html <- as.character(auth_ui("auth"))
    expect_match(
      .button_tag_attrs_only(html, "auth-do_sign_in"),
      "\\bdisabled\\b"
    )
    expect_match(
      .button_tag_attrs_only(html, "auth-do_sign_up"),
      "\\bdisabled\\b"
    )
  })
})

test_that("sign in/create account have no disabled attr when configured", {
  .with_supabase_configured(TRUE, {
    html <- as.character(auth_ui("auth"))
    expect_no_match(
      .button_tag_attrs_only(html, "auth-do_sign_in"),
      "\\bdisabled\\b"
    )
    expect_no_match(
      .button_tag_attrs_only(html, "auth-do_sign_up"),
      "\\bdisabled\\b"
    )
  })
})
