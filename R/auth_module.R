auth_ui <- function(id) {
  ns <- NS(id)
  configured <- supabase_configured()
  tagList(
    tags$h3(if (configured) "Sign in to save your games" else "Bookworm"),
    if (!configured) {
      div(
        class = "alert alert-warning py-2 small",
        "Saving is not configured on this deployment. You can score a game as a guest, ",
        "but it will be lost when you refresh."
      )
    },
    textInput(ns("username"), "Username"),
    passwordInput(ns("password"), "Password"),
    if (configured) {
      checkboxInput(ns("remember"), "Keep me signed in", value = TRUE)
    },
    div(
      class = "d-flex gap-2 flex-wrap",
      if (configured) {
        # Task buttons show a spinner and disable themselves while the sign-in /
        # sign-up DB query runs, then auto-reset when the flush completes.
        input_task_button(
          ns("do_sign_in"),
          "Sign in",
          label_busy = "Signing in\u2026",
          class = "btn-primary"
        )
      } else {
        actionButton(
          ns("do_sign_in"),
          "Sign in",
          class = "btn-outline-secondary disabled",
          disabled = TRUE
        )
      },
      if (configured) {
        input_task_button(
          ns("do_sign_up"),
          "Create account",
          label_busy = "Creating\u2026",
          class = "btn-outline-secondary"
        )
      } else {
        actionButton(
          ns("do_sign_up"),
          "Create account",
          class = "btn-outline-secondary disabled",
          disabled = TRUE
        )
      },
      actionButton(
        ns("do_guest"),
        "Continue as guest",
        class = if (configured) "btn-link" else "btn-primary"
      )
    ),
    div(class = "text-danger small mt-2", textOutput(ns("err")))
  )
}

auth_server <- function(
  id,
  sign_in = db_sign_in,
  sign_up = db_sign_up,
  issue_token = db_issue_remember_token,
  validate_token = db_validate_remember_token,
  revoke_token = db_revoke_remember_token,
  purge_tokens = db_purge_expired_tokens
) {
  moduleServer(id, function(input, output, session) {
    identity <- reactiveVal(list(
      mode = NA_character_,
      user_id = NA_character_,
      is_admin = FALSE,
      remember_token = NA_character_
    ))
    err <- reactiveVal("")

    set_user <- function(res, username, remember_token = NA_character_) {
      identity(list(
        mode = "user",
        user_id = res$user_id,
        is_admin = isTRUE(res$is_admin),
        username = username,
        remember_token = remember_token %||% NA_character_
      ))
      err("")
    }

    # Issue a remember-me token and hand {user_id, username, token} to the
    # browser for auto-login next visit. Best-effort: a failure here must not
    # block the sign-in that already succeeded.
    remember <- function(res, username) {
      if (!isTRUE(input$remember)) {
        return(NA_character_)
      }
      token <- tryCatch(issue_token(res$user_id), error = function(e) NULL)
      if (is.null(token) || is.na(token)) {
        return(NA_character_)
      }
      session$sendCustomMessage(
        "bw_storeUser",
        jsonlite::toJSON(
          list(user_id = res$user_id, username = username, token = token),
          auto_unbox = TRUE
        )
      )
      token
    }

    handle <- function(fn) {
      username <- input$username
      res <- tryCatch(fn(username, input$password), error = function(e) {
        list(ok = FALSE, error = "Could not reach the sign-in service.")
      })
      if (isTRUE(res$ok)) {
        token <- remember(res, username)
        set_user(res, username, token)
      } else {
        err(res$error %||% "Authentication failed")
      }
    }
    observeEvent(input$do_sign_in, handle(sign_in))
    observeEvent(input$do_sign_up, handle(sign_up))
    observeEvent(
      input$do_guest,
      identity(list(
        mode = "guest",
        user_id = NA_character_,
        is_admin = FALSE,
        remember_token = NA_character_
      ))
    )

    # Ask the browser for any stored remember-me token — but only once the
    # client JS has registered its handlers (input$jsReady), so the message is
    # never sent to an unregistered handler.
    observeEvent(input$jsReady, {
      if (supabase_configured()) {
        session$sendCustomMessage("bw_getStoredUser", "")
      }
    })

    # Auto-login: validate the stored token; on success sign the user in, on
    # failure clear the stale localStorage entry.
    observeEvent(input$storedUser, {
      raw <- input$storedUser
      if (is.null(raw) || !nzchar(raw)) {
        return(invisible())
      }
      tryCatch(purge_tokens(), error = function(e) NULL)
      stored <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
      ok <- !is.null(stored) &&
        !is.null(stored$user_id) &&
        !is.null(stored$username) &&
        !is.null(stored$token)
      if (!ok) {
        session$sendCustomMessage("bw_clearStoredUser", "")
        return(invisible())
      }
      res <- tryCatch(
        validate_token(stored$user_id, stored$username, stored$token),
        error = function(e) list(ok = FALSE)
      )
      if (isTRUE(res$ok)) {
        set_user(res, stored$username, stored$token)
      } else {
        session$sendCustomMessage("bw_clearStoredUser", "")
      }
    })

    # Sign out: revoke the token server-side and clear the browser store.
    observeEvent(input$sign_out, {
      cur <- identity()
      tok <- cur$remember_token %||% NA_character_
      if (!is.na(tok) && nzchar(tok)) {
        tryCatch(revoke_token(tok), error = function(e) NULL)
      }
      session$sendCustomMessage("bw_clearStoredUser", "")
      identity(list(
        mode = NA_character_,
        user_id = NA_character_,
        is_admin = FALSE,
        remember_token = NA_character_
      ))
      err("")
    })

    output$err <- renderText(err())
    identity
  })
}
