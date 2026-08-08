auth_ui <- function(id) {
  ns <- NS(id)
  configured <- supabase_configured()
  tagList(
    tags$h3(if (configured) "Sign in to save your games" else "Bookworm"),
    if (!configured)
      div(class = "alert alert-warning py-2 small",
        "Saving is not configured on this deployment. You can score a game as a guest, ",
        "but it will be lost when you refresh."),
    textInput(ns("email"), "Email"),
    passwordInput(ns("password"), "Password"),
    div(class = "d-flex gap-2 flex-wrap",
      actionButton(ns("do_sign_in"), "Sign in",
        class = if (configured) "btn-primary" else "btn-outline-secondary disabled",
        disabled = !configured),
      actionButton(ns("do_sign_up"), "Create account",
        class = if (configured) "btn-outline-secondary" else "btn-outline-secondary disabled",
        disabled = !configured),
      actionButton(ns("do_guest"), "Continue as guest",
        class = if (configured) "btn-link" else "btn-primary")),
    div(class = "text-danger small mt-2", textOutput(ns("err")))
  )
}

auth_server <- function(id, sign_in = gotrue_sign_in, sign_up = gotrue_sign_up) {
  moduleServer(id, function(input, output, session) {
    identity <- reactiveVal(list(mode = NA_character_, user_id = NA_character_, access_token = NA_character_))
    err <- reactiveVal("")

    handle <- function(fn) {
      res <- tryCatch(fn(input$email, input$password),
                      error = function(e) list(ok = FALSE,
                        error = "Could not reach the sign-in service."))
      if (isTRUE(res$ok)) {
        identity(list(mode = "user", user_id = res$user_id, access_token = res$access_token))
        err("")
      } else err(res$error %||% "Authentication failed")
    }
    observeEvent(input$do_sign_in, handle(sign_in))
    observeEvent(input$do_sign_up, handle(sign_up))
    observeEvent(input$do_guest,
      identity(list(mode = "guest", user_id = NA_character_, access_token = NA_character_)))

    output$err <- renderText(err())
    identity
  })
}
