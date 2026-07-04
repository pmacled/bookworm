auth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Sign in to save your games"),
    textInput(ns("email"), "Email"),
    passwordInput(ns("password"), "Password"),
    div(class = "d-flex gap-2 flex-wrap",
      actionButton(ns("do_sign_in"), "Sign in", class = "btn-primary"),
      actionButton(ns("do_sign_up"), "Create account", class = "btn-outline-secondary"),
      actionButton(ns("do_guest"), "Continue as guest", class = "btn-link")),
    div(class = "text-danger small mt-2", textOutput(ns("err")))
  )
}

auth_server <- function(id, sign_in = gotrue_sign_in, sign_up = gotrue_sign_up) {
  moduleServer(id, function(input, output, session) {
    identity <- reactiveVal(list(mode = NA_character_, user_id = NA_character_, access_token = NA_character_))
    err <- reactiveVal("")

    handle <- function(fn) {
      res <- fn(input$email, input$password)
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
