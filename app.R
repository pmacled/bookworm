source("global.R")

ui <- page_fillable(
  title = APP_CONFIG$app_name,
  theme = app_theme,
  tags$head(tags$link(rel = "stylesheet", href = "css/app.css")),
  tags$div(class = "p-4", tags$h2("Bookworm"), tags$p("Scaffold OK."))
)

server <- function(input, output, session) {}

shinyApp(ui, server)
