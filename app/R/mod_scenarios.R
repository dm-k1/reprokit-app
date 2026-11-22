# Tab 3: class scenarios, one YAML file per scenario in app/scenarios/.

read_scenarios <- function(dir) {
  files <- list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE)
  files <- files[basename(files) != "template.yml"]
  stats::setNames(lapply(files, yaml::read_yaml), tools::file_path_sans_ext(basename(files)))
}

mod_scenarios_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(selectInput(ns("pick"), "Scenario", choices = NULL)),
    card(card_header(textOutput(ns("title"))), uiOutput(ns("body")))
  )
}

mod_scenarios_server <- function(id, dir) {
  moduleServer(id, function(input, output, session) {
    scenarios <- read_scenarios(dir)
    updateSelectInput(session, "pick", choices = names(scenarios))
    current <- reactive({
      validate(need(length(scenarios) > 0, "No scenarios yet: copy scenarios/template.yml."))
      req(input$pick)
      scenarios[[input$pick]]
    })
    output$title <- renderText(current()$title)
    output$body <- renderUI(tagList(
      p(current()$situation),
      tags$b("Question: "), current()$question
    ))
    invisible(current)
  })
}
