# Tab 1: authors who cannot share raw data.
# The stated reason for not sharing is required before anything is produced
# (plan: the tool must not become a respectable excuse for not sharing).

mod_share_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      textAreaInput(ns("sharing_reason"), "Why can't the raw data be shared? (required)"),
      fileInput(ns("model"), "Fitted model saved with saveRDS()", accept = ".rds"),
      actionButton(ns("go"), "Triage", class = "btn-primary")
    ),
    card(card_header("Verdict"), verbatimTextOutput(ns("verdict"))),
    card(card_header("What reprokit covers"), tableOutput(ns("coverage")))
  )
}

mod_share_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    result <- eventReactive(input$go, {
      validate(
        need(nzchar(trimws(input$sharing_reason)), "State why the raw data cannot be shared."),
        need(input$model, "Upload a fitted model (.rds).")
      )
      reprokit::triage(readRDS(input$model$datapath))
    })
    output$verdict <- renderPrint(result())
    output$coverage <- renderTable(reprokit::coverage_table())
    invisible(result)
  })
}
