# Tab 2: readers checking whether reported statistics are consistent.
# Placeholder until reprokit's check_*() functions exist.

mod_check_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Consistency checks"),
    p("Planned: paste reported statistics and see which values cannot co-occur."),
    p("Results say 'consistent with' or 'cannot co-occur'. They never claim fraud or error."),
    verbatimTextOutput(ns("verdicts"))
  )
}

mod_check_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$verdicts <- renderPrint(reprokit::verdicts())
  })
}
