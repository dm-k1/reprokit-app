# reprokit app: a front end for the reprokit package.
# Rule: this app contains no statistics. Every number comes from a reprokit::
# function; tests/testthat/test-no-statistics.R enforces this.
# Shiny sources app/R/*.R automatically before running this file.

library(shiny)
library(bslib)

ui <- page_navbar(
  title = "Reproducibility oracle",
  theme = bs_theme(version = 5),
  nav_panel("I can't share my data", mod_share_ui("share")),
  nav_panel("Does this paper add up?", mod_check_ui("check")),
  nav_panel("Class scenarios", mod_scenarios_ui("scenarios")),
  nav_spacer(),
  nav_item(tags$small("Sharing raw data remains the standard."))
)

server <- function(input, output, session) {
  mod_share_server("share")
  mod_check_server("check")
  mod_scenarios_server("scenarios", dir = "scenarios")
}

shinyApp(ui, server)
