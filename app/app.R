# reprokit app: a front end for the reprokit package.
# Rule: this app contains no statistics. Every number comes from a reprokit::
# function; tests/testthat/test-no-statistics.R enforces this.
# Shiny sources app/R/*.R automatically before running this file.

library(shiny)
library(bslib)

practice <- read_practice("practice")

ui <- page_navbar(
  id = "tabs",
  title = "Reproducibility oracle",
  theme = app_theme(),
  fillable = FALSE,
  header = app_css(),
  nav_spacer(),
  nav_panel("Start here", value = "start", mod_start_ui("start")),
  nav_panel("I can't share my data", value = "share", mod_share_ui("share", practice$share)),
  nav_panel("Does this paper add up?", value = "check", mod_check_ui("check")),
  nav_panel("Practice examples", value = "practice", mod_practice_ui("practice", practice)),
  nav_panel("Help: How the App works", value = "help", mod_help_ui("help")),
  nav_spacer()
)

server <- function(input, output, session) {
  # Each of these returns a reactive that fires with list(tab = , ...) when the
  # user asks to go to another tab; the practice cards also carry their example.
  start <- mod_start_server("start")
  sent <- mod_practice_server("practice", practice)
  mod_share_server("share", practice$share, incoming = sent_to(sent, "share"))
  mod_check_server("check", incoming = sent_to(sent, "check"))
  mod_help_server("help")

  observeEvent(start(), nav_select("tabs", start()$tab))
  observeEvent(sent(), nav_select("tabs", sent()$tab))
}

# The practice example in `sent`, if it was sent to `tab` (else NULL).
sent_to <- function(sent, tab) {
  reactive({
    s <- sent()
    if (identical(s$tab, tab)) s
  })
}

shinyApp(ui, server)
