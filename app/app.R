# reprokit app: a front end for the reprokit package.
# Rule: this app contains no statistics. Every number comes from a
# reprokit:: function; tests/testthat/test-no-statistics.R enforces this.
# Shiny sources app/R/*.R automatically before running this file; the page
# itself is built in R/layout.R.

library(shiny)
library(bslib)

load_app_text("text")
practice <- read_practice("practice")

shinyApp(app_ui(practice), app_server(practice))
