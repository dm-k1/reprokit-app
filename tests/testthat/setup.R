library(shiny)
library(bslib)
app_dir <- testthat::test_path("..", "..", "app")
for (file in list.files(file.path(app_dir, "R"), full.names = TRUE)) {
  source(file, local = FALSE)
}
load_app_text(file.path(app_dir, "text"))
practice <- read_practice(file.path(app_dir, "practice"))
