# Reading the app's words and test catalogue from text/*.yml.
#
# load_app_text() reads the files once, when the app starts; the helpers
# below look things up. Keeping the words in YAML keeps them in one place,
# where they can be reviewed without reading R code.

app_store <- new.env(parent = emptyenv())

load_app_text <- function(dir) {
  app_store$text <- read_yaml_keep_words(file.path(dir, "en.yml"))
  app_store$tests <- read_yaml_keep_words(file.path(dir, "tests.yml"))
  app_store$examples <- read_yaml_keep_words(file.path(dir, "examples.yml"))
  invisible(app_store)
}

# YAML 1.1 reads a bare `n:` (or `y:`, `yes`, `no`) as a boolean, which would
# turn the sample-size key into FALSE. Only `true` and `false` stay booleans.
read_yaml_keep_words <- function(file) {
  keep <- function(x) {
    if (tolower(x) %in% c("true", "false")) tolower(x) == "true" else x
  }
  yaml::read_yaml(
    file,
    handlers = list("bool#yes" = keep, "bool#no" = keep)
  )
}

# A text by its path, e.g. txt("share", "steps", "data").
txt <- function(...) {
  value <- app_store$text[[c(...)]]
  if (is.null(value)) {
    stop("No text at ", paste(c(...), collapse = "/"), call. = FALSE)
  }
  value
}

# Fill {name} placeholders: fill_in("{n} rows", n = 5) gives "5 rows".
fill_in <- function(template, ...) {
  values <- list(...)
  for (name in names(values)) {
    template <- gsub(
      paste0("{", name, "}"),
      paste(values[[name]], collapse = ", "),
      template,
      fixed = TRUE
    )
  }
  template
}

# Markdown text as HTML. Inline text loses its surrounding paragraph.
md <- function(text, inline = FALSE) {
  html <- commonmark::markdown_html(
    paste(text, collapse = "\n"),
    extensions = TRUE
  )
  if (inline) {
    html <- sub("^<p>(.*)</p>\n?$", "\\1", html)
  }
  HTML(html)
}
