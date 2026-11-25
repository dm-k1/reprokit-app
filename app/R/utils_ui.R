# Small UI building blocks shared by the tabs. Layout only, no statistics.

# Theme -------------------------------------------------------------------------
# Purple for the app's own chrome; for results, the Okabe-Ito colours (blue, sky
# blue, orange, vermillion), which stay distinguishable with every common form of
# colour blindness. Results never rely on colour alone: each also has an icon and
# a word.
app_theme <- function() {
  bs_theme(
    version = 5,
    primary = "#5B2C83",
    secondary = "#6B6480",
    success = "#0F5A94",
    info = "#56B4E9",
    warning = "#E69F00",
    danger = "#B34700",
    light = "#F4F0FA",
    dark = "#2A1A3E",
    fg = "#231B2E",
    bg = "#FFFFFF",
    base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto",
                                "Helvetica Neue", "Arial", "sans-serif"),
    "border-radius" = ".6rem",
    "card-border-color" = "#E4DCF0"
  )
}

# The stylesheet lives in www/app.css.
app_css <- function() {
  tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
}

# Page structure ----------------------------------------------------------------

# Every tab sits in the same centred column.
page_body <- function(...) div(class = "page-body", ...)

# Centred title and one-line subtitle at the top of a tab.
page_intro <- function(title, subtitle) {
  div(class = "page-intro", tags$h1(title), p(subtitle))
}

# A numbered step heading, so users always know where they are.
step_header <- function(number, title, subtitle = NULL) {
  card_header(
    div(class = "step-header",
        tags$span(class = "step-number", number),
        div(div(class = "step-title", title),
            if (!is.null(subtitle)) div(class = "step-subtitle", subtitle)))
  )
}

# Explanations --------------------------------------------------------------------

# A round "?" that opens an explanation when clicked (works on touch screens too).
help_icon <- function(text, title = NULL) {
  popover(
    tags$span(class = "help-icon", role = "button", tabindex = "0",
              `aria-label` = "More information", "?"),
    text,
    title = title
  )
}

# An input label with its help icon.
label_with_help <- function(label, text) {
  tags$span(label, " ", help_icon(text, title = label))
}

# A soft box with an explanation, always visible.
explain <- function(..., title = NULL, icon = "\U0001F4A1") {
  div(class = "explain-box",
      if (!is.null(title)) div(class = "explain-title", icon, " ", title),
      ...)
}

# Results -------------------------------------------------------------------------

# A coloured badge with an icon and a word, never colour alone.
status_badge <- function(status) {
  info <- status_info()[[status]]
  if (is.null(info)) return(tags$span(class = "badge text-bg-light", status))
  tags$span(class = paste0("badge status-badge text-bg-", info[2L]),
            `aria-label` = status, info[1L], " ", status)
}

# The 0-3 coverage ladder with the current level highlighted.
level_ladder <- function(level) {
  steps <- level_steps_text()
  tags$ol(class = "level-ladder",
          lapply(seq_along(steps) - 1L, function(i) {
            state <- if (i == level) "current" else if (i < level) "done"
            tags$li(class = paste("level-step", state),
                    tags$span(class = "level-number", i),
                    tags$span(steps[i + 1L]))
          }))
}

# A big banner for a verdict: icon, title, one-sentence meaning.
verdict_banner <- function(icon, title, meaning, class = "info", detail = NULL) {
  div(class = paste0("alert alert-", class, " verdict-banner"), role = "status",
      div(class = "verdict-icon", icon),
      div(div(class = "verdict-title", title),
          div(meaning),
          if (!is.null(detail)) div(class = "verdict-detail", detail)))
}

# A friendly message box for an error from reprokit.
error_box <- function(message, hint = NULL) {
  div(class = "alert alert-warning", role = "alert",
      tags$b("The app could not do this yet. "), message,
      if (!is.null(hint)) p(class = "mb-0 mt-2", hint))
}

# What a results column shows before the first run.
results_placeholder <- function(title, intro, gets) {
  card(class = "placeholder-card", card_body(
    tags$h4(title),
    p(intro),
    explain(title = "What you will get", tags$ul(lapply(gets, tags$li)))
  ))
}

# Render markdown text (the appendix) as HTML.
markdown_html <- function(lines) {
  HTML(commonmark::markdown_html(paste(lines, collapse = "\n"), extensions = TRUE))
}

# Format a number for display only (never used to compute anything).
show_number <- function(x, digits = 4L) {
  format(x, digits = digits, nsmall = 0L, scientific = FALSE, trim = TRUE)
}

# `x`, or `default` when `x` is NULL (base R has this from 4.4 on).
`%||%` <- function(x, default) if (is.null(x)) default else x
