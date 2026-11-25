# Small UI building blocks shared by the tabs. Layout only, no statistics.
# Icons come from Font Awesome (shiny::icon()); the app uses no emoji.

`%||%` <- function(x, default) if (is.null(x)) default else x

# Theme ------------------------------------------------------------------------
# The Open Science Framework's palette (from OSF's own stylesheets): a navy
# title bar, OSF blue, teal and slate. Results use blue for "fits", red for
# "does not fit" and orange for remarks: blue against red and orange stays
# distinguishable with the common forms of colour blindness, and no result
# relies on colour alone (each also has an icon and a word).
app_theme <- function() {
  bs_theme(
    version = 5,
    primary = "#337ab7",
    secondary = "#6c757d",
    success = "#2d6a9f",
    info = "#80c2f7",
    warning = "#f0ad4e",
    danger = "#b52b27",
    light = "#ecf0f1",
    dark = "#263947",
    fg = "#333333",
    bg = "#ffffff",
    base_font = font_collection(
      "Open Sans", "Helvetica Neue", "Helvetica", "Arial", "sans-serif"
    ),
    "border-radius" = ".35rem"
  )
}

# The stylesheet lives in www/app.css.
app_css <- function() {
  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
    tags$script(src = "theme.js")
  )
}

# Explanations -----------------------------------------------------------------

# A round "?" that opens an explanation when clicked (works on touch screens).
help_icon <- function(text, title = NULL) {
  popover(
    tags$span(
      class = "help-icon", role = "button", tabindex = "0",
      `aria-label` = "More information", "?"
    ),
    text,
    title = title
  )
}

label_with_help <- function(label, text) {
  tags$span(label, " ", help_icon(text, title = label))
}

# A soft box with an explanation, always visible.
explain <- function(..., title = NULL, icon_name = "lightbulb") {
  div(
    class = "explain-box",
    if (!is.null(title)) {
      div(class = "explain-title", icon(icon_name), " ", title)
    },
    ...
  )
}

# The title of a numbered step in a sidebar accordion.
step_title <- function(number, title) {
  tags$span(
    class = "step-title",
    tags$span(class = "step-number", number),
    title
  )
}

# Results ----------------------------------------------------------------------

status_badge <- function(status) {
  info <- txt("statuses")[[status]]
  if (is.null(info)) return(tags$span(class = "badge text-bg-light", status))
  tags$span(
    class = paste0("badge status-badge text-bg-", info$class),
    icon(info$icon), " ", status
  )
}

# The 0-3 coverage ladder with the current level highlighted.
level_ladder <- function(level) {
  steps <- unlist(txt("levels"))
  tags$ol(
    class = "level-ladder",
    lapply(seq_along(steps) - 1L, function(i) {
      state <- if (i == level) "current" else if (i < level) "done"
      tags$li(
        class = paste("level-step", state),
        tags$span(class = "level-number", i),
        tags$span(steps[[i + 1L]])
      )
    })
  )
}

verdict_banner <- function(icon_name, title, meaning, class = "info",
                           detail = NULL) {
  div(
    class = paste0("alert alert-", class, " verdict-banner"),
    role = "status",
    div(class = "verdict-icon", icon(icon_name)),
    div(
      div(class = "verdict-title", title),
      div(meaning),
      if (!is.null(detail)) div(class = "verdict-detail", detail)
    )
  )
}

error_box <- function(message, hint = NULL) {
  div(
    class = "alert alert-warning", role = "alert",
    tags$b("The app could not do this yet. "), message,
    if (!is.null(hint)) p(class = "mb-0 mt-2", hint)
  )
}

# A sidebar layout with `class` on the whole layout (bslib puts a `class`
# given to layout_sidebar() on the main area only), so the app's styles can
# reach both the sidebar and the main area.
tool_layout <- function(..., class) {
  layout_sidebar(...) |>
    tagAppendAttributes(class = class)
}

# A sidebar that is always open and has a fixed width: no button to fold it
# away and no edge to drag (newer bslib versions add one unless told not to).
tool_sidebar <- function(...) {
  args <- list(..., open = "always")
  if ("resizable" %in% names(formals(sidebar))) args$resizable <- FALSE
  do.call(sidebar, args)
}

# A result box: the title on a small first line, then the value and its hint.
result_box <- function(title, value, hint = NULL, icon_name = "circle-info",
                       class = NULL) {
  div(
    class = paste("oracle-box", class),
    div(class = "box-title", icon(icon_name), title),
    div(
      class = "box-line",
      tags$span(class = "box-value", value),
      if (!is.null(hint)) tags$span(class = "box-hint", hint)
    )
  )
}

# The three result boxes side by side.
box_strip <- function(...) div(class = "box-strip", ...)

# Format a number for display only (never used to compute anything).
show_number <- function(x, digits = 4L) {
  format(x, digits = digits, nsmall = 0L, scientific = FALSE, trim = TRUE)
}
