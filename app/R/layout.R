# The one page: a title bar, a short explanation, and three tabs: check a
# paper, publish data, and help (with the practice examples).

app_ui <- function(practice) {
  page_fillable(
    title = txt("app", "title"),
    theme = app_theme(),
    padding = 0,
    gap = 0,
    app_css(),
    title_bar(),
    div(class = "app-intro", md(txt("app", "intro"), inline = TRUE)),
    mode_tabs(
      nav_panel(txt("tabs", "check"), value = "check",
                icon = icon("magnifying-glass"), mod_check_ui("check")),
      nav_panel(txt("tabs", "share"), value = "share",
                icon = icon("file-export"),
                mod_share_ui("share", practice$share)),
      nav_panel(txt("tabs", "help"), value = "help",
                icon = icon("circle-question"), help_tab(practice))
    )
  )
}

app_server <- function(practice) {
  function(input, output, session) {
    # The practice examples return a reactive that fires with
    # list(tab = , item = ) when an example is sent to one of the tools.
    sent <- mod_practice_server("practice", practice)
    mod_share_server("share", practice$share, incoming = sent_to(sent, "share"))
    mod_check_server("check", incoming = sent_to(sent, "check"))
    observeEvent(sent(), nav_select("mode", sent()$tab))
  }
}

# The practice example in `sent`, if it was sent to `tab` (else NULL).
sent_to <- function(sent, tab) {
  reactive({
    s <- sent()
    if (identical(s$tab, tab)) s
  })
}

title_bar <- function() {
  words <- txt("app", "theme")
  div(
    class = "app-titlebar",
    tags$span(class = "app-brand", icon("circle-nodes"), txt("app", "title")),
    theme_button(words)
  )
}

# Switches between the light and the dark colour scheme (www/theme.js).
# It shows a moon and "Dark mode" in light mode, a sun and "Light mode" in
# dark mode.
theme_button <- function(words) {
  tags$button(
    type = "button", class = "theme-toggle", id = "theme_toggle",
    title = words$hint, `aria-pressed` = "false",
    tags$span(class = "when-light", icon("moon"), words$dark),
    tags$span(class = "when-dark", icon("sun"), words$light)
  )
}

# The square tabs that switch between the tools and the help. Their content
# has no padding: each tool brings its own sidebar.
mode_tabs <- function(...) {
  tabs <- navset_card_tab(
    ...,
    id = "mode",
    wrapper = \(...) card_body(..., padding = 0, gap = 0)
  )
  tagAppendAttributes(tabs, class = "mode-tabs")
}

# Help -----------------------------------------------------------------------

# The Help tab: the practice examples first, then the explanations.
help_tab <- function(practice) {
  words <- txt("help")
  navset_underline(
    id = "help_tabs",
    nav_panel(words$tabs$how, value = "how", help_page(help_how(words))),
    nav_panel(words$tabs$practice, value = "practice",
              mod_practice_ui("practice", practice)),
    nav_panel(words$tabs$results, help_page(help_results(words))),
    nav_panel(words$tabs$words, help_page(help_words())),
    nav_panel(words$tabs$coverage, help_page(help_coverage(words))),
    nav_panel(words$tabs$apa, help_page(help_apa(words))),
    nav_panel(words$tabs$data, help_page(help_data(words)))
  ) |>
    tagAppendAttributes(class = "help-tabs")
}

# A help sub-tab: padded, and scrolling on its own when it is long.
help_page <- function(...) div(class = "help-page", ...)

help_how <- function(words) {
  tabs <- txt("tabs")
  div(
    class = "help-section",
    p(md(words$how_intro, inline = TRUE)),
    help_steps(tabs$check, words$check_steps, "magnifying-glass",
               words$check_for),
    help_steps(tabs$share, words$share_steps, "file-export",
               words$share_for),
    help_steps(tabs$practice, words$practice_steps, "graduation-cap",
               words$practice_for)
  )
}

help_steps <- function(title, steps, icon_name, audience = NULL) {
  div(
    class = "help-steps",
    tags$h6(icon(icon_name), title,
            if (!is.null(audience)) tags$span(class = "text-muted", audience)),
    tags$ol(lapply(unlist(steps), \(step) tags$li(md(step, inline = TRUE))))
  )
}

help_apa <- function(words) {
  div(
    class = "help-section",
    p(class = "help-lead", words$apa_lead),
    p(words$apa_text),
    explain(
      title = words$apa_rule_title,
      p(txt("reminders", "standard")),
      p(words$apa_rule_extra),
      icon_name = "scale-balanced"
    )
  )
}

help_data <- function(words) {
  div(
    class = "help-section",
    tags$h6(icon("shield-halved"), words$data_title),
    p(words$data_text)
  )
}

help_words <- function() {
  glossary <- txt("glossary")
  tags$dl(
    class = "glossary",
    lapply(names(glossary), function(term) {
      div(class = "glossary-item", tags$dt(term), tags$dd(glossary[[term]]))
    })
  )
}

help_results <- function(words) {
  statuses <- txt("statuses")
  div(
    class = "help-section",
    tags$h6(words$statuses_title),
    tags$table(
      class = "table table-sm",
      tags$tbody(lapply(names(statuses), function(status) {
        tags$tr(tags$td(status_badge(status)),
                tags$td(statuses[[status]]$meaning))
      }))
    ),
    tags$h6(words$levels_title),
    level_ladder(-1L),
    tags$h6(class = "mt-3", words$verdicts_title),
    tags$ul(lapply(txt("verdicts"), function(verdict) {
      tags$li(tags$b(paste0(verdict$title, ":")), verdict$meaning)
    })),
    explain(p(txt("reminders", "inconsistency")),
            icon_name = "scale-balanced")
  )
}

help_coverage <- function(words) {
  table <- reprokit::coverage_table()
  div(
    class = "help-section",
    md(words$coverage_text),
    div(
      class = "table-responsive",
      tags$table(
        class = "table table-sm table-striped coverage-table",
        tags$thead(tags$tr(lapply(names(table), tags$th))),
        tags$tbody(lapply(seq_len(nrow(table)), function(i) {
          tags$tr(lapply(table[i, ], \(cell) tags$td(cell)))
        }))
      )
    )
  )
}
