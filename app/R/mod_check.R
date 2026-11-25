# Tab: Check Paper (opens first). Readers type in the numbers a paper
# reports; reprokit::check_reported() says whether they fit together.
# Layout: the steps in the sidebar, three value boxes on top, and tabs below.
# The result tabs appear once there is a result for the numbers in the form.
# Wording rule: "consistent / inconsistent with the reported statistics",
# never "wrong"; an inconsistency never says which number is off.
#
# `incoming` is a reactive that, when it fires, holds a practice example:
# list(item = list(title = , check = list(test, reported, args, claims))).

result_tabs <- c("result", "detail", "choices")

mod_check_ui <- function(id) {
  ns <- NS(id)
  tool_layout(
    fillable = TRUE,
    border = FALSE,
    border_radius = FALSE,
    class = "tool-layout",
    sidebar = tool_sidebar(
      width = 360,
      accordion(
        id = ns("steps"),
        multiple = FALSE,
        open = "test",
        accordion_panel(
          step_title(1, txt("check", "steps", "test")),
          value = "test",
          selectInput(ns("test"), NULL, choices = test_choices("check"),
                      width = "100%"),
          uiOutput(ns("test_help")),
          actionButton(ns("example"), txt("check", "example"),
                       icon = icon("book-open"),
                       class = "btn-outline-primary w-100 mb-2"),
          actionButton(ns("to_claims"), txt("check", "next_step"),
                       icon = icon("arrow-down"),
                       class = "btn-light w-100")
        ),
        accordion_panel(
          step_title(2, txt("check", "steps", "claims")),
          value = "claims",
          p(class = "small", txt("check", "claims_help")),
          uiOutput(ns("claims"))
        )
      ),
      div(
        class = "sidebar-actions",
        actionButton(ns("go"), txt("check", "go"), icon = icon("play"),
                     class = "btn-primary btn-lg w-100"),
        actionButton(ns("clear"), txt("check", "clear"),
                     icon = icon("eraser"), class = "btn-link w-100")
      )
    ),
    uiOutput(ns("boxes"), class = "box-row"),
    navset_card_underline(
      id = ns("tabs"),
      nav_panel(
        txt("check", "tabs", "numbers"),
        value = "numbers",
        layout_columns(
          col_widths = c(9, 3),
          div(
            tags$h5(txt("check", "numbers_title")),
            p(class = "text-muted", txt("check", "numbers_subtitle")),
            uiOutput(ns("form"))
          ),
          div(
            uiOutput(ns("loaded")),
            explain(
              title = txt("check", "exact_title"),
              p(txt("check", "exact_text")),
              icon_name = "ruler"
            ),
            uiOutput(ns("apa_set"))
          )
        )
      ),
      nav_panel(txt("check", "tabs", "result"), value = "result",
                uiOutput(ns("result"))),
      nav_panel(txt("check", "tabs", "numbers_detail"), value = "detail",
                uiOutput(ns("detail"))),
      nav_panel(txt("check", "tabs", "choices"), value = "choices",
                uiOutput(ns("choices")))
    )
  )
}

mod_check_server <- function(id, incoming = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    spec <- reactive(test_entry(req(input$test))$check)

    # Numbers put into the form by an example, a practice card or "Clear":
    # list(test, reported, args, claims, note). They apply to the form of
    # their own test only and are dropped as soon as the user picks another
    # test or form size, so they never overwrite what the user typed. The
    # form is rendered with them as its values, so they appear whatever the
    # timing of the browser.
    filled <- reactiveVal(NULL)
    # The last check, and whether it belongs to the numbers in the form.
    outcome <- reactiveVal(NULL)
    fresh <- reactiveVal(FALSE)

    fill <- function(test, reported = list(), args = list(), claims = list(),
                     note = NULL) {
      filled(list(test = test, reported = reported, args = args,
                  claims = claims, note = note, at = Sys.time()))
      fresh(FALSE)
      updateSelectInput(session, "test", selected = test)
      nav_select("tabs", "numbers")
    }
    current_fill <- reactive({
      f <- filled()
      if (!is.null(f) && identical(f$test, input$test)) f
    })
    shape <- reactive(form_shape(spec(), input, current_fill()))

    observeEvent(input$test, {
      if (!identical(filled()$test, input$test)) filled(NULL)
      fresh(FALSE)
    }, ignoreInit = TRUE)
    observeEvent(
      list(input$shape_cells, input$shape_rows, input$shape_cols,
           input$shape_terms),
      {
        f <- current_fill()
        if (is.null(f)) return()
        if (!identical(form_shape(spec(), input, f),
                       form_shape(spec(), input, NULL))) {
          filled(NULL)
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$example, {
      ex <- app_store$examples[[input$test]]
      fill(input$test, parse_reported(ex$reported),
           args = ex[intersect(names(ex), c("mu", "k", "effect"))],
           note = ex$note)
    })
    observeEvent(input$clear, fill(input$test))
    observeEvent(incoming(), {
      item <- incoming()$item
      x <- item$check
      fill(x$test, parse_reported(x$reported), args = x$args %||% list(),
           claims = x$claims %||% list(),
           note = fill_in(txt("check", "loaded_from_practice"),
                          title = item$title))
    })
    observeEvent(input$to_claims, accordion_panel_open("steps", "claims"))

    # The result tabs show only when there is a result for these numbers.
    observe({
      for (tab in result_tabs) {
        if (fresh()) nav_show("tabs", tab) else nav_hide("tabs", tab)
      }
    })

    # The value a field starts with: the filled-in one, or what it holds now.
    value_for <- function(id, from_fill, default = "") {
      f <- current_fill()
      if (!is.null(f)) return(from_fill(f) %||% default)
      isolate(input[[id]]) %||% default
    }

    # Sidebar -------------------------------------------------------------
    output$test_help <- renderUI({
      entry <- test_entry(input$test)
      explain(
        title = entry$technical,
        p(tags$i("Example: "), entry$example),
        icon_name = "circle-info"
      )
    })

    output$claims <- renderUI({
      spec <- spec()
      claim <- function(kind) {
        value_for(paste0("claim_", kind), \(f) f$claims[[kind]])
      }
      inputs <- list(
        if (length(unlist(spec$tests)) > 1L) {
          selectInput(ns("claim_version"), txt("check", "version_label"),
                      choices = invert(txt("check", "version_choices")),
                      selected = claim("version"))
        },
        if ("tail" %in% unlist(spec$claims)) {
          selectInput(ns("claim_tail"), txt("check", "tail_label"),
                      choices = invert(txt("check", "tail_choices")),
                      selected = claim("tail"))
        }
      )
      inputs <- Filter(Negate(is.null), inputs)
      if (length(inputs) == 0L) {
        return(p(class = "text-muted", txt("check", "nothing_to_claim")))
      }
      tagList(inputs)
    })

    # The claims step may be folded away; its inputs must exist anyway.
    outputOptions(output, "claims", suspendWhenHidden = FALSE)

    # Numbers tab ----------------------------------------------------------
    output$form <- renderUI({
      check_form_ui(ns, spec(), shape(), value_for)
    })

    output$loaded <- renderUI({
      note <- current_fill()$note
      if (is.null(note)) return(NULL)
      explain(
        title = txt("check", "loaded_title"),
        p(note),
        md(txt("check", "loaded_text")),
        icon_name = "book-open"
      )
    })

    output$apa_set <- renderUI({
      row <- coverage_row(input$test)
      if (is.null(row)) return(NULL)
      explain(
        title = test_entry(input$test)$technical,
        p(tags$b(txt("check", "test_reports")), paste0(row$apa, ".")),
        if (row$missing != "—") {
          p(tags$b(txt("check", "test_missing")), paste0(row$missing, "."))
        },
        icon_name = "clipboard-list"
      )
    })

    # Running the check -----------------------------------------------------
    observeEvent(input$go, {
      collected <- collect_form(input, spec(), shape())
      if (!is.null(collected$problem)) {
        showNotification(collected$problem, type = "error")
        return()
      }
      if (length(collected$reported) == 0L) {
        showNotification(txt("check", "need_numbers"), type = "error")
        return()
      }
      outcome(check_numbers(input$test, collected$reported, collected$args,
                            claimed = input$claim_version %||% ""))
      outcome_claims(list(tail = input$claim_tail %||% ""))
      fresh(TRUE)
      nav_select("tabs", "result")
    })
    outcome_claims <- reactiveVal(list())

    shown <- reactive({
      req(fresh())
      outcome()
    })

    output$boxes <- renderUI({
      if (!fresh()) return(check_boxes_waiting())
      check_boxes(outcome())
    })

    output$result <- renderUI({
      r <- shown()
      if (inherits(r, "error")) {
        return(error_box(conditionMessage(r), txt("check", "error_hint")))
      }
      layout_columns(
        col_widths = c(6, 6),
        div(
          check_verdict_ui(r),
          check_version_note(r),
          # An inconsistent verdict already says this; the others need it.
          if (check_state(r$result) != "inconsistent") {
            explain(title = txt("check", "fair_title"),
                    p(txt("reminders", "inconsistency")),
                    icon_name = "scale-balanced")
          }
        ),
        div(check_level_ui(r$result), check_unreported_ui(r$result))
      )
    })

    output$detail <- renderUI({
      r <- shown()
      req(!inherits(r, "error"))
      check_numbers_table(r$result$numbers)
    })

    output$choices <- renderUI({
      r <- shown()
      req(!inherits(r, "error"))
      check_choices_ui(r, outcome_claims())
    })

    invisible(outcome)
  })
}

# Pieces of the results --------------------------------------------------------

check_boxes_waiting <- function() {
  words <- txt("check", "boxes")
  box_strip(
    result_box(words$verdict, words$waiting, words$waiting_hint,
               "scale-balanced", "box-waiting"),
    result_box(words$level, "—", NULL, "layer-group", "box-waiting"),
    result_box(words$numbers, "—", NULL, "list-check", "box-waiting")
  )
}

check_boxes <- function(r) {
  words <- txt("check", "boxes")
  if (inherits(r, "error")) return(check_boxes_waiting())
  x <- r$result
  state <- check_state(x)
  verdict <- txt("check", "verdicts", state)
  counts <- status_counts(x$numbers)
  box_strip(
    result_box(words$verdict, verdict$short, NULL,
               c(consistent = "circle-check", inconsistent = "circle-xmark",
                 unchecked = "circle-question")[[state]],
               paste0("box-", state)),
    result_box(words$level, fill_in(words$level_value, level = x$level),
               unlist(txt("levels"))[[x$level + 1L]], "layer-group"),
    result_box(
      words$numbers,
      fill_in(words$numbers_value, checked = counts$checked,
              total = counts$total),
      fill_in(words$numbers_hint, inconsistent = counts$inconsistent,
              unconfirmed = counts$unconfirmed),
      "list-check"
    )
  )
}

check_state <- function(x) {
  if (!isTRUE(x$checked)) return("unchecked")
  if (isTRUE(x$consistent)) "consistent" else "inconsistent"
}

check_verdict_ui <- function(r) {
  state <- check_state(r$result)
  words <- txt("check", "verdicts", state)
  meaning <- words$text %||% txt("reminders", "inconsistency")
  icons <- c(consistent = "circle-check", inconsistent = "circle-xmark",
             unchecked = "circle-question")
  classes <- c(consistent = "success", inconsistent = "danger",
               unchecked = "secondary")
  verdict_banner(icons[[state]], words$title, meaning, classes[[state]])
}

# For a test with two versions (Student, Welch): what the numbers fit,
# compared with what the paper says.
check_version_note <- function(r) {
  if (length(r$fits) < 2L) return(NULL)
  words <- txt("check", "version_note")
  fits <- names(r$fits)[r$fits]
  claimed <- r$claimed
  text <- if (nzchar(claimed) && !isTRUE(r$fits[[claimed]]) &&
                length(fits) > 0L) {
    fill_in(words$fits_other, claimed = variant_label(claimed),
            other = variant_label(fits))
  } else if (length(fits) == 2L) {
    words$both
  } else if (length(fits) == 1L && !nzchar(claimed)) {
    fill_in(words$one, fits = variant_label(fits))
  } else if (length(fits) == 0L) {
    words$none
  }
  if (is.null(text)) return(NULL)
  explain(title = txt("variant_kinds", "test"), p(text),
          icon_name = "code-compare")
}

check_level_ui <- function(x) {
  words <- txt("check")
  card(
    card_header(
      words$level_title, " ",
      help_icon(words$level_help)
    ),
    card_body(
      level_ladder(x$level),
      if (nrow(x$missing) > 0L) {
        explain(
          title = words$raise_title,
          tags$ul(lapply(seq_len(nrow(x$missing)), function(i) {
            row <- x$missing[i, ]
            range <- if (is.na(row$implied_range)) {
              "."
            } else {
              fill_in(words$raise_range, range = row$implied_range)
            }
            tags$li(paste0(
              fill_in(words$raise_item, quantity = quantity_label(row$quantity),
                      level = row$would_reach_level),
              range
            ))
          })),
          icon_name = "arrow-trend-up"
        )
      }
    )
  )
}

check_unreported_ui <- function(x) {
  implied <- x$implied
  derived <- x$implied_inputs
  if (length(implied) == 0L && length(derived) == 0L) return(NULL)
  words <- txt("check")
  explain(
    title = words$unreported_title,
    if (length(implied) > 0L) {
      tagList(
        p(words$unreported_text),
        tags$ul(lapply(names(implied), function(q) {
          range <- implied[[q]]
          tags$li(tags$b(quantity_label(q)), ": ",
                  show_number(range$lower), " to ", show_number(range$upper))
        }))
      )
    },
    if (length(derived) > 0L) {
      tagList(
        p(words$derived_text),
        tags$ul(lapply(names(derived), function(q) {
          d <- derived[[q]]
          value <- if (d$lower == d$upper) {
            d$lower
          } else {
            paste(d$lower, "to", d$upper)
          }
          tags$li(fill_in(words$derived_item, quantity = quantity_label(q),
                          value = value, from = quantity_label(d$from)))
        }))
      )
    },
    icon_name = "arrows-left-right-to-line"
  )
}

check_numbers_table <- function(numbers) {
  words <- txt("check", "columns")
  statuses <- txt("statuses")
  div(
    class = "table-responsive",
    tags$table(
      class = "table table-sm numbers-table",
      tags$thead(tags$tr(lapply(
        c(words$number, words$reported, words$status, words$meaning,
          words$checked_with),
        tags$th
      ))),
      tags$tbody(lapply(seq_len(nrow(numbers)), function(i) {
        status <- numbers$status[i]
        tags$tr(
          tags$td(quantity_label(numbers$quantity[i])),
          tags$td(numbers$reported[i]),
          tags$td(status_badge(status)),
          tags$td(statuses[[status]]$meaning %||% ""),
          tags$td(tags$small(checked_with_words(numbers$checked_against[i])))
        )
      }))
    )
  )
}

check_choices_ui <- function(r, claims) {
  words <- txt("check")
  variants <- r$result$variants
  # A two-version test was checked once per version: show both.
  if (length(r$fits) > 1L) {
    variants$test <- data.frame(variant = names(r$fits), fits = unname(r$fits))
  }
  kinds <- Filter(\(kind) nrow(variants[[kind]]) > 0L, names(variants))
  rows <- lapply(kinds, function(kind) {
    v <- variants[[kind]]
    tags$tr(
      tags$th(txt("variant_kinds")[[kind]] %||% kind),
      tags$td(lapply(seq_len(nrow(v)), function(i) {
        fits <- isTRUE(v$fits[i])
        tags$span(
          class = paste0("badge me-1 mb-1 text-bg-",
                         if (fits) "success" else "light"),
          icon(if (fits) "check" else "xmark"), " ",
          variant_label(v$variant[i])
        )
      }))
    )
  })
  layout_columns(
    col_widths = c(7, 5),
    if (length(rows) > 0L) {
      tags$table(class = "table table-sm", tags$tbody(rows))
    } else {
      p(class = "text-muted", words$choices_none)
    },
    div(
      check_claims_ui(variants, claims),
      explain(p(words$choices_help), icon_name = "circle-info")
    )
  )
}

check_claims_ui <- function(variants, claims) {
  claim <- claims$tail %||% ""
  v <- variants$tail
  if (!nzchar(claim) || is.null(v) || nrow(v) == 0L) return(NULL)
  words <- txt("check")
  fitting <- v$variant[v$fits]
  text <- if (claim %in% fitting) {
    fill_in(words$claim_fits, claim = variant_label(claim))
  } else {
    fits <- if (length(fitting) > 0L) {
      paste(variant_label(fitting), collapse = ", ")
    } else {
      words$none_of_them
    }
    fill_in(words$claim_misfit, claim = variant_label(claim), fits = fits)
  }
  explain(title = words$claims_title, md(text), icon_name = "code-compare")
}
