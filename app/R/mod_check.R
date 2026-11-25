# Tab: "Does this paper add up?". Readers type in the numbers a paper reports;
# reprokit::check_reported() says whether they fit together.
# Wording rule (plan section 6): "consistent / inconsistent with the reported
# statistics", never "wrong"; an inconsistency never says which number is off.
#
# `incoming` is a reactive that, when it fires, holds a practice example to load:
# list(item = list(title = , check = list(handler, reported, mu, declared))).

mod_check_ui <- function(id) {
  ns <- NS(id)
  page_body(
    page_intro("Does this paper add up?",
               paste("Type in the numbers a paper reports and see whether they can all be",
                     "correct together.")),
    layout_columns(
      col_widths = c(6, 6),
      div(
        class = "stack",
        card(
          step_header(1, "Which test does the paper report?"),
          selectInput(ns("handler"), NULL, choices = test_choices(), width = "100%"),
          uiOutput(ns("test_help")),
          actionButton(ns("example"), "See a practice example", class = "btn-outline-primary")
        ),
        card(
          step_header(2, "Type the numbers exactly as the paper prints them",
                      "Leave a box empty if the paper does not report that number"),
          uiOutput(ns("loaded")),
          explain(
            title = "Why \"exactly as printed\" matters",
            p("A printed number stands for every value that rounds to it: 2.85 means anything",
              "from 2.845 to 2.855. The check uses that whole range, so keep the decimals the",
              "paper used (2.850 is a more precise claim than 2.85). For p you can type .010,",
              "0.010 or < .001.")
          ),
          conditionalPanel(
            "input.handler == 'anova_oneway'", ns = ns,
            numericInput(ns("groups"), "How many groups?", value = 3, min = 2, max = 8, step = 1)
          ),
          uiOutput(ns("form"))
        ),
        card(
          step_header(3, "What does the paper say it did?", "Optional"),
          explain(p("The check does not take this on trust: it tries every option and reports",
                    "which fit. If you say what the paper claims, the app compares the two."),
                  icon = "\u2139\uFE0F"),
          uiOutput(ns("declared")),
          layout_columns(
            col_widths = c(8, 4),
            actionButton(ns("go"), "Check these numbers", class = "btn-primary btn-lg w-100"),
            actionButton(ns("clear"), "Clear the form", class = "btn-outline-secondary w-100")
          )
        )
      ),
      uiOutput(ns("results"))
    )
  )
}

mod_check_server <- function(id, incoming = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    entry <- reactive(test_catalogue()[[req(input$handler)]])

    # Numbers put into the form by an example, a practice card or "Clear":
    # list(handler, reported, mu, declared, note). They apply to the form of their
    # own test only and are dropped as soon as the user picks another test or
    # group count, so they never overwrite what the user typed. The form is
    # rendered with them as its values, so they appear whatever the timing.
    filled <- reactiveVal(NULL)
    # TRUE once the results shown belong to the numbers in the form.
    fresh <- reactiveVal(FALSE)

    fill <- function(handler, reported = list(), mu = 0, declared = list(), note = NULL) {
      filled(list(handler = handler, reported = reported, mu = mu, declared = declared,
                  note = note, at = Sys.time()))
      fresh(FALSE)
      updateSelectInput(session, "handler", selected = handler)
      if (length(reported$n) > 1L && is.na(test_catalogue()[[handler]]$check$cells)) {
        updateNumericInput(session, "groups", value = length(reported$n))
      }
    }
    current_fill <- reactive({
      f <- filled()
      if (!is.null(f) && identical(f$handler, input$handler)) f
    })
    observeEvent(input$handler, {
      if (!is.null(filled()) && !identical(filled()$handler, input$handler)) filled(NULL)
    })
    observeEvent(input$groups, {
      n <- current_fill()$reported$n
      if (length(n) > 1L && !isTRUE(length(n) == input$groups)) filled(NULL)
    })

    observeEvent(input$example, {
      ex <- check_examples()[[input$handler]]
      fill(input$handler, ex$reported, ex$mu %||% 0, note = ex$note)
    })
    observeEvent(input$clear, fill(input$handler))
    observeEvent(incoming(), {
      item <- incoming()$item
      x <- item$check
      fill(x$handler, x$reported, x$mu %||% 0, x$declared %||% list(),
           note = paste0("\u201C", item$title, "\u201D, from Practice examples. The story is ",
                         "on its card; here you only need the numbers."))
    })

    # The value a field starts with: the filled-in one, or what it holds now.
    start_value <- function(id, from_fill, default = "") {
      f <- current_fill()
      if (!is.null(f)) return(from_fill(f) %||% default)
      isolate(input[[id]]) %||% default
    }
    nth <- function(x, i) if (length(x) >= i) x[[i]]

    n_cells <- reactive({
      cells <- entry()$check$cells
      if (!is.na(cells)) return(cells)
      n <- current_fill()$reported$n
      if (length(n) > 1L) return(length(n))
      groups <- suppressWarnings(as.integer(input$groups))
      if (length(groups) == 1L && isTRUE(groups >= 2L)) groups else 3L
    })

    output$test_help <- renderUI({
      e <- entry()
      row <- rk_coverage_row(input$handler)
      explain(
        title = e$technical,
        p(tags$i("Example: "), e$example),
        if (!is.null(row)) p(tags$b("A paper following APA style reports: "), paste0(row$apa, ".")),
        if (!is.null(row) && row$missing != "\u2014") {
          p(tags$b("What APA leaves out but a full check needs: "), paste0(row$missing, "."))
        }
      )
    })

    output$loaded <- renderUI({
      note <- current_fill()$note
      if (is.null(note)) return(NULL)
      explain(title = "Practice example loaded", icon = "\U0001F4D6",
              p(note),
              p("The numbers are in the boxes below. Press", tags$b("Check these numbers"),
                "in step 3 to see what the oracle says."))
    })

    # The form -------------------------------------------------------------
    output$form <- renderUI({
      e <- entry()
      k <- n_cells()
      cell_names <- e$check$cell_names %||% paste("Group", seq_len(k))
      tagList(
        if (length(e$check$per_cell) > 0L) {
          tagList(
            tags$h6(if (k > 1L) "Descriptives per group or condition" else "Descriptives"),
            div(class = "cell-grid table-responsive",
                tags$table(class = "table table-sm align-middle",
                           tags$thead(tags$tr(tags$th(""), lapply(cell_names, tags$th))),
                           tags$tbody(lapply(e$check$per_cell, function(q) {
                             tags$tr(
                               tags$th(label_with_help(quantity_label(q), quantity_help(q))),
                               lapply(seq_len(k), function(i) {
                                 cell_id <- paste0("cell_", q, "_", i)
                                 value <- start_value(cell_id, function(f) nth(f$reported[[q]], i))
                                 tags$td(textInput(ns(cell_id), NULL, value = value,
                                                   width = "100%"))
                               })
                             )
                           }))))
          )
        },
        tags$h6("Test results"),
        do.call(layout_column_wrap, c(
          list(width = "12rem"),
          lapply(e$check$single, function(q) {
            single_id <- paste0("num_", q)
            textInput(ns(single_id), label_with_help(quantity_label(q), quantity_help(q)),
                      value = start_value(single_id, function(f) f$reported[[q]]))
          })
        )),
        if (isTRUE(e$has_mu)) {
          mu_help <- paste("The value the mean (or the mean difference) was compared with. It is",
                           "a setting of the test, not a reported number, so type it as a plain",
                           "number. Usually 0.")
          numericInput(ns("mu"), label_with_help("Test value (mu)", mu_help),
                       value = start_value("mu", function(f) f$mu, default = 0))
        }
      )
    })

    output$declared <- renderUI({
      e <- entry()
      if (length(e$declarable) == 0L) {
        return(p(class = "text-muted", "Nothing to declare for this test."))
      }
      declared <- function(kind) {
        start_value(paste0("declared_", kind), function(f) f$declared[[kind]])
      }
      tagList(
        if ("test" %in% e$declarable) {
          selectInput(ns("declared_test"), "Test version the paper names",
                      c("Not stated" = "", "Student's t (equal variances)" = "student",
                        "Welch's t (unequal variances)" = "welch"), selected = declared("test"))
        },
        if ("tail" %in% e$declarable) {
          selectInput(ns("declared_tail"), "Tails the paper names",
                      c("Not stated" = "", "two-sided" = "two.sided",
                        "one-sided (less)" = "less", "one-sided (greater)" = "greater"),
                      selected = declared("tail"))
        }
      )
    })

    # Run the check -----------------------------------------------------------
    observeEvent(input$go, fresh(TRUE), priority = 1)

    result <- eventReactive(input$go, {
      collected <- collect_reported(input, entry(), n_cells())
      validate(need(is.null(collected$problem), collected$problem))
      validate(need(length(collected$reported) > 0L,
                    "Type at least a few of the numbers the paper reports."))
      args <- c(list(handler = input$handler), collected$reported)
      if (isTRUE(entry()$has_mu)) args$mu <- if (isTRUE(is.finite(input$mu))) input$mu else 0
      x <- tryCatch(do.call(reprokit::check_reported, args), error = function(e) e)
      list(check = x,
           declared = list(test = input$declared_test, tail = input$declared_tail))
    })

    output$results <- renderUI({
      if (!fresh()) return(check_placeholder())
      r <- result()
      x <- r$check
      if (inherits(x, "error")) {
        return(error_box(conditionMessage(x),
                         "Numbers are typed as printed, for example 2.85, .010 or < .001."))
      }
      tagList(
        check_verdict_ui(x),
        check_level_ui(x),
        check_variants_ui(x, r$declared),
        check_numbers_ui(x),
        check_unreported_ui(x),
        explain(p(inconsistency_caveat()), title = "Reading the result fairly",
                icon = "\u2696\uFE0F"),
        accordion(open = FALSE, accordion_panel(
          "Technical output (the package's own printout)",
          verbatimTextOutput(ns("technical"))
        ))
      )
    })

    output$technical <- renderPrint({
      x <- result()$check
      req(!inherits(x, "error"))
      print(x)
    })

    invisible(result)
  })
}

# Reading the form ---------------------------------------------------------------

# The reported numbers as reprokit wants them: strings, named by quantity, one
# value per group for per-group quantities. Returns list(reported, problem).
collect_reported <- function(input, e, k) {
  reported <- list()
  for (q in e$check$per_cell) {
    values <- vapply(seq_len(k), function(i) {
      v <- input[[paste0("cell_", q, "_", i)]]
      if (is.null(v)) "" else trimws(v)
    }, character(1))
    if (all(!nzchar(values))) next
    if (any(!nzchar(values))) {
      problem <- paste0(quantity_label(q), ": fill it in for every group or condition, ",
                        "or leave it empty for all.")
      return(list(reported = NULL, problem = problem))
    }
    reported[[q]] <- values
  }
  for (q in e$check$single) {
    v <- input[[paste0("num_", q)]]
    if (!is.null(v) && nzchar(trimws(v))) reported[[q]] <- trimws(v)
  }
  list(reported = reported, problem = NULL)
}

# Pieces of the result panel ------------------------------------------------------

check_placeholder <- function() {
  results_placeholder(
    "The check will appear here",
    tagList("Pick the test, type the numbers, and press", tags$b("Check these numbers."),
            "Not sure what to type? Press", tags$b("See a practice example"), "first."),
    c("Whether the numbers can all be correct together, allowing for rounding.",
      "How much of the result could be checked at all (level 0 to 3).",
      "Which analysis choices fit (Student or Welch, one- or two-sided, ...).",
      "A status for every number, with what it means.",
      "For numbers the paper leaves out: the range they would have to lie in.")
  )
}

check_verdict_ui <- function(x) {
  if (!isTRUE(x$checked)) {
    why <- paste("None of the formulas of this test had all its numbers. Usually a statistic, its",
                 "df and p are enough for a first check; adding n, M and SD lets the app",
                 "recompute the statistic itself.")
    return(verdict_banner("\u2753", "No check was possible", why, class = "secondary"))
  }
  if (isTRUE(x$consistent)) {
    verdict_banner("\u2705", "Consistent with the reported statistics", class = "success",
                   paste("After allowing for rounding and for the usual analysis choices, these",
                         "numbers can all be correct together. That does not show the analysis",
                         "was appropriate, only that the numbers fit each other."))
  } else {
    verdict_banner("\u26A0\uFE0F", "Inconsistent with the reported statistics", class = "danger",
                   inconsistency_caveat())
  }
}

check_level_ui <- function(x) {
  card(
    card_header(tags$span("How much could be checked: level ", x$level, " of 3 ",
                          help_icon(paste("The level describes coverage, not agreement. A paper",
                                          "can be fully consistent and still be checkable only",
                                          "at level 1.")))),
    level_ladder(x$level),
    p(class = "mt-2 mb-0", tags$i(rk_level_words(x$level))),
    if (nrow(x$missing) > 0L) {
      explain(
        title = "What would raise the level",
        tags$ul(lapply(seq_len(nrow(x$missing)), function(i) {
          row <- x$missing[i, ]
          range_text <- if (is.na(row$implied_range)) {
            "."
          } else {
            paste0(". From the other numbers, it must lie between ", row$implied_range, ".")
          }
          tags$li(tags$b(quantity_label(row$quantity)),
                  paste0(" would raise it to level ", row$would_reach_level, range_text))
        }))
      )
    }
  )
}

variant_kind_names <- function() {
  c(tail = "Tails", level = "Confidence level", test = "Test version",
    convention = "Effect-size formula")
}

check_variants_ui <- function(x, declared) {
  kinds <- Filter(function(k) nrow(x$variants[[k]]) > 0L, names(x$variants))
  if (length(kinds) == 0L) return(NULL)
  rows <- lapply(kinds, function(kind) {
    v <- x$variants[[kind]]
    tags$tr(
      tags$th(variant_kind_names()[[kind]]),
      tags$td(lapply(seq_len(nrow(v)), function(i) {
        tags$span(class = paste0("badge me-1 mb-1 text-bg-", if (v$fits[i]) "success" else "light"),
                  if (v$fits[i]) "\u2714 " else "\u2716 ", rk_label(v$variant[i]))
      }))
    )
  })
  mismatches <- lapply(c("test", "tail"), function(kind) {
    claim <- declared[[kind]]
    v <- x$variants[[kind]]
    if (is.null(claim) || !nzchar(claim) || is.null(v) || nrow(v) == 0L) return(NULL)
    fitting <- v$variant[v$fits]
    if (claim %in% fitting) {
      return(tags$li(HTML(paste0("The paper says <b>", rk_label(claim),
                                 "</b>, and the numbers fit it."))))
    }
    fit_text <- if (length(fitting) > 0L) {
      paste0("<b>", paste(rk_label(fitting), collapse = ", "), "</b>")
    } else {
      "none of the options"
    }
    tags$li(HTML(paste0("The paper says <b>", rk_label(claim), "</b>, but the numbers fit ",
                        fit_text, ". The numbers may be fine and the method description loose, ",
                        "or the other way round.")))
  })
  mismatches <- Filter(Negate(is.null), mismatches)
  card(
    card_header(tags$span("Which analysis choices fit ",
                          help_icon(paste("The check tries every common option and keeps the ones",
                                          "under which all the numbers fit. All numbers of one",
                                          "result must fit the same choice: a t that fits only",
                                          "Student next to a CI that fits only Welch does not",
                                          "count as consistent.")))),
    tags$table(class = "table table-sm", tags$tbody(rows)),
    if (length(mismatches) > 0L) explain(title = "Compared with what the paper says",
                                         tags$ul(mismatches), icon = "\U0001F50E")
  )
}

check_numbers_ui <- function(x) {
  n <- x$numbers
  if (nrow(n) == 0L) return(NULL)
  card(
    card_header("Number by number"),
    div(style = "overflow-x: auto;",
        tags$table(class = "table table-sm numbers-table",
                   tags$thead(tags$tr(tags$th("Number"), tags$th("Reported"), tags$th("Status"),
                                      tags$th("What that means"), tags$th("Checked with"))),
                   tags$tbody(lapply(seq_len(nrow(n)), function(i) {
                     info <- status_info()[[n$status[i]]]
                     tags$tr(tags$td(quantity_label(n$quantity[i])), tags$td(n$reported[i]),
                             tags$td(status_badge(n$status[i])),
                             tags$td(if (is.null(info)) "" else info[3L]),
                             tags$td(tags$small(checked_against_words(n$checked_against[i]))))
                   }))))
  )
}

check_unreported_ui <- function(x) {
  implied <- x$implied
  derived <- x$implied_inputs
  if (length(implied) == 0L && length(derived) == 0L) return(NULL)
  card(
    card_header("Numbers the paper did not report"),
    if (length(implied) > 0L) {
      tagList(
        p("For these numbers to fit together, the missing ones would have to lie in these",
          "ranges. That is a constraint, not a claim about their true value."),
        tags$ul(lapply(names(implied), function(q) {
          tags$li(tags$b(quantity_label(q)), ": ", rk_format_implied(implied[[q]]))
        }))
      )
    },
    if (length(derived) > 0L) {
      tagList(
        p("These were not reported but follow exactly from another number, so the check",
          "used them:"),
        tags$ul(lapply(names(derived), function(q) {
          d <- derived[[q]]
          value <- if (d$lower == d$upper) d$lower else paste(d$lower, "to", d$upper)
          tags$li(tags$b(quantity_label(q)), " = ", value, " (from ", quantity_label(d$from), ")")
        }))
      )
    }
  )
}
