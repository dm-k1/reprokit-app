# Tab: Publish data. For authors who cannot share raw data.
# Layout: three steps in the sidebar (data, analysis, reason), three value
# boxes on top, and tabs below; the result tabs appear once there is a
# verdict. The reason is required before anything is produced (plan section
# 1: the tool must not become a respectable excuse for not sharing). Every
# number shown comes from reprokit; the app only builds the formula from the
# columns the user picks.
#
# `datasets` are the practice datasets (read_practice()$share). `incoming` is
# a reactive that, when it fires, holds one of them to load: list(item = ).

share_result_tabs <- c("verdict", "appendix", "lost")

mod_share_ui <- function(id, datasets) {
  ns <- NS(id)
  words <- txt("share")
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
        open = "data",
        accordion_panel(
          step_title(1, words$steps$data),
          value = "data",
          radioButtons(ns("source"), words$source_label,
                       choices = invert(words$sources)),
          conditionalPanel(
            "input.source == 'demo'", ns = ns,
            selectInput(ns("demo"), words$demo_label,
                        choices = dataset_choices(datasets), width = "100%")
          ),
          conditionalPanel(
            "input.source == 'csv'", ns = ns,
            fileInput(ns("csv"), words$csv_label, accept = c(".csv", ".txt")),
            layout_columns(
              radioButtons(
                ns("sep"),
                label_with_help(words$sep_label, words$sep_help),
                c("comma (,)" = ",", "semicolon (;)" = ";", "tab" = "\t")
              ),
              radioButtons(ns("dec"), words$dec_label,
                           c("point (1.5)" = ".", "comma (1,5)" = ","))
            )
          ),
          conditionalPanel(
            "input.source == 'rds'", ns = ns,
            fileInput(
              ns("model"),
              label_with_help(words$rds_label, md(words$rds_help)),
              accept = ".rds"
            )
          ),
          next_button(ns("to_analysis"), words$next_step)
        ),
        accordion_panel(
          step_title(2, words$steps$analysis),
          value = "analysis",
          conditionalPanel(
            "input.source != 'rds'", ns = ns,
            selectInput(ns("test"), words$test_label,
                        choices = test_choices("share"), width = "100%"),
            uiOutput(ns("columns")),
            uiOutput(ns("options"))
          ),
          conditionalPanel(
            "input.source == 'rds'", ns = ns,
            p(class = "text-muted", words$rds_no_analysis)
          ),
          next_button(ns("to_reason"), words$next_step)
        ),
        accordion_panel(
          step_title(3, words$steps$reason),
          value = "reason",
          textAreaInput(ns("sharing_reason"), NULL, width = "100%", rows = 4,
                        placeholder = words$reason_placeholder),
          # A plain fold-out, so opening it never closes step 3.
          tags$details(
            class = "fold",
            tags$summary(words$reason_help_title),
            md(words$reason_help)
          ),
          numericInput(
            ns("threshold"),
            label_with_help(words$threshold_label, words$threshold_help),
            value = 5, min = 2, step = 1
          )
        )
      ),
      div(
        class = "sidebar-actions",
        actionButton(ns("go"), words$go, icon = icon("play"),
                     class = "btn-primary btn-lg w-100")
      )
    ),
    uiOutput(ns("boxes"), class = "box-row"),
    navset_card_underline(
      id = ns("tabs"),
      nav_panel(words$tabs$data, value = "data", uiOutput(ns("data"))),
      nav_panel(words$tabs$verdict, value = "verdict", uiOutput(ns("verdict"))),
      nav_panel(words$tabs$appendix, value = "appendix",
                uiOutput(ns("appendix"))),
      nav_panel(words$tabs$lost, value = "lost", uiOutput(ns("lost")))
    )
  )
}

next_button <- function(id, label) {
  actionButton(id, label, icon = icon("arrow-down"),
               class = "btn-light w-100 mt-2")
}

# Practice datasets for the drop-down: title, with the field in brackets.
dataset_choices <- function(datasets) {
  labels <- vapply(datasets, \(d) paste0(d$title, " (", d$field, ")"),
                   character(1))
  stats::setNames(names(datasets), labels)
}

mod_share_server <- function(id, datasets, incoming = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    words <- txt("share")
    result <- reactiveVal(NULL)
    fresh <- reactiveVal(FALSE)

    observeEvent(input$to_analysis, accordion_panel_open("steps", "analysis"))
    observeEvent(input$to_reason, accordion_panel_open("steps", "reason"))

    # A practice dataset sent from the Practice examples tab: select it, its
    # analysis and the authors' own reason. The columns follow from `preset`.
    observeEvent(incoming(), {
      item <- incoming()$item
      fresh(FALSE)
      updateRadioButtons(session, "source", selected = "demo")
      updateSelectInput(session, "demo", selected = item$id)
      updateSelectInput(session, "test", selected = item$analysis$test)
      updateTextAreaInput(session, "sharing_reason",
                          value = trimws(item$sharing_reason))
      nav_select("tabs", "data")
    })

    observe({
      for (tab in share_result_tabs) {
        if (fresh()) nav_show("tabs", tab) else nav_hide("tabs", tab)
      }
    })
    observeEvent(list(input$source, input$demo, input$csv, input$model,
                      input$test), fresh(FALSE), ignoreInit = TRUE)
    # Picking a practice dataset also picks the analysis its authors ran.
    observeEvent(input$demo, {
      test <- datasets[[input$demo]]$analysis$test
      if (!is.null(test)) updateSelectInput(session, "test", selected = test)
    })

    # Step 1: the data -------------------------------------------------------
    practice_item <- reactive({
      if (identical(input$source, "demo")) datasets[[req(input$demo)]]
    })
    dataset <- reactive({
      switch(
        input$source %||% "",
        demo = practice_item()$data,
        csv = {
          req(input$csv)
          tryCatch(
            utils::read.csv(input$csv$datapath, sep = input$sep,
                            dec = input$dec, check.names = FALSE,
                            stringsAsFactors = TRUE),
            error = \(e) NULL
          )
        },
        NULL
      )
    })
    # The practice dataset's own analysis, while the chosen test is that one.
    preset <- reactive({
      analysis <- practice_item()$analysis
      if (identical(analysis$test, input$test)) analysis
    })

    output$data <- renderUI({
      if (identical(input$source, "rds")) {
        return(explain(md(words$rds_explain), icon_name = "circle-info"))
      }
      d <- dataset()
      if (is.null(d)) {
        if (identical(input$source, "csv") && !is.null(input$csv)) {
          return(error_box(words$read_error, words$read_hint))
        }
        return(div(
          class = "waiting-panel",
          icon("table", class = "waiting-icon"),
          tags$h5(words$waiting_title),
          p(words$data_empty)
        ))
      }
      layout_columns(
        col_widths = c(7, 5),
        div(
          p(tags$b(fill_in(words$data_rows, rows = nrow(d), cols = ncol(d)))),
          div(class = "table-responsive", tableOutput(ns("head")))
        ),
        dataset_about(practice_item(), words)
      )
    })
    output$head <- renderTable(utils::head(dataset(), 8L))

    # Step 2: the analysis ---------------------------------------------------
    entry <- reactive(test_entry(req(input$test)))

    output$columns <- renderUI({
      d <- dataset()
      if (is.null(d)) return(p(class = "text-muted", words$need_columns))
      roles <- entry()$share$columns
      selects <- list()
      taken <- character()
      for (role in names(roles)) {
        type <- roles[[role]]$type
        choices <- column_choices(d, type)
        selected <- preset()$columns[[role]] %||%
          default_column(d, type, taken)
        taken <- c(taken, selected)
        selects[[role]] <- selectInput(
          ns(paste0("col_", role)), roles[[role]]$label,
          choices = choices, selected = selected, width = "100%"
        )
      }
      tagList(selects, p(class = "small text-muted", words$columns_note))
    })

    output$options <- renderUI({
      share <- entry()$share
      options <- unlist(share$options)
      tagList(
        if (length(share$versions) > 1L) {
          radioButtons(
            ns("version"),
            label_with_help(words$version_label, words$version_help),
            choices = invert(share$versions),
            selected = preset()$version
          )
        },
        if ("mu" %in% options) {
          numericInput(ns("mu"),
                       label_with_help(words$mu_label, words$mu_help),
                       value = 0)
        },
        if (any(c("tails", "level") %in% options)) {
          layout_columns(
            radioButtons(ns("alternative"), words$tails_label,
                         invert(txt("check", "tail_choices")[-1L])),
            selectInput(ns("conf_level"), words$level_label,
                        c("95%" = 0.95, "90%" = 0.90, "99%" = 0.99))
          )
        },
        if ("correct" %in% options) {
          radioButtons(ns("correct"), words$correct_label,
                       invert(words$correct_choices))
        },
        if ("p0" %in% options) {
          textInput(ns("p0"), label_with_help(words$p0_label, words$p0_help))
        }
      )
    })

    # The formula and arguments for reprokit, from the picked columns.
    analysis <- reactive({
      d <- dataset()
      share <- entry()$share
      cols <- vapply(names(share$columns), function(role) {
        input[[paste0("col_", role)]] %||% NA_character_
      }, character(1))
      validate(need(!is.null(d) && !anyNA(cols), words$need_columns))
      problem <- column_problem(input$test, d, cols)
      validate(need(is.null(problem), problem))
      versions <- names(share$versions)
      # Shiny keeps the last value of an input that is no longer shown, so
      # the version counts only if it belongs to this test.
      version <- if (isTRUE(input$version %in% versions)) {
        input$version
      } else {
        versions[[1L]]
      }
      formula_cols <- cols[setdiff(names(cols), unlist(share$column_args))]
      args <- list(data = d, test = version)
      for (arg in unlist(share$column_args)) args[[arg]] <- cols[[arg]]
      options <- unlist(share$options)
      if ("mu" %in% options) args$mu <- input$mu %||% 0
      if ("tails" %in% options) args$alternative <- input$alternative
      if ("level" %in% options) {
        args$conf_level <- as.numeric(input$conf_level)
      }
      if ("correct" %in% options) {
        args$correct <- identical(input$correct, "TRUE")
      }
      p0 <- trimws(strsplit(input$p0 %||% "", ",", fixed = TRUE)[[1L]])
      if ("p0" %in% options && length(p0) > 0L) {
        args$p0 <- suppressWarnings(as.numeric(p0))
      }
      list(x = share_formula(input$test, formula_cols), args = args)
    })

    # The analysis step may be folded away; its inputs must exist anyway.
    for (name in c("columns", "options")) {
      outputOptions(output, name, suspendWhenHidden = FALSE)
    }

    # Result ------------------------------------------------------------------
    observeEvent(input$go, {
      if (!nzchar(trimws(input$sharing_reason %||% ""))) {
        showNotification(words$need_reason, type = "error")
        accordion_panel_open("steps", "reason")
        return()
      }
      threshold <- if (isTRUE(input$threshold >= 2)) input$threshold else 5
      if (identical(input$source, "rds")) {
        if (is.null(input$model)) {
          showNotification(words$need_model, type = "error")
          return()
        }
        x <- tryCatch(readRDS(input$model$datapath), error = identity)
        stats <- if (inherits(x, "error")) x else share_verdict(x)
      } else {
        a <- tryCatch(analysis(), error = identity)
        if (inherits(a, "error")) {
          showNotification(conditionMessage(a), type = "error")
          accordion_panel_open("steps", "analysis")
          return()
        }
        stats <- share_verdict(a$x, a$args, threshold)
      }
      result(stats)
      fresh(TRUE)
      nav_select("tabs", "verdict")
    })

    shown <- reactive({
      req(fresh())
      result()
    })

    output$boxes <- renderUI({
      if (!fresh()) return(share_boxes_waiting(words))
      share_boxes(result(), words)
    })

    output$verdict <- renderUI({
      s <- shown()
      if (inherits(s, "error")) {
        return(error_box(conditionMessage(s)))
      }
      share_verdict_ui(s, words)
    })

    output$lost <- renderUI({
      s <- shown()
      req(!inherits(s, "error"))
      share_lost_ui(s, words)
    })

    # The appendix waits until the effect-size formula is declared, for
    # every test that has more than one: readers need it.
    choice_specs <- reactive({
      s <- shown()
      req(!inherits(s, "error"), s$verdict == "summaries_suffice")
      appendix_choice_specs(s$handler)
    })
    convention_missing <- reactive({
      isTRUE(choice_specs()$convention$required) &&
        !nzchar(input$convention %||% "")
    })
    appendix_lines <- reactive({
      req(!convention_missing())
      share_appendix(
        shown(),
        sharing_reason = input$sharing_reason,
        choices = list(convention = input$convention,
                       ss_type = input$ss_type,
                       level = input$ci_level)
      )
    })

    output$appendix <- renderUI({
      s <- shown()
      req(!inherits(s, "error"))
      if (s$verdict != "summaries_suffice") {
        return(explain(p(words$appendix_refused), icon_name = "lock"))
      }
      layout_columns(
        col_widths = c(4, 8),
        div(
          tags$h5(words$choices_title),
          p(class = "small", words$choices_text),
          appendix_choice_inputs(ns, s$handler, choice_specs(), words),
          explain(title = words$appendix_blocks_title,
                  md(words$appendix_blocks), icon_name = "list-ol")
        ),
        uiOutput(ns("appendix_preview"))
      )
    })

    output$appendix_preview <- renderUI({
      if (convention_missing()) {
        return(explain(p(words$appendix_wait), icon_name = "hand-point-left"))
      }
      a <- appendix_lines()
      if (inherits(a, "error")) return(error_box(conditionMessage(a)))
      tagList(
        downloadButton(ns("download"), words$download,
                       class = "btn-primary mb-3"),
        div(class = "appendix-preview", md(a))
      )
    })

    output$download <- downloadHandler(
      filename = function() "appendix-sufficient-statistics.md",
      content = function(file) {
        writeLines(enc2utf8(unclass(appendix_lines())), file, useBytes = TRUE)
      }
    )

    invisible(result)
  })
}

# Pieces of the results --------------------------------------------------------

dataset_about <- function(item, words) {
  if (is.null(item)) return(explain(p(words$csv_help)))
  explain(
    title = item$title,
    p(item$story),
    tags$h6(words$codebook),
    tags$dl(
      class = "codebook",
      lapply(names(item$codebook), function(column) {
        tagList(tags$dt(tags$code(column)), tags$dd(item$codebook[[column]]))
      })
    ),
    icon_name = "table"
  )
}

share_boxes_waiting <- function(words) {
  boxes <- words$boxes
  box_strip(
    result_box(boxes$verdict, boxes$waiting, boxes$waiting_hint,
               "scale-balanced", "box-waiting"),
    result_box(boxes$publish, "—", NULL, "file-lines", "box-waiting"),
    result_box(boxes$disclosure, "—", NULL, "shield-halved",
               "box-waiting")
  )
}

share_boxes <- function(s, words) {
  boxes <- words$boxes
  if (inherits(s, "error")) return(share_boxes_waiting(words))
  verdict <- txt("verdicts", s$verdict)
  disclosure <- s$disclosure
  passed <- isTRUE(disclosure$passed)
  box_strip(
    result_box(boxes$verdict, verdict$short, NULL, verdict$icon,
               paste0("box-", verdict$class)),
    result_box(boxes$publish, s$publish %||% "—", NULL, "file-lines"),
    result_box(
      boxes$disclosure,
      if (passed) words$disclosure_passed else words$disclosure_flagged,
      paste(unique(c(disclosure$checked, disclosure$problems)),
            collapse = "; "),
      if (passed) "shield-halved" else "triangle-exclamation",
      if (passed) NULL else "box-warning"
    )
  )
}

share_verdict_ui <- function(s, words) {
  verdict <- txt("verdicts", s$verdict)
  suffice <- s$verdict == "summaries_suffice"
  layout_columns(
    col_widths = c(6, 6),
    div(
      verdict_banner(
        verdict$icon, verdict$title, verdict$meaning, verdict$class,
        detail = tagList(tags$b(paste0(words$verdict_why, ": ")),
                         s$verdict_reason)
      ),
      if (suffice) {
        explain(md(words$suffice_next), icon_name = "arrow-right")
      } else {
        explain(
          title = words$instead_title,
          tags$ol(lapply(unlist(words$instead), tags$li)),
          icon_name = "signs-post"
        )
      }
    ),
    div(
      explain(
        title = words$boxes$disclosure,
        p(fill_in(words$disclosure_text,
                  threshold = s$disclosure$threshold)),
        if (length(s$disclosure$checked) > 0L) {
          p(fill_in(words$disclosure_checked,
                    checked = s$disclosure$checked))
        },
        if (length(s$disclosure$problems) > 0L) {
          tags$ul(lapply(unique(s$disclosure$problems), tags$li))
        },
        icon_name = "shield-halved"
      ),
      explain(p(txt("reminders", "standard")), icon_name = "scale-balanced")
    )
  )
}

share_lost_ui <- function(s, words) {
  layout_columns(
    col_widths = c(6, 6),
    div(
      tags$h5(words$lost_title),
      tags$ul(lapply(c(unlist(s$lost), words$lost_always), tags$li))
    ),
    explain(
      title = words$instead_title,
      tags$ol(lapply(unlist(words$instead), tags$li)),
      icon_name = "signs-post"
    )
  )
}

appendix_choice_inputs <- function(ns, handler, specs, words) {
  if (length(specs) == 0L) {
    return(p(class = "text-muted", words$choices_none))
  }
  tagList(
    if (!is.null(specs$convention)) {
      selectInput(
        ns("convention"), words$convention_label,
        choices = c(stats::setNames("", words$not_declared),
                    convention_choices(handler)),
        width = "100%"
      )
    },
    if (!is.null(specs$ss_type)) {
      selectInput(
        ns("ss_type"),
        label_with_help(words$ss_type_label, words$ss_type_help),
        choices = c(stats::setNames("", words$not_declared),
                    convention_choices(handler, "ss_type")),
        width = "100%"
      )
    },
    if (!is.null(specs$level)) {
      selectInput(
        ns("ci_level"), words$ci_level_label,
        choices = c(stats::setNames("", words$not_declared),
                    "95%" = "0.95", "90%" = "0.90", "99%" = "0.99"),
        width = "100%"
      )
    }
  )
}
