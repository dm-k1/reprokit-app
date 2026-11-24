# Tab: "I can't share my data". For authors who cannot share raw data.
# Steps: (1) data, (2) analysis, (3) reason for not sharing -> verdict,
# recomputed result, declared choices, appendix, what readers lose.
# The stated reason is required before anything is produced (plan section 1:
# the tool must not become a respectable excuse for not sharing).
# Every number shown comes from reprokit; the app only builds the formula from
# the columns the user picks.
#
# `datasets` are the practice datasets (read_practice()$share). `incoming` is a
# reactive that, when it fires, holds one of them to load: list(item = ).

mod_share_ui <- function(id, datasets) {
  ns <- NS(id)
  page_body(
    page_intro("I can't share my data",
               paste("Find out whether summary statistics let readers reproduce your result,",
                     "and get the appendix that reports them.")),
    layout_columns(
      col_widths = c(6, 6),
      div(
        class = "stack",
        card(
          step_header(1, "Your data", "Nothing leaves your computer"),
          radioButtons(ns("source"), NULL, choices = c(
            "Use a practice dataset" = "demo",
            "Open a CSV file from my computer" = "csv",
            "A fitted model saved from R with saveRDS() (advanced)" = "rds"
          )),
          conditionalPanel(
            "input.source == 'demo'", ns = ns,
            selectInput(ns("demo"), "Practice dataset", choices = dataset_choices(datasets),
                        width = "100%"),
            uiOutput(ns("dataset_story"))
          ),
          conditionalPanel(
            "input.source == 'csv'", ns = ns,
            fileInput(ns("csv"), "CSV file", accept = c(".csv", ".txt")),
            layout_columns(
              radioButtons(
                ns("sep"),
                label_with_help("Columns separated by",
                                paste("Excel in many European countries saves CSV files with",
                                      "semicolons.")),
                c("comma (,)" = ",", "semicolon (;)" = ";", "tab" = "\t")
              ),
              radioButtons(ns("dec"), "Decimal mark", c("point (1.5)" = ".", "comma (1,5)" = ","))
            ),
            explain(p("One row per participant, one column per variable. For a paired test, put",
                      "the two measurements in two columns (\"wide\" format)."))
          ),
          conditionalPanel(
            "input.source == 'rds'", ns = ns,
            fileInput(
              ns("model"),
              label_with_help("Fitted model (.rds)",
                              "In R: model <- lm(y ~ x, data = d); saveRDS(model, \"model.rds\")."),
              accept = ".rds"
            ),
            explain(p("reprokit reads the analysis from the model. At the moment it understands",
                      "linear models from lm(). Other models still get a verdict, with the",
                      "reason."))
          ),
          uiOutput(ns("data_preview"))
        ),
        conditionalPanel(
          "input.source != 'rds'", ns = ns,
          card(
            step_header(2, "Your analysis", "Pick the test you ran, then the columns it used"),
            selectInput(ns("handler"), "What did you test?", choices = test_choices(),
                        width = "100%"),
            uiOutput(ns("analysis_help")),
            uiOutput(ns("columns")),
            uiOutput(ns("options"))
          )
        ),
        card(
          step_header(3, "Why can't the raw data be shared?", "Required"),
          textAreaInput(ns("sharing_reason"), NULL, width = "100%", rows = 3,
                        placeholder = paste("e.g. Participants consented to the use of their",
                                            "data in this study only.")),
          accordion(open = FALSE, accordion_panel("What counts as a good reason?",
                                                  sharing_reason_help())),
          textAreaInput(
            ns("lost_extra"),
            label_with_help("Anything else readers lose? (optional, one per line)",
                            paste("The app always lists what readers lose for this test.",
                                  "Add anything specific to your study.")),
            width = "100%", rows = 2
          ),
          numericInput(
            ns("threshold"),
            label_with_help("Smallest group size that is safe to summarise",
                            paste("Summaries of very small groups are almost the raw data, so",
                                  "they protect no one. Default 5; the total must also be at",
                                  "least twice this.")),
            value = 5, min = 2, step = 1, width = "60%"
          ),
          actionButton(ns("go"), "Consult the oracle", class = "btn-primary btn-lg w-100")
        )
      ),
      uiOutput(ns("results"))
    )
  )
}

# Practice datasets for the drop-down: title, with the field in brackets.
dataset_choices <- function(datasets) {
  stats::setNames(names(datasets), vapply(datasets, function(d) {
    paste0(d$title, " (", d$field, ")")
  }, character(1)))
}

mod_share_server <- function(id, datasets, incoming = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # TRUE once the results shown belong to the current inputs.
    fresh <- reactiveVal(FALSE)

    # A practice dataset sent from the Practice examples tab: select it, its
    # analysis and the authors' own reason. The columns follow from `preset`.
    observeEvent(incoming(), {
      item <- incoming()$item
      fresh(FALSE)
      updateRadioButtons(session, "source", selected = "demo")
      updateSelectInput(session, "demo", selected = item$id)
      updateSelectInput(session, "handler", selected = item$analysis$handler)
      updateTextAreaInput(session, "sharing_reason", value = trimws(item$sharing_reason))
    })

    # Step 1: the data -------------------------------------------------------
    practice_item <- reactive({
      if (identical(input$source, "demo")) datasets[[req(input$demo)]]
    })
    dataset <- reactive({
      switch(input$source %||% "",
             demo = practice_item()$data,
             csv = {
               req(input$csv)
               tryCatch(utils::read.csv(input$csv$datapath, sep = input$sep, dec = input$dec,
                                        check.names = FALSE, stringsAsFactors = TRUE),
                        error = function(e) NULL)
             },
             NULL)
    })
    # The practice dataset's own analysis, while the chosen test is that analysis.
    preset <- reactive({
      analysis <- practice_item()$analysis
      if (identical(analysis$handler, input$handler)) analysis
    })

    output$dataset_story <- renderUI({
      item <- practice_item()
      req(item)
      accordion(open = FALSE, accordion_panel(
        "About this dataset",
        p(item$story),
        p(class = "mb-0 text-muted",
          "More on the", tags$i("Practice examples"), "tab, with the answer to try afterwards.")
      ))
    })

    output$data_preview <- renderUI({
      if (identical(input$source, "rds")) return(NULL)
      d <- dataset()
      if (is.null(d)) {
        if (identical(input$source, "csv") && !is.null(input$csv)) {
          return(error_box("The file could not be read as a table.",
                           "Check the separator and decimal mark above."))
        }
        return(NULL)
      }
      tagList(
        p(class = "mt-3", tags$b(nrow(d), "rows and", ncol(d), "columns."), "The first rows:"),
        div(class = "table-responsive", tableOutput(ns("head")))
      )
    })
    output$head <- renderTable(utils::head(dataset(), 5L))

    # Step 2: the analysis ---------------------------------------------------
    entry <- reactive(test_catalogue()[[req(input$handler)]])

    output$analysis_help <- renderUI({
      e <- entry()
      row <- rk_coverage_row(input$handler)
      tagList(
        explain(title = e$technical, p(tags$i("Example: "), e$example)),
        if (!is.null(row)) {
          explain(
            title = "What this test needs",
            tags$ul(
              tags$li(tags$b("APA style asks you to report: "), row$apa),
              tags$li(tags$b("Readers need, to reproduce it exactly: "), row$sufficient),
              tags$li(tags$b("Missing from the APA list: "),
                      if (row$missing == "\u2014") "nothing, the APA list is enough" else
                        row$missing),
              tags$li(tags$b("Choices you must declare: "), row$choices)
            ),
            icon = "\U0001F4CB"
          )
        }
      )
    })

    output$columns <- renderUI({
      d <- dataset()
      if (is.null(d)) return(p(class = "text-muted", "Choose your data in step 1 first."))
      roles <- entry()$share$columns
      numeric_cols <- names(d)[vapply(d, is.numeric, logical(1))]
      tagList(
        lapply(names(roles), function(role) {
          choices <- if (role == "group") names(d) else numeric_cols
          selected <- preset()$columns[[role]] %||% default_column(role, choices, d)
          selectInput(ns(paste0("col_", role)), roles[[role]], choices = choices,
                      selected = selected)
        }),
        explain(p("Rows with a missing value in any of these columns are left out, as in",
                  "most analyses (complete cases)."), icon = "\u2139\uFE0F")
      )
    })

    output$options <- renderUI({
      e <- entry()
      tagList(
        if (length(e$share$tests) > 1L) {
          radioButtons(
            ns("test"),
            label_with_help("Which version of the test?",
                            paste("Welch's t is the default in R's t.test(). If you are not",
                                  "sure, check your output: Welch's df usually has decimals",
                                  "(17.78).")),
            choices = e$share$tests, selected = preset()$test
          )
        },
        if (isTRUE(e$has_mu)) {
          numericInput(
            ns("mu"),
            label_with_help("Test value",
                            paste("The value the mean (or the mean difference) was compared",
                                  "with. Usually 0.")),
            value = 0
          )
        },
        if (input$handler != "anova_oneway") {
          layout_columns(
            radioButtons(ns("alternative"), "Tails",
                         c("two-sided" = "two.sided", "one-sided: less" = "less",
                           "one-sided: greater" = "greater")),
            selectInput(ns("conf_level"), "Confidence level",
                        c("95%" = 0.95, "90%" = 0.90, "99%" = 0.99))
          )
        }
      )
    })

    # The formula and arguments for reprokit, from the picked columns.
    analysis <- reactive({
      d <- dataset()
      e <- entry()
      cols <- vapply(names(e$share$columns), function(role) {
        input[[paste0("col_", role)]] %||% NA_character_
      }, character(1))
      validate(need(!is.null(d) && !anyNA(cols), "Choose your data and columns first."))
      problem <- column_problem(input$handler, d, cols)
      validate(need(is.null(problem), problem))
      # Shiny keeps the last value of an input that is no longer shown, so the
      # test version counts only if it belongs to this test.
      test <- if (isTRUE(input$test %in% e$share$tests)) input$test else e$share$tests[[1L]]
      args <- list(data = d, test = test)
      if (isTRUE(e$has_mu)) args$mu <- input$mu %||% 0
      if (input$handler != "anova_oneway") {
        args$alternative <- input$alternative
        args$conf_level <- as.numeric(input$conf_level)
      }
      list(x = share_formula(input$handler, cols), args = args)
    })

    # Result ------------------------------------------------------------------
    observeEvent(input$go, fresh(TRUE), priority = 1)

    result <- eventReactive(input$go, {
      validate(need(nzchar(trimws(input$sharing_reason)),
                    "Step 3: state why the raw data cannot be shared."))
      lost <- trimws(strsplit(input$lost_extra, "\n", fixed = TRUE)[[1L]])
      threshold <- if (isTRUE(input$threshold >= 2)) input$threshold else 5
      if (identical(input$source, "rds")) {
        validate(need(input$model, "Step 1: open a fitted model (.rds)."))
        run_share(readRDS(input$model$datapath), list(), input$sharing_reason,
                  lost[nzchar(lost)], threshold)
      } else {
        a <- analysis()
        run_share(a$x, a$args, input$sharing_reason, lost[nzchar(lost)], threshold)
      }
    })

    # The appendix waits until the effect-size formula is declared (step 4),
    # for every test that has one: the reader needs it to recompute the effect.
    convention_missing <- reactive({
      r <- result()
      length(rk_handler_info(r$stats$handler)$conventions) > 0L &&
        !nzchar(input$convention %||% "")
    })

    appendix_lines <- reactive({
      r <- result()
      req(r$stats, !convention_missing())
      choices <- list(convention = input$convention %||% "")
      choices <- Filter(nzchar, choices)
      tryCatch(reprokit::appendix(r$stats, choices = choices),
               error = function(e) structure(conditionMessage(e), class = "appendix_error"))
    })

    output$results <- renderUI({
      if (!fresh()) {
        return(results_placeholder(
          "Your results will appear here",
          tagList("Work through steps 1 to 3, then press", tags$b("Consult the oracle.")),
          c("A verdict: do summary statistics suffice, or do readers need more?",
            "Your result, recomputed from the summaries alone.",
            "An appendix for your paper, ready to paste (Markdown).",
            "A list of what readers lose compared with the raw data.")
        ))
      }
      r <- result()
      v <- verdict_info()[[r$triage$verdict]]
      tagList(
        verdict_banner(v$icon, v$title, v$meaning, v$class,
                       detail = tagList(tags$b("Why: "), r$triage$reason)),
        explain(p(standard_reminder()), icon = "\u2696\uFE0F"),
        if (!is.null(r$stats)) share_success_ui(ns, r) else share_refused_ui(r),
        lost_card(r$stats$lost %||% r$triage$lost, r$stats)
      )
    })

    output$reproduced <- renderTable({
      r <- result()
      req(r$reproduced)
      data.frame(Number = reproduced_labels(names(r$reproduced)),
                 Value = show_number(unname(r$reproduced), 6L), check.names = FALSE)
    })

    output$appendix_preview <- renderUI({
      if (convention_missing()) {
        return(explain(title = "One step left", icon = "\u261D\uFE0F",
                       p("Choose how you computed the effect size in step 4 above. Your",
                         "appendix appears here as soon as you have.")))
      }
      a <- appendix_lines()
      if (inherits(a, "appendix_error")) return(error_box(unclass(a)))
      tagList(
        div(class = "appendix-preview", markdown_html(a)),
        downloadButton(ns("download"), "Download the appendix (.md)", class = "btn-primary")
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

# Pieces of the result panel -------------------------------------------------------

share_success_ui <- function(ns, r) {
  conventions <- rk_handler_info(r$stats$handler)$conventions
  tagList(
    card(
      card_header("Your result, recomputed from the summaries alone"),
      explain(p("These numbers were computed from the stored summary statistics only, not from",
                "the raw data. Compare them with your own output: if they match, a reader with",
                "the appendix gets exactly your result.")),
      tableOutput(ns("reproduced"))
    ),
    card(
      step_header(4, "Declare your choices", "So readers know which formula you used"),
      if (length(conventions) > 0L) {
        tagList(
          selectInput(
            ns("convention"),
            label_with_help("How did you compute the effect size?",
                            paste("There are several formulas for d (and for eta squared). They",
                                  "give different numbers from the same data, so readers need",
                                  "to know which one you used. Declaring it is this project's",
                                  "rule; APA does not require it.")),
            choices = c("Not declared yet" = "",
                        stats::setNames(conventions, rk_label(conventions)))
          ),
          explain(p("Tails, confidence level and the test version are taken from step 2."),
                  icon = "\u2139\uFE0F")
        )
      } else {
        p("Tails, confidence level and the test version are taken from step 2.")
      }
    ),
    card(
      card_header("Your appendix"),
      explain(
        title = "What the four blocks are",
        tags$ul(
          tags$li(tags$b("A."), "The numbers APA style asks for, with their values."),
          tags$li(tags$b("B."), "Numbers APA does not ask for but the result needs, with the",
                  "reason. Empty for most simple tests; for a paired t-test it is the",
                  "correlation between the two measurements."),
          tags$li(tags$b("C."), "Your analysis choices (tails, level, test, effect-size formula)."),
          tags$li(tags$b("D."), "APA numbers a reader cannot check from these statistics, and why.")
        ),
        p("It ends with your reason for not sharing, what readers lose, and the disclosure check.",
          "Paste it into your supplementary materials.")
      ),
      uiOutput(ns("appendix_preview"))
    )
  )
}

share_refused_ui <- function(r) {
  card(
    card_header("What you can do instead"),
    p("Summary statistics cannot stand in for the data here. Options, from best to weakest:"),
    tags$ol(lapply(r$alternatives, tags$li)),
    # A refusal repeats the verdict's reason; any other error is news.
    if (!is.null(r$error)) explain(p(r$error), title = "What went wrong")
  )
}

lost_card <- function(lost, stats) {
  card(
    card_header("What readers lose compared with the raw data"),
    p("Whatever you report, readers without the raw data cannot do the following.",
      if (!is.null(stats)) "This list is always printed in the appendix and cannot be removed."),
    tags$ul(lapply(c(lost, "diagnostics, reanalysis with other methods, and fraud detection"),
                   tags$li)),
    if (!is.null(stats)) {
      explain(
        title = "Disclosure check",
        p(paste0("No summary rests on fewer than ", stats$disclosure$threshold, " cases",
                 if (length(stats$disclosure$checked) > 0L) {
                   paste0(" (checked: ", stats$disclosure$checked, ")")
                 },
                 ". This lowers the risk that the summaries identify a participant. It does not ",
                 "remove it, and it is not a guarantee of anonymity or of legal compliance.")),
        icon = "\U0001F6E1\uFE0F"
      )
    }
  )
}

# Helpers -------------------------------------------------------------------------

# Calls reprokit in order: triage, then (only if summaries suffice) the
# sufficient statistics and the recomputed result. Never throws.
run_share <- function(x, args, sharing_reason, lost, threshold) {
  tr <- do.call(reprokit::triage, c(list(x), args, list(threshold = threshold)))
  out <- list(triage = tr, stats = NULL, reproduced = NULL, error = NULL,
              alternatives = NULL)
  s <- tryCatch(
    do.call(reprokit::sufficient_stats,
            c(list(x), args, list(sharing_reason = sharing_reason,
                                  lost = if (length(lost)) lost, threshold = threshold))),
    error = function(e) e
  )
  if (inherits(s, "reprokit_sufficient_stats")) {
    out$stats <- s
    out$reproduced <- reprokit::reproduce(s)
    return(out)
  }
  if (!inherits(s, "reprokit_impossible")) out$error <- conditionMessage(s)
  out$alternatives <- s$alternatives %||% c(
    "share the raw data (possibly under a data-use agreement)",
    "share synthetic data and report its utility",
    "offer federated analysis (e.g. DataSHIELD)"
  )
  out
}

# reprokit's formula for each test, with backticks so any column name works.
share_formula <- function(handler, cols) {
  q <- function(x) paste0("`", x, "`")
  text <- switch(handler,
                 t_independent = ,
                 anova_oneway = paste(q(cols[["outcome"]]), "~", q(cols[["group"]])),
                 t_one_sample = paste(q(cols[["outcome"]]), "~ 1"),
                 t_paired = paste0("Pair(", q(cols[["first"]]), ", ", q(cols[["second"]]), ") ~ 1"),
                 cor_pearson = paste("~", q(cols[["x"]]), "+", q(cols[["y"]])))
  stats::as.formula(text, env = globalenv())
}

# Input problems the user can fix, in plain words (NULL when fine). These are
# checks on the shape of the input, not statistics.
column_problem <- function(handler, d, cols) {
  if (anyDuplicated(cols)) return("Pick a different column for each role.")
  if ("group" %in% names(cols)) {
    groups <- length(unique(stats::na.omit(d[[cols[["group"]]]])))
    if (handler == "t_independent" && groups != 2L) {
      return(paste0("The group column has ", groups, " groups; this test needs exactly 2. ",
                    "For 3 or more groups, choose \"Compare three or more separate groups\"."))
    }
    if (handler == "anova_oneway" && groups < 2L) {
      return("The group column needs at least 2 groups.")
    }
  }
  NULL
}

# A first guess for each column role when the dataset does not say: the first
# factor for a group, otherwise the first (or second) numeric column.
default_column <- function(role, choices, d) {
  if (role == "group") {
    factors <- names(d)[vapply(d, function(x) is.factor(x) || is.character(x), logical(1))]
    if (length(factors) > 0L) return(factors[1L])
  }
  if (role %in% c("second", "y") && length(choices) > 1L) return(choices[2L])
  choices[1L]
}

reproduced_labels <- function(x) {
  extra <- c(mean_diff = "Mean difference", mean = "Mean", r.squared = "R squared",
             adj.r.squared = "Adjusted R squared", sigma = "Residual SD", F = "F value",
             numdf = "df, first number", dendf = "df, second number")
  vapply(x, function(name) {
    if (name %in% names(extra)) return(extra[[name]])
    if (grepl(":", name, fixed = TRUE)) return(sub(":", " of ", name, fixed = TRUE))
    quantity_label(name)
  }, character(1), USE.NAMES = FALSE)
}
