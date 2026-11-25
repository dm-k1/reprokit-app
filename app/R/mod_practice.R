# Tab: "Practice examples". Fictional psychology studies, one YAML file each
# in app/practice/ (see practice/README.md): papers to check (`check-*.yml`)
# and datasets to share (`share-*.yml`, data in practice/data/). The sidebar
# lists them; the story and the task sit side by side. The cards never say
# which kind of case they are; the answer stays folded until it is opened.
# Returns a reactive that fires when an example is sent to another tab:
# list(tab = "check" or "share", item = the example).

# All examples in `dir`, as list(check = list(id = item, ...), share = ...).
# A share example also gets its data (`data`) and file path (`path`).
read_practice <- function(dir) {
  pattern <- "^(check|share)-.*\\.ya?ml$"
  files <- sort(list.files(dir, pattern = pattern, full.names = TRUE))
  items <- lapply(files, function(file) {
    item <- read_yaml_keep_words(file)
    item$id <- tools::file_path_sans_ext(basename(file))
    if (identical(item$type, "share")) {
      item$path <- file.path(dir, "data", item$file)
      item$data <- utils::read.csv(item$path, stringsAsFactors = TRUE)
    }
    item
  })
  names(items) <- vapply(items, \(item) item$id, character(1))
  types <- vapply(items, \(item) item$type, character(1))
  list(check = items[types == "check"], share = items[types == "share"])
}

mod_practice_ui <- function(id, practice) {
  ns <- NS(id)
  words <- txt("practice")
  titles <- function(items) {
    stats::setNames(names(items), vapply(items, \(x) x$title, character(1)))
  }
  tool_layout(
    fillable = TRUE,
    border = FALSE,
    border_radius = FALSE,
    class = "tool-layout practice-layout",
    sidebar = tool_sidebar(
      width = 300,
      p(class = "small", words$intro),
      radioButtons(ns("kind"), NULL, choices = invert(words$kinds),
                   inline = TRUE),
      conditionalPanel(
        "input.kind == 'check'", ns = ns,
        radioButtons(ns("pick_check"), words$pick_label,
                     choices = titles(practice$check))
      ),
      conditionalPanel(
        "input.kind == 'share'", ns = ns,
        radioButtons(ns("pick_share"), words$pick_label,
                     choices = titles(practice$share))
      )
    ),
    uiOutput(ns("example"), fill = TRUE)
  )
}

mod_practice_server <- function(id, practice) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    words <- txt("practice")
    sent <- reactiveVal(NULL)

    current <- reactive({
      kind <- req(input$kind)
      pick <- req(input[[paste0("pick_", kind)]])
      practice[[kind]][[pick]]
    })

    output$example <- renderUI({
      item <- current()
      layout_columns(
        col_widths = c(7, 5),
        fill = TRUE,
        example_story_card(item, words),
        example_task_card(item, words, ns)
      )
    })

    observeEvent(input$send, {
      sent(list(tab = current()$type, item = current(), at = Sys.time()))
    })

    output$download <- downloadHandler(
      filename = function() current()$file,
      content = function(file) file.copy(current()$path, file)
    )

    sent
  })
}

example_story_card <- function(item, words) {
  card(
    class = "example-card",
    card_header(
      div(class = "example-field", item$field),
      tags$h4(class = "example-title", item$title)
    ),
    card_body(
      p(item$story),
      if (item$type == "check") {
        tagList(
          quote_block(words$reason, item$sharing_reason),
          tags$h6(words$as_printed),
          tags$figure(class = "as-printed", item$as_printed)
        )
      } else {
        tagList(
          quote_block(
            item$reason_label,
            item$plan %||% paste0("“", trimws(item$sharing_reason),
                                  "”")
          ),
          tags$h6(fill_in(words$rows, rows = nrow(item$data))),
          tags$dl(
            class = "codebook",
            lapply(names(item$codebook), function(column) {
              tagList(tags$dt(tags$code(column)),
                      tags$dd(item$codebook[[column]]))
            })
          )
        )
      }
    )
  )
}

example_task_card <- function(item, words, ns) {
  send <- if (item$type == "check") words$send_check else words$send_share
  card(
    class = "example-card",
    card_header(icon("pen"), words$task),
    card_body(
      p(class = "lead-task", item$question),
      actionButton(ns("send"), send, icon = icon("arrow-right"),
                   class = "btn-primary w-100 mb-2"),
      if (item$type == "share") {
        downloadButton(ns("download"), words$download,
                       class = "btn-outline-primary w-100 mb-3")
      },
      accordion(
        open = FALSE,
        accordion_panel(words$answer, p(item$answer),
                        icon = icon("key"))
      )
    )
  )
}

quote_block <- function(label, text) {
  tagList(tags$h6(label), tags$blockquote(class = "example-quote", text))
}
