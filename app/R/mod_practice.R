# Tab: "Practice examples". Fictional psychology studies, one YAML file each in
# app/practice/ (see practice/README.md): papers to check (`check-*.yml`) and
# datasets to share (`share-*.yml`, data in practice/data/). The cards never say
# which kind of case they are; the answer stays folded until a student opens it.
# Returns a reactive that fires when a card is sent to another tab:
# list(tab = "check" or "share", item = the example).

# Reading the examples -------------------------------------------------------------

# All examples in `dir`, as list(check = list(id = item, ...), share = ...).
# A share example also gets its data (`data`) and the path of its file (`path`).
read_practice <- function(dir) {
  files <- sort(list.files(dir, pattern = "^(check|share)-.*\\.ya?ml$", full.names = TRUE))
  items <- lapply(files, function(file) {
    item <- read_yaml_keep_words(file)
    item$id <- tools::file_path_sans_ext(basename(file))
    if (identical(item$type, "share")) {
      item$path <- file.path(dir, "data", item$file)
      item$data <- utils::read.csv(item$path, stringsAsFactors = TRUE)
    }
    item
  })
  names(items) <- vapply(items, `[[`, character(1), "id")
  types <- vapply(items, `[[`, character(1), "type")
  list(check = items[types == "check"], share = items[types == "share"])
}

# YAML 1.1 reads a bare `n:` (or `y:`, `yes`, `no`) as a boolean, which would turn
# the sample size key into FALSE. Only `true` and `false` stay booleans; the
# other yes/no words are kept as they were written.
read_yaml_keep_words <- function(file) {
  keep <- function(x) if (tolower(x) %in% c("true", "false")) tolower(x) == "true" else x
  yaml::read_yaml(file, handlers = list("bool#yes" = keep, "bool#no" = keep))
}

# UI ---------------------------------------------------------------------------------

mod_practice_ui <- function(id, practice) {
  ns <- NS(id)
  page_body(
    class = "practice-page",
    page_intro("Practice examples",
               paste("Fictional psychology studies to try the oracle on. Read the story, run",
                     "it yourself, decide what you think, then open the answer.")),
    navset_pill(
      id = ns("section"),
      nav_panel(
        "Does this paper add up?",
        p(class = "section-lead",
          "Three published results. Send the numbers to the checker and see how far they hold up."),
        practice_grid(lapply(practice$check, check_example_card, ns = ns))
      ),
      nav_panel(
        "I can't share my data",
        p(class = "section-lead",
          "Three datasets and what their authors plan to do with them. Load each one into the",
          "sharing tab and see what the oracle says."),
        practice_grid(lapply(practice$share, share_example_card, ns = ns))
      )
    )
  )
}

practice_grid <- function(cards) {
  do.call(layout_column_wrap, c(list(width = "20rem", class = "practice-grid"), unname(cards)))
}

# The parts every example card has: story, task and folded answer.
example_card <- function(item, body, actions) {
  card(
    class = "example-card",
    card_header(div(class = "example-header",
                    tags$span(class = "example-field", item$field),
                    tags$h4(item$title))),
    card_body(
      p(item$story),
      body,
      explain(title = "Your task", p(item$question),
              p(class = "text-muted small", "Open the answer below once you have tried it."),
              icon = "\u270F\uFE0F")
    ),
    card_footer(
      div(class = "example-actions", actions),
      accordion(open = FALSE, class = "mt-3",
                accordion_panel("Answer and discussion",
                                p(item$answer)))
    )
  )
}

quote_block <- function(label, text) {
  tagList(tags$h6(label), tags$blockquote(class = "example-quote", text))
}

check_example_card <- function(item, ns) {
  example_card(
    item,
    body = tagList(
      quote_block("Their reason for not sharing the raw data", item$sharing_reason),
      tags$h6("As printed in the paper"),
      tags$figure(class = "as-printed", item$as_printed)
    ),
    actions = actionButton(ns(paste0("send_", item$id)), "Check these numbers",
                           class = "btn-primary w-100")
  )
}

share_example_card <- function(item, ns) {
  example_card(
    item,
    body = tagList(
      quote_block(item$reason_label,
                  if (is.null(item$plan)) paste0("\u201C", trimws(item$sharing_reason), "\u201D")
                  else item$plan),
      tags$h6(paste0("What is in the file (", nrow(item$data), " rows)")),
      tags$dl(class = "codebook",
              lapply(names(item$codebook), function(column) {
                tagList(tags$dt(tags$code(column)), tags$dd(item$codebook[[column]]))
              }))
    ),
    actions = tagList(
      actionButton(ns(paste0("try_", item$id)), "Try it in \"I can't share my data\"",
                   class = "btn-primary w-100"),
      downloadButton(ns(paste0("download_", item$id)), "Download the data (.csv)",
                     class = "btn-outline-primary w-100")
    )
  )
}

# Server ---------------------------------------------------------------------------

mod_practice_server <- function(id, practice) {
  moduleServer(id, function(input, output, session) {
    sent <- reactiveVal(NULL)
    send <- function(tab, item) sent(list(tab = tab, item = item, at = Sys.time()))

    for (item in practice$check) local({
      example <- item
      observeEvent(input[[paste0("send_", example$id)]], send("check", example))
    })
    for (item in practice$share) local({
      example <- item
      observeEvent(input[[paste0("try_", example$id)]], send("share", example))
      output[[paste0("download_", example$id)]] <- downloadHandler(
        filename = function() example$file,
        content = function(file) file.copy(example$path, file)
      )
    })
    sent
  })
}
