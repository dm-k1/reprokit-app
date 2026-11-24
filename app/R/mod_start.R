# Tab 0: "Start here". Who the app is for, what it does, where the data go.
# Layout, top to bottom: one sentence on APA's policy; the two directions as
# cards that turn over when clicked (readers left, authors right); the practice
# examples; the APA background beside what happens to the data and where to
# find help.
# Returns a reactive with the tab a user asked to go to (or NULL).

mod_start_ui <- function(id) {
  ns <- NS(id)
  page_body(
    class = "start-page",
    p(class = "start-intro",
      "APA asks authors to share their data and to report a sufficient set of statistics;",
      "the oracle helps authors meet that obligation and helps readers check that it was met."),

    layout_columns(
      col_widths = c(6, 6),
      flip_card(
        ns, target = "check",
        front_title = "Help readers check article statistics",
        question = "Do this paper's numbers fit together?",
        you_get = "A status for each number, a coverage level, and what is missing",
        back_icon = "\U0001F50D", back_title = "I'm reading a paper",
        back_text = paste("Type in the numbers a paper reports and see whether they fit together,",
                          "and how much of the result can be checked at all.")
      ),
      flip_card(
        ns, target = "share",
        front_title = "Help authors share their data",
        question = "What must I report so others can reproduce this?",
        you_get = "A verdict, the appendix, and what readers lose",
        back_icon = "\u270D\uFE0F", back_title = "I wrote a study and can't share my data",
        back_text = paste("Find out whether summary statistics let readers reproduce your result,",
                          "and download a ready-to-paste appendix.")
      )
    ),
    p(class = "start-note",
      "Both directions use the same statistics: what an author's appendix reports is exactly",
      "what a reader's check needs."),

    card(
      class = "practice-box",
      card_body(
        tags$div(
          class = "practice-content",
          tags$div(class = "practice-icon", "\U0001F393"),
          tags$div(
            tags$h4("Learn how to use the Oracle with practical examples"),
            p("Six fictional psychology studies to try the oracle on: three papers whose numbers",
              "you can check, and three datasets whose authors cannot or will not share them.",
              "Run each one yourself, then open the answer."),
            p(class = "mb-0", tags$b("Tip:"), "every",
              help_icon("Like this one. Click it again to close it."),
              "opens a short explanation.")
          ),
          actionButton(ns("go_practice"), "See the practice examples", class = "btn-primary")
        )
      )
    ),

    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("APA and data sharing"),
        p("APA style already asks you to report \"a sufficient set of statistics\", and says",
          "that what counts as sufficient cannot be specified in general. This app specifies it",
          "for each analysis, and checks whether a paper's numbers meet it."),
        explain(title = "The one rule behind everything here",
                p(standard_reminder()),
                p("This app is for when sharing is genuinely impossible. It then tells you the",
                  "minimum you still owe your readers, and exactly what they lose."))
      ),
      div(
        class = "stack",
        card(
          card_header("What happens to my data?"),
          p("Nothing leaves your computer. The online version runs R inside your web browser",
            "(webR); the local version runs in R on your own machine. Either way, a file you",
            "open here is read on your own computer and is never uploaded to a server. That is",
            "part of the lesson: protecting participants starts with not moving their data",
            "around.")
        ),
        card(
          class = "help-box",
          card_body(
            div(class = "practice-content",
                div(class = "practice-icon", "\U0001F9ED"),
                div(tags$h5("Want to know more?"),
                    p(class = "mb-0", tags$i("Help: How the App works"), "explains every word",
                      "the app uses, what each result means, and what can be checked for",
                      "each test.")),
                actionButton(ns("go_help"), "Open the help", class = "btn-outline-primary"))
          )
        )
      )
    ),
    flip_card_script()
  )
}

# A card that turns over when clicked or when Enter/Space is pressed on it.
# Front: the direction, its question and what you get. Back: who it is for and
# a Start button (clicking the button does not turn the card).
flip_card <- function(ns, target, front_title, question, you_get, back_icon, back_title,
                      back_text) {
  turn_hint <- tags$div(class = "flip-hint", "\u21BB Click to turn")
  tags$div(
    class = "flip-card", role = "button", tabindex = "0", `aria-pressed` = "false",
    `aria-label` = paste(front_title, "- click to turn the card"),
    tags$div(
      class = "flip-inner",
      tags$div(
        class = "flip-face flip-front",
        tags$h3(front_title),
        tags$dl(
          tags$dt("Question"), tags$dd(question),
          tags$dt("You get"), tags$dd(you_get)
        ),
        turn_hint
      ),
      tags$div(
        class = "flip-face flip-back",
        tags$div(class = "flip-back-icon", back_icon),
        tags$h4(back_title),
        p(back_text),
        actionButton(ns(paste0("go_", target)), "Start", class = "btn-primary w-100"),
        turn_hint
      )
    )
  )
}

# One delegated handler for every flip card on the page.
flip_card_script <- function() {
  tags$script(HTML("
    (function() {
      function turn(card) {
        card.classList.toggle('flipped');
        card.setAttribute('aria-pressed', card.classList.contains('flipped'));
      }
      document.addEventListener('click', function(e) {
        var card = e.target.closest('.flip-card');
        if (!card || e.target.closest('button, a, .help-icon')) return;
        turn(card);
      });
      document.addEventListener('keydown', function(e) {
        var card = e.target.closest ? e.target.closest('.flip-card') : null;
        if (!card || e.target !== card) return;
        if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); turn(card); }
      });
    })();
  "))
}

mod_start_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    clicked <- reactiveVal(NULL)
    for (tab in c("share", "check", "practice", "help")) local({
      target <- tab
      observeEvent(input[[paste0("go_", target)]], clicked(list(tab = target, at = Sys.time())))
    })
    clicked
  })
}
