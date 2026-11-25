# Practice examples (app/practice/).

test_that("there are three examples of each kind", {
  expect_length(practice$check, 3L)
  expect_length(practice$share, 3L)
})

test_that("every check example gets the result its answer describes", {
  for (item in practice$check) {
    x <- item$check
    expect_true(x$test %in% test_ids("check"), info = item$id)
    out <- check_numbers(x$test, parse_reported(x$reported),
                         x$args %||% list())
    expect_identical(out$result$checked, item$expected$checked,
                     info = item$id)
    if (out$result$checked) {
      expect_identical(out$result$consistent, item$expected$consistent,
                       info = item$id)
    }
  }
})

test_that("the cards never name the kind of example they are", {
  revealing <- "consistent|inconsistent|suffice|raw data are needed|fabricat"
  for (item in c(practice$check, practice$share)) {
    shown <- paste(item$title, item$story, item$question, item$as_printed,
                   item$plan)
    expect_false(grepl(revealing, shown, ignore.case = TRUE), info = item$id)
  }
})

test_that("sending an example passes it on", {
  testServer(mod_practice_server, args = list(practice = practice), {
    session$setInputs(kind = "check", pick_check = "check-02-colour-priming",
                      send = 1)
    expect_identical(sent()$tab, "check")
    expect_identical(sent()$item$check$reported$p, "< .001")
    session$setInputs(kind = "share", pick_share = "share-03-night-shift",
                      send = 2)
    expect_identical(sent()$tab, "share")
  })
})

test_that("the page is one screen: two tools and a help tab", {
  html <- as.character(app_ui(practice))
  for (tab in c("check", "share", "help")) {
    expect_match(html, paste0("data-value=\"", tab, "\""))
  }
  for (label in c("Check Paper", "Publish data", "Help")) {
    expect_match(html, label, fixed = TRUE)
  }
  expect_false(grepl("navbar|offcanvas", html))
})

test_that("help sub-tabs come in the agreed order", {
  words <- txt("help", "tabs")
  expect_identical(
    names(words),
    c("how", "practice", "results", "words", "coverage", "apa", "data")
  )
  expect_identical(words$data, "Data safety")
  html <- as.character(help_tab(practice))
  find <- \(w) regexpr(w, html, fixed = TRUE)[[1]]
  positions <- vapply(unlist(words), find, 1)
  expect_true(all(positions > 0))
  expect_false(is.unsorted(positions))
  expect_match(html, "For readers who want to check", fixed = TRUE)
})

test_that("no drag handles; Publish data runs with Check this dataset", {
  html <- as.character(app_ui(practice))
  expect_false(grepl("drag-handle", html, fixed = TRUE))
  expect_match(html, "Check this dataset", fixed = TRUE)
  expect_false(grepl("Consult the oracle", html, fixed = TRUE))
})

# Whether every sidebar is fixed open. Older bslib leaves the fold button
# out; newer bslib always writes it and hides it for fixed sidebars, which it
# marks with data-collapsible-desktop="false".
sidebars_fixed <- function(html) {
  !grepl("collapse-toggle", html, fixed = TRUE) ||
    !grepl("data-collapsible-desktop=\"true\"", html, fixed = TRUE)
}

test_that("sidebars have no collapse toggle and step 3 help is a fold-out", {
  html <- as.character(app_ui(practice))
  expect_true(sidebars_fixed(html))
  expect_match(html, "<details class=\"fold\">", fixed = TRUE)
})

test_that("result boxes show a title line, then the value and its hint", {
  box <- as.character(result_box("Verdict", "Consistent", "All fine", "check"))
  expect_match(box, "class=\"oracle-box")
  for (part in c("box-title", "box-line", "box-value", "box-hint")) {
    expect_match(box, part)
  }
  expect_false(grepl("bslib-value-box", box, fixed = TRUE))
})

test_that("sidebars cannot be resized or folded away", {
  html <- as.character(app_ui(practice))
  expect_false(grepl("data-resizable", html, fixed = TRUE))
  expect_true(sidebars_fixed(html))
})

test_that("the title bar has a dark mode switch", {
  html <- as.character(title_bar())
  expect_match(html, "id=\"theme_toggle\"", fixed = TRUE)
  expect_match(html, "Dark mode", fixed = TRUE)
  expect_match(html, "Light mode", fixed = TRUE)
  css <- readLines(file.path(app_dir, "www", "app.css"))
  expect_true(any(grepl("[data-bs-theme=\"dark\"]", css, fixed = TRUE)))
})

test_that("unconfirmed is neutral grey, not the green-tinted slate", {
  expect_identical(txt("statuses", "unconfirmed", "class"), "secondary")
  colours <- bslib::bs_get_variables(app_theme(), "secondary")
  expect_identical(tolower(unname(colours)), "#6c757d")
})

test_that("the data sources are plain nouns, practice dataset last", {
  sources <- txt("share", "sources")
  expect_identical(names(sources), c("csv", "rds", "demo"))
  expect_false(any(grepl("^(A|An) ", unlist(sources))))
})

test_that("sending an example switches the tool tab", {
  testServer(app_server(practice), {
    session$setInputs(`practice-kind` = "share",
                      `practice-pick_share` = "share-02-face-memory",
                      `practice-send` = 1)
    expect_identical(sent()$tab, "share")
    expect_identical(sent()$item$id, "share-02-face-memory")
  })
})
