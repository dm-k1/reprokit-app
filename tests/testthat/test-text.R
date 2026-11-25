# The words, the test catalogue and the worked examples in app/text/.

all_text_files <- function() {
  c(
    list.files(file.path(app_dir, "text"), full.names = TRUE),
    list.files(file.path(app_dir, "practice"), pattern = "\\.(yml|md)$",
               full.names = TRUE),
    list.files(file.path(app_dir, "R"), full.names = TRUE),
    file.path(app_dir, "app.R"),
    file.path(app_dir, "www", "app.css")
  )
}

test_that("the app uses no emoji anywhere", {
  emoji <- "[\U0001F000-\U0001FAFF☀-➿⬀-⯿️]"
  for (file in all_text_files()) {
    lines <- readLines(file, encoding = "UTF-8", warn = FALSE)
    hits <- grep(emoji, lines, perl = TRUE)
    expect(length(hits) == 0L,
           paste0(basename(file), " has emoji on line ", hits[1L]))
  }
  # \u escapes in R code count too.
  for (file in list.files(file.path(app_dir, "R"), full.names = TRUE)) {
    code <- readLines(file, warn = FALSE)
    escaped <- grep("\\\\U0001F|\\\\u2[67][0-9A-F]{2}|\\\\uFE0F", code)
    expect(length(escaped) == 0L,
           paste0(basename(file), " has an escaped emoji on line ",
                  escaped[1L]))
  }
})

test_that("every icon the app names exists in Font Awesome", {
  known <- fontawesome::fa_metadata()$icon_names
  code <- unlist(lapply(list.files(file.path(app_dir, "R"), full.names = TRUE),
                        readLines))
  code <- c(code, readLines(file.path(app_dir, "app.R")))
  in_code <- regmatches(
    code,
    gregexpr("(icon|icon_name = )\\(?\"[a-z0-9-]+\"", code)
  )
  names <- gsub(".*\"([a-z0-9-]+)\"", "\\1", unlist(in_code))
  in_text <- c(
    vapply(txt("statuses"), \(x) x$icon, character(1)),
    vapply(txt("verdicts"), \(x) x$icon, character(1))
  )
  missing <- setdiff(c(names, in_text), known)
  expect(length(missing) == 0L,
         paste("Unknown icons:", paste(missing, collapse = ", ")))
})

test_that("every status reprokit can give has an explanation", {
  expect_true(all(
    c("verified", "coherent", "inconsistent", "not reproducible",
      "unconfirmed", "not checkable") %in% names(txt("statuses"))
  ))
  expect_setequal(names(txt("verdicts")), reprokit::verdicts())
})

test_that("the catalogue offers only what reprokit can do", {
  for (id in names(app_store$tests)) {
    entry <- test_entry(id)
    expect_true(entry$handler %in% reprokit::handlers(), info = id)
    expect_false(is.null(coverage_row(id)), info = id)
    quantities <- unlist(c(
      entry$check$cells$quantities, entry$check$matrix$quantities,
      entry$check$terms$quantities, entry$check$single
    ))
    expect_true(all(quantities %in% reprokit::quantity_names()), info = id)
    for (quantity in quantities) {
      expect_false(identical(quantity_label(quantity), quantity),
                   info = quantity)
    }
  }
})

test_that("the worked example of every test is consistent", {
  for (id in names(app_store$tests)) {
    example <- app_store$examples[[id]]
    expect_false(is.null(example), info = id)
    expect_true(nzchar(example$note), info = id)
    args <- example[intersect(names(example), c("mu", "k", "effect"))]
    out <- check_numbers(id, parse_reported(example$reported), args)
    expect_false(inherits(out, "error"), info = id)
    expect_true(out$result$checked, info = id)
    expect_true(out$result$consistent, info = id)
  }
})

test_that("the grouped menus list every test once", {
  for (direction in c("share", "check")) {
    ids <- unlist(test_choices(direction))
    expect_setequal(ids, test_ids(direction))
  }
  expect_true(all(c("factorial", "chisq", "gof", "mediation") %in%
                    test_ids("share")))
})

test_that("variant codes get readable labels", {
  expect_identical(variant_label(c("welch", "two.sided", "d_z", "yates")),
                   c("Welch's t", "two-sided", "d_z (SD of the differences)",
                     "with Yates' correction"))
  expect_identical(variant_label("unknown"), "unknown")
})

test_that("relations and quantities read as words", {
  expect_identical(checked_with_words("p_from_t:wt; t_from_b_se:wt"),
                   "p from t and df:wt; t from b and SE:wt")
  expect_identical(quantity_label("b:wt"), "Coefficient (b), wt")
})
