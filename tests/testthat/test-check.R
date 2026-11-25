# "Does this paper add up?": the form engine and the module.

test_that("an independent-groups check tries both versions", {
  reported <- list(n = c("34", "33"), m = c("41.3", "47.6"),
                   sd = c("9.8", "10.4"), t = "-2.55", df = "65", p = ".013")
  out <- check_numbers("independent", reported)
  expect_named(out$fits, c("student", "welch"))
  expect_true(out$fits[["student"]])
  claimed <- check_numbers("independent", reported, claimed = "welch")
  expect_identical(claimed$claimed, "welch")
  expect_true(claimed$result$consistent)
})

test_that("the version shown follows the claim, then the fit", {
  expect_identical(pick_version(c(student = TRUE, welch = FALSE), ""),
                   "student")
  expect_identical(pick_version(c(student = TRUE, welch = TRUE), "welch"),
                   "welch")
  expect_identical(pick_version(c(student = FALSE, welch = FALSE), ""),
                   "welch")
})

test_that("practice YAML becomes check_reported() input", {
  raw <- list(n = list(c("1", "2"), c("3", "4")), b = list(x = "1.5"),
              m = c("5", "6"))
  out <- parse_reported(raw)
  expect_true(is.matrix(out$n))
  expect_identical(out$n[2, 1], "3")
  expect_identical(out$b, c(x = "1.5"))
  expect_identical(out$m, c("5", "6"))
})

test_that("the form collects cells, tables, coefficients and arguments", {
  spec <- test_entry("factorial")$check
  input <- list(arg_effect = "B", num_f = " 3.1 ")
  for (i in 1:2) for (j in 1:2) {
    input[[paste0("mat_n_", i, "_", j)]] <- "10"
  }
  out <- collect_form(input, spec, list(rows = 2L, cols = 2L))
  expect_null(out$problem)
  expect_identical(dim(out$reported$n), c(2L, 2L))
  expect_identical(out$reported$f, "3.1")
  expect_identical(out$args$effect, "B")

  lm_spec <- test_entry("lm")$check
  input <- list(term_name_1 = "sleep", term_name_2 = "support",
                term_b_1 = "-1.66", term_b_2 = "-1.94", arg_k = 2)
  out <- collect_form(input, lm_spec, list(terms = 2L))
  expect_identical(out$reported$b, c(sleep = "-1.66", support = "-1.94"))
  expect_identical(out$args$k, 2L)
})

test_that("half-filled rows and nameless coefficients are explained", {
  spec <- test_entry("independent")$check
  out <- collect_form(list(cell_n_1 = "10", cell_n_2 = ""), spec,
                      list(cells = 2L))
  expect_match(out$problem, "every group")

  lm_spec <- test_entry("lm")$check
  out <- collect_form(list(term_b_1 = "1.2", term_name_1 = ""), lm_spec,
                      list(terms = 2L))
  expect_match(out$problem, "name")
})

test_that("goodness of fit passes the expected proportions as numbers", {
  spec <- test_entry("gof")$check
  input <- list(cell_n_1 = "20", cell_n_2 = "30", cell_p0_1 = ".4",
                cell_p0_2 = ".6")
  out <- collect_form(input, spec, list(cells = 2L))
  expect_identical(out$args$p0, c(0.4, 0.6))
  expect_identical(out$args$dims, 2L)
})

test_that("an example fills the form for its own test only", {
  incoming <- reactiveVal(NULL)
  testServer(mod_check_server, args = list(incoming = incoming), {
    session$setInputs(test = "independent")
    session$setInputs(example = 1)
    expect_identical(current_fill()$reported$t, "-1.86")

    incoming(list(tab = "check",
                  item = practice$check[["check-02-colour-priming"]]))
    session$flushReact()
    expect_null(current_fill())
    session$setInputs(test = "oneway")
    expect_identical(current_fill()$reported$f, "21.84")
    expect_identical(shape()$cells, 3L)

    session$setInputs(shape_cells = "4")
    expect_null(filled())
  })
})

test_that("pressing Check runs reprokit on the form", {
  testServer(mod_check_server, {
    session$setInputs(test = "paired", claim_tail = "", num_n = "20",
                      cell_m_1 = "5.1", cell_m_2 = "4.6", cell_sd_1 = "1.2",
                      cell_sd_2 = "1.1", num_t = "2.85", num_df = "19",
                      num_p = ".010", arg_mu = 0, go = 1)
    expect_true(fresh())
    expect_true(outcome()$result$consistent)
    expect_identical(outcome()$result$missing$quantity, "r")
  })
})

test_that("both t-test versions show among the choices that fit", {
  reported <- list(n = c("34", "33"), m = c("41.3", "47.6"),
                   sd = c("9.8", "10.4"), t = "-2.55", df = "65", p = ".013")
  out <- check_numbers("independent", reported)
  html <- as.character(check_choices_ui(out, list()))
  expect_match(html, "Student&#39;s t|Student's t")
  expect_false(grepl("Technical output", html))
  expect_match(html, "Welch&#39;s t|Welch's t")
})
