test_that("the worked example of every test is consistent according to reprokit", {
  for (handler in names(check_examples())) {
    ex <- check_examples()[[handler]]
    expect_true(nzchar(ex$note), info = handler)
    args <- c(list(handler = handler), ex$reported)
    if (!is.null(ex$mu)) args$mu <- ex$mu
    x <- do.call(reprokit::check_reported, args)
    expect_true(x$checked, info = handler)
    expect_true(x$consistent, info = handler)
  }
})

test_that("the app offers only tests reprokit can check, with known quantities", {
  for (handler in names(test_catalogue())) {
    e <- test_catalogue()[[handler]]
    expect_true(handler %in% reprokit::handlers(), info = handler)
    expect_true(all(c(e$check$per_cell, e$check$single) %in% reprokit::quantity_names()),
                info = handler)
  }
})

test_that("every status reprokit can give has an explanation", {
  expect_setequal(names(status_info()), c("verified", "coherent", "inconsistent",
                                          "not reproducible", "unconfirmed", "not checkable"))
})

test_that("a half-filled group row is explained instead of sent to reprokit", {
  e <- test_catalogue()$t_independent
  input <- list(cell_n_1 = "10", cell_n_2 = "", num_t = "2.1")
  out <- collect_reported(input, e, 2L)
  expect_match(out$problem, "every group")
})

test_that("the check runs from the form and loads a scenario", {
  incoming <- reactiveVal(NULL)
  testServer(mod_check_server, args = list(incoming = incoming), {
    session$setInputs(handler = "t_independent", cell_n_1 = "10", cell_n_2 = "10",
                      cell_m_1 = "0.75", cell_m_2 = "2.33", cell_sd_1 = "1.79",
                      cell_sd_2 = "2.00", num_t = "-1.86", num_df = "17.8", num_p = ".079",
                      declared_test = "student", declared_tail = "", go = 1)
    x <- result()$check
    expect_s3_class(x, "reprokit_check")
    expect_true(x$consistent)
    expect_identical(result()$declared$test, "student")

    session$setInputs(cell_n_2 = "", go = 2)
    expect_error(result(), "every group", class = "shiny.silent.error")
  })
})

test_that("a practice example fills the form for its own test only", {
  incoming <- reactiveVal(NULL)
  testServer(mod_check_server, args = list(incoming = incoming), {
    session$setInputs(handler = "t_independent", groups = 3)
    session$setInputs(example = 1)
    expect_identical(current_fill()$reported$t, "-1.86")
    expect_match(current_fill()$note, "sleep aid")

    incoming(list(tab = "check", item = practice$check[["check-02-colour-priming"]]))
    session$flushReact()
    expect_null(current_fill())                # the form still shows the t-test
    session$setInputs(handler = "anova_oneway")
    expect_identical(current_fill()$reported$f, "21.84")
    expect_identical(n_cells(), 3L)
    expect_false(fresh())

    session$setInputs(groups = 4)              # the user changes the form: drop the fill
    expect_null(filled())
  })
})
