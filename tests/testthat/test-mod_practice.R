test_that("there are three examples of each kind", {
  expect_length(practice$check, 3L)
  expect_length(practice$share, 3L)
})

test_that("every check example gets the result its answer describes", {
  for (item in practice$check) {
    expect_true(item$check$handler %in% names(test_catalogue()), info = item$id)
    expect_true(all(vapply(item$check$reported, is.character, logical(1))), info = item$id)
    args <- c(list(handler = item$check$handler), item$check$reported)
    if (!is.null(item$check$mu)) args$mu <- item$check$mu
    x <- do.call(reprokit::check_reported, args)
    expect_identical(x$checked, item$expected$checked, info = item$id)
    if (x$checked) expect_identical(x$consistent, item$expected$consistent, info = item$id)
  }
})

test_that("every practice dataset gets the verdict its answer describes", {
  for (item in practice$share) {
    a <- item$analysis
    x <- share_formula(a$handler, unlist(a$columns))
    verdict <- reprokit::triage(x, data = item$data, test = a$test)$verdict
    expect_identical(verdict, item$expected_verdict, info = item$id)
    expect_true(all(names(item$codebook) %in% names(item$data)), info = item$id)
  }
})

test_that("the cards never name the kind of example they are", {
  revealing <- "consistent|inconsistent|suffice|raw data are needed|fabricat"
  for (item in c(practice$check, practice$share)) {
    shown <- paste(item$title, item$story, item$question, item$as_printed, item$plan)
    expect_false(grepl(revealing, shown, ignore.case = TRUE), info = item$id)
  }
})

test_that("sending a card passes its example on", {
  testServer(mod_practice_server, args = list(practice = practice), {
    session$setInputs(`send_check-02-colour-priming` = 1)
    expect_identical(sent()$tab, "check")
    expect_identical(sent()$item$check$reported$p, "< .001")
    session$setInputs(`try_share-03-night-shift` = 1)
    expect_identical(sent()$tab, "share")
  })
})

test_that("the start page buttons ask for the right tabs", {
  testServer(mod_start_server, {
    session$setInputs(go_practice = 1)
    expect_identical(clicked()$tab, "practice")
    session$setInputs(go_help = 1)
    expect_identical(clicked()$tab, "help")
  })
})
