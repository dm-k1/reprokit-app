# "I can't share my data".

test_that("every share test builds a formula reprokit accepts", {
  sleep <- datasets::sleep
  wide <- data.frame(a = sleep$extra[1:10], b = sleep$extra[11:20],
                     c = rev(sleep$extra[1:10]))
  counts <- data.frame(
    x = rep(c("yes", "no"), c(30, 30)),
    y = rep(c("a", "b", "a", "b"), c(18, 12, 9, 21))
  )
  two_way <- data.frame(
    score = c(datasets::warpbreaks$breaks),
    wool = datasets::warpbreaks$wool,
    tension = datasets::warpbreaks$tension
  )
  cases <- list(
    independent = list(sleep, c(outcome = "extra", group = "group"), "welch"),
    one_sample = list(sleep, c(outcome = "extra"), "one_sample"),
    paired = list(wide, c(first = "a", second = "b"), "paired"),
    oneway = list(datasets::PlantGrowth,
                  c(outcome = "weight", group = "group"), "oneway"),
    factorial = list(two_way,
                     c(outcome = "score", factor_a = "wool",
                       factor_b = "tension"), "factorial"),
    pearson = list(datasets::mtcars, c(x = "mpg", y = "wt"), "pearson"),
    chisq = list(counts, c(row = "x", col = "y"), "chisq"),
    gof = list(counts, c(category = "y"), "gof")
  )
  for (id in names(cases)) {
    case <- cases[[id]]
    s <- share_verdict(share_formula(id, case[[2L]]),
                       list(data = case[[1L]], test = case[[3L]]))
    expect_s3_class(s, "reprokit_sufficient_stats")
    expect_identical(s$verdict, "summaries_suffice", info = id)
  }
  mediation <- share_verdict(
    share_formula("mediation", c(y = "c", x = "a")),
    list(data = wide, test = "mediation", mediator = "b")
  )
  expect_s3_class(mediation, "reprokit_sufficient_stats")
})

test_that("every practice dataset gets the verdict its answer describes", {
  for (item in practice$share) {
    a <- item$analysis
    s <- share_verdict(share_formula(a$test, unlist(a$columns)),
                       list(data = item$data, test = a$version))
    expect_identical(s$verdict, item$expected_verdict, info = item$id)
    expect_true(all(names(item$codebook) %in% names(item$data)),
                info = item$id)
  }
})

test_that("the appendix waits for a declared effect size", {
  item <- practice$share[["share-01-school-anxiety"]]
  s <- share_verdict(share_formula("oneway", unlist(item$analysis$columns)),
                     list(data = item$data, test = "oneway"))
  expect_true(appendix_choice_specs(s$handler)$convention$required)
  lines <- share_appendix(s, "Consent did not cover sharing.",
                          list(convention = "eta2", ss_type = NULL))
  expect_s3_class(lines, "reprokit_appendix")
  expect_true(any(grepl("Consent did not cover", lines, fixed = TRUE)))
})

test_that("input problems are explained before reprokit is called", {
  d <- datasets::PlantGrowth
  expect_match(column_problem("independent", d,
                              c(outcome = "weight", group = "group")),
               "exactly 2")
  expect_match(column_problem("paired", d,
                              c(first = "weight", second = "weight")),
               "different column")
  expect_match(column_problem("oneway", d,
                              c(outcome = "weight", group = "gone")),
               "again")
})

test_that("column guesses prefer groups with few levels", {
  d <- data.frame(id = factor(1:20), arm = rep(c("a", "b"), 10), y = 1:20)
  expect_identical(default_column(d, "group"), "arm")
  expect_identical(default_column(d, "numeric"), "y")
})

test_that("a practice card loads its dataset, analysis and reason", {
  item <- practice$share[["share-03-night-shift"]]
  testServer(mod_share_server, args = list(datasets = practice$share), {
    session$setInputs(source = "demo", demo = item$id, test = "independent")
    expect_identical(preset()$columns$group, "shift")
    expect_identical(preset()$version, "welch")
    expect_identical(dataset(), item$data)
  })
})

test_that("a version left over from another test is ignored", {
  testServer(mod_share_server, args = list(datasets = practice$share), {
    session$setInputs(source = "demo", demo = "share-01-school-anxiety",
                      test = "oneway", version = "welch",
                      col_outcome = "anxiety_after", col_group = "programme")
    expect_identical(analysis()$args$test, "oneway")
  })
})

test_that("no verdict without a stated reason, then a verdict with one", {
  testServer(mod_share_server, args = list(datasets = practice$share), {
    session$setInputs(source = "demo", demo = "share-01-school-anxiety",
                      test = "oneway", col_outcome = "anxiety_after",
                      col_group = "programme", sharing_reason = "",
                      threshold = 5, go = 1)
    expect_false(fresh())
    session$setInputs(sharing_reason = "No consent.", go = 2)
    expect_true(fresh())
    expect_identical(result()$verdict, "summaries_suffice")
  })
})

test_that("a semicolon CSV with decimal commas is read", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("score;cond",
               paste0(c("1,5", "2,1", "3,0", "2,2", "1,9", "2,8"), ";a"),
               paste0(c("3,5", "4,1", "3,9", "4,2", "3,3", "4,8"), ";b")),
             path)
  testServer(mod_share_server, args = list(datasets = practice$share), {
    session$setInputs(source = "csv", csv = list(datapath = path), sep = ";",
                      dec = ",")
    expect_true(is.numeric(dataset()$score))
  })
})
