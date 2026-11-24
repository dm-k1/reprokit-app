test_that("no verdict without a stated reason for not sharing", {
  testServer(mod_share_server, args = list(datasets = practice$share), {
    session$setInputs(source = "rds", sharing_reason = "", model = NULL, go = 1)
    expect_error(result(), "state why", class = "shiny.silent.error")
  })
})

test_that("an unknown model with a reason gets a reprokit verdict, not an error", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(list(), path)
  testServer(mod_share_server, args = list(datasets = practice$share), {
    session$setInputs(source = "rds", sharing_reason = "No consent for sharing.",
                      lost_extra = "", threshold = 5, model = list(datapath = path), go = 1)
    expect_s3_class(result()$triage, "reprokit_triage")
    expect_identical(result()$triage$verdict, "needs_raw_data")
    expect_null(result()$stats)
    expect_true(length(result()$alternatives) > 0L)
  })
})

test_that("a practice ANOVA goes all the way to an appendix", {
  testServer(mod_share_server, args = list(datasets = practice$share), {
    session$setInputs(source = "demo", demo = "share-01-school-anxiety",
                      handler = "anova_oneway", col_outcome = "anxiety_after",
                      col_group = "programme", sharing_reason = "Consent did not cover sharing.",
                      lost_extra = "", threshold = 5, go = 1)
    expect_identical(result()$triage$verdict, "summaries_suffice")
    expect_s3_class(result()$stats, "reprokit_sufficient_stats")
    expect_true("f" %in% names(result()$reproduced))
    expect_true(convention_missing())
    session$setInputs(convention = "eta2")
    expect_false(convention_missing())
    expect_s3_class(appendix_lines(), "reprokit_appendix")
    expect_true(any(grepl("Consent did not cover", appendix_lines(), fixed = TRUE)))
  })
})

test_that("every test has a working formula", {
  sleep <- datasets::sleep
  wide <- data.frame(a = sleep$extra[1:10], b = sleep$extra[11:20])
  cases <- list(
    list("t_independent", sleep, c(outcome = "extra", group = "group"), "welch"),
    list("t_one_sample", sleep, c(outcome = "extra"), "one_sample"),
    list("t_paired", wide, c(first = "a", second = "b"), "paired"),
    list("anova_oneway", datasets::PlantGrowth, c(outcome = "weight", group = "group"), "oneway"),
    list("cor_pearson", datasets::mtcars, c(x = "mpg", y = "wt"), "pearson")
  )
  for (case in cases) {
    out <- run_share(share_formula(case[[1L]], case[[3L]]),
                     list(data = case[[2L]], test = case[[4L]]), "reason", character(), 5)
    expect_identical(out$triage$verdict, "summaries_suffice", info = case[[1L]])
    expect_s3_class(out$stats, "reprokit_sufficient_stats")
  }
})

test_that("input problems are explained before reprokit is called", {
  d <- datasets::PlantGrowth
  expect_match(column_problem("t_independent", d, c(outcome = "weight", group = "group")),
               "exactly 2")
  expect_match(column_problem("t_paired", d, c(first = "weight", second = "weight")),
               "different column")
  expect_null(column_problem("anova_oneway", d, c(outcome = "weight", group = "group")))
})

test_that("a practice card loads its dataset, analysis and reason", {
  item <- practice$share[["share-03-night-shift"]]
  incoming <- reactiveVal(NULL)
  testServer(mod_share_server, args = list(datasets = practice$share, incoming = incoming), {
    session$setInputs(source = "demo", demo = item$id, handler = "t_independent")
    expect_identical(preset()$columns$group, "shift")
    expect_identical(preset()$test, "welch")
    expect_identical(dataset(), item$data)
  })
})

test_that("a semicolon CSV with decimal commas is read", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("score;cond", paste0(c("1,5", "2,1", "3,0", "2,2", "1,9", "2,8"), ";a"),
               paste0(c("3,5", "4,1", "3,9", "4,2", "3,3", "4,8"), ";b")), path)
  testServer(mod_share_server, args = list(datasets = practice$share), {
    session$setInputs(source = "csv", csv = list(datapath = path), sep = ";", dec = ",",
                      handler = "t_independent", col_outcome = "score", col_group = "cond",
                      test = "student", alternative = "two.sided", conf_level = "0.95",
                      sharing_reason = "Consent.", lost_extra = "", threshold = 5, go = 1)
    expect_true(is.numeric(dataset()$score))
    expect_identical(result()$triage$verdict, "summaries_suffice")
  })
})

test_that("a test version left over from another test is ignored", {
  testServer(mod_share_server, args = list(datasets = practice$share), {
    session$setInputs(source = "demo", demo = "share-01-school-anxiety",
                      handler = "anova_oneway", test = "welch",
                      col_outcome = "anxiety_after", col_group = "programme")
    expect_identical(analysis()$args$test, "oneway")
  })
})
