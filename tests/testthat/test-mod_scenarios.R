test_that("every scenario names a valid verdict", {
  scenarios <- read_scenarios(file.path(app_dir, "scenarios"))
  skip_if(length(scenarios) == 0, "no scenarios written yet")
  for (s in scenarios) {
    expect_true(s$expected_verdict %in% reprokit::verdicts(), info = s$title)
  }
})

test_that("the template itself is valid", {
  tpl <- yaml::read_yaml(file.path(app_dir, "scenarios", "template.yml"))
  expect_true(all(c("title", "situation", "question", "expected_verdict") %in% names(tpl)))
  expect_true(tpl$expected_verdict %in% reprokit::verdicts())
})
