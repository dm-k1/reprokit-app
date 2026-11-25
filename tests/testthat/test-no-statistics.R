# The app contains no statistics: every number must come from reprokit::.
# This catches the common slips. Extend the list when you spot a new one.
# It reads the parsed code, so the words "mean" or "lm()" inside help texts and
# strings are not flagged; only real function calls are.
test_that("app code calls no statistical functions directly", {
  forbidden <- c(
    "lm", "glm", "aov", "anova", "t.test", "chisq.test", "cor.test", "wilcox.test",
    "mean", "median", "sd", "var", "cor", "cov", "quantile", "scale",
    "pt", "pf", "pn", "qt", "qf", "qn", "pchisq", "qchisq", "pbinom", "round", "signif"
  )
  files <- list.files(app_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  hits <- unlist(lapply(files, function(f) {
    tokens <- utils::getParseData(parse(f, keep.source = TRUE))
    calls <- tokens[tokens$token == "SYMBOL_FUNCTION_CALL" & tokens$text %in% forbidden, ]
    if (nrow(calls)) paste0(basename(f), ":", calls$line1, ": ", calls$text, "()")
  }))
  expect(length(hits) == 0, paste(c("Statistics in app code:", hits), collapse = "\n"))
})

test_that("the forbidden-call check itself still bites", {
  f <- withr::local_tempfile(fileext = ".R")
  writeLines(c("x <- stats::sd(1:3)", "y <- \"the mean (of x)\""), f)
  tokens <- utils::getParseData(parse(f, keep.source = TRUE))
  called <- tokens$text[tokens$token == "SYMBOL_FUNCTION_CALL"]
  expect_true("sd" %in% called)
  expect_false("mean" %in% called)
})
