# The app contains no statistics: every number must come from reprokit::.
# This catches the common slips. Extend the list when you spot a new one.
# It reads the parsed code, so the words "mean" or "lm()" inside help texts
# and strings are not flagged; only real function calls are.

forbidden_calls <- c(
  "lm", "glm", "aov", "anova", "t.test", "chisq.test", "cor.test",
  "wilcox.test", "mean", "median", "sd", "var", "cor", "cov", "quantile",
  "scale", "pt", "pf", "pn", "qt", "qf", "qn", "pchisq", "qchisq", "pbinom",
  "round", "signif"
)

function_calls <- function(file) {
  tokens <- utils::getParseData(parse(file, keep.source = TRUE))
  tokens[tokens$token == "SYMBOL_FUNCTION_CALL", c("line1", "text")]
}

test_that("app code calls no statistical functions directly", {
  files <- list.files(app_dir, pattern = "\\.R$", recursive = TRUE,
                      full.names = TRUE)
  hits <- unlist(lapply(files, function(file) {
    calls <- function_calls(file)
    calls <- calls[calls$text %in% forbidden_calls, ]
    if (nrow(calls) > 0L) {
      paste0(basename(file), ":", calls$line1, ": ", calls$text, "()")
    }
  }))
  expect(
    length(hits) == 0L,
    paste(c("Statistics in app code:", hits), collapse = "\n")
  )
})

test_that("the forbidden-call check itself still bites", {
  file <- withr::local_tempfile(fileext = ".R")
  writeLines(c("x <- stats::sd(1:3)", "y <- \"the mean (of x)\""), file)
  called <- function_calls(file)$text
  expect_true("sd" %in% called)
  expect_false("mean" %in% called)
})
