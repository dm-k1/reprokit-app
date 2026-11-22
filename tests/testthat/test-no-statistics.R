# The app contains no statistics: every number must come from reprokit::.
# This catches the common slips. Extend the list when you spot a new one.
test_that("app code calls no statistical functions directly", {
  forbidden <- c(
    "lm", "glm", "aov", "anova", "t\\.test", "chisq\\.test", "cor\\.test", "wilcox\\.test",
    "mean", "median", "sd", "var", "cor", "cov", "quantile", "scale",
    "p[tfn]", "q[tfn]", "pchisq", "qchisq", "pbinom", "round", "signif"
  )
  pattern <- paste0("(?<![[:alnum:]_.:])(stats::)?(", paste(forbidden, collapse = "|"), ")\\s*\\(")
  files <- list.files(app_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  hits <- unlist(lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    code <- sub("#.*$", "", lines)
    idx <- grep(pattern, code, perl = TRUE)
    if (length(idx)) paste0(basename(f), ":", idx, ": ", trimws(lines[idx]))
  }))
  expect(length(hits) == 0, paste(c("Statistics in app code:", hits), collapse = "\n"))
})
