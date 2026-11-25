# The app's calls to reprokit, in one place. Each wrapper returns a value or
# a condition object instead of throwing, so the modules can explain what
# went wrong. Nothing here computes a statistic.

# Share ------------------------------------------------------------------------

# Whether summaries suffice, from sufficient_stats(), which never errors on
# the verdict. Invalid input comes back as the error object.
share_verdict <- function(x, args = list(), threshold = 5) {
  tryCatch(
    do.call(
      reprokit::sufficient_stats,
      c(list(x), args, list(threshold = threshold))
    ),
    error = identity
  )
}

# The Markdown appendix, or the error object when it cannot be written.
share_appendix <- function(stats, sharing_reason, choices = list()) {
  choices <- Filter(\(value) !is.null(value) && nzchar(value), choices)
  tryCatch(
    reprokit::appendix(
      stats,
      sharing_reason = sharing_reason,
      choices = choices
    ),
    error = identity
  )
}

# The appendix choices an author still has to make for a handler: the
# effect-size convention (when there is more than one), the sums of squares
# type (factorial ANOVA) and the CI level (regression and mediation).
appendix_choice_specs <- function(handler) {
  rows <- reprokit::conventions(handler)
  specs <- list()
  conventions <- rows[rows$choice == "convention", , drop = FALSE]
  if (nrow(conventions) > 1L) {
    specs$convention <- list(required = TRUE)
  }
  if (any(rows$choice == "ss_type")) {
    specs$ss_type <- list(required = FALSE)
  }
  regression <- c("lm_simple", "lm_multiple", "lm_moderation",
                  "mediation_simple")
  if (handler %in% regression) {
    specs$level <- list(required = FALSE)
  }
  specs
}

# Check ------------------------------------------------------------------------

# Runs check_reported() for a test. A two-version test (independent groups)
# is checked under both versions; `claimed` is the version the paper names,
# or "" when it does not say. Returns list(result, by_version, fits,
# claimed), or the error object.
check_numbers <- function(id, reported, args = list(), claimed = "") {
  tests <- unlist(test_entry(id)$check$tests)
  run <- function(test) {
    tryCatch(
      do.call(reprokit::check_reported, c(list(test), reported, args)),
      error = identity
    )
  }
  by_version <- stats::setNames(lapply(tests, run), tests)
  errors <- Filter(\(x) inherits(x, "error"), by_version)
  if (length(errors) == length(by_version)) return(errors[[1L]])
  by_version <- Filter(\(x) !inherits(x, "error"), by_version)
  fits <- vapply(by_version, \(x) isTRUE(x$consistent), logical(1))
  list(
    result = by_version[[pick_version(fits, claimed)]],
    by_version = by_version,
    fits = fits,
    claimed = claimed
  )
}

# Which version's result to show: the claimed one if it fits, else the first
# that fits, else the claimed one, else the last (Welch, R's default).
pick_version <- function(fits, claimed) {
  if (nzchar(claimed) && isTRUE(fits[claimed])) return(claimed)
  if (any(fits)) return(names(fits)[fits][1L])
  if (nzchar(claimed) && claimed %in% names(fits)) return(claimed)
  names(fits)[length(fits)]
}

# Counts of the statuses, for the value box.
status_counts <- function(numbers) {
  status <- numbers$status
  list(
    total = length(status),
    checked = sum(status %in% c("verified", "coherent", "inconsistent")),
    inconsistent = sum(status == "inconsistent"),
    unconfirmed = sum(status == "unconfirmed")
  )
}
