# The one place where the app reaches into reprokit.
#
# Most of what the app shows comes from exported reprokit functions. A few
# display helpers are still internal to the package (readable labels for variant
# codes, the coverage-level sentences, the formatting of implied ranges, the
# handler details). They are fetched here, with a fallback, so that:
#   * the rest of the app never uses `:::`;
#   * a rename inside reprokit degrades the display instead of crashing the app.
#
# TODO (package side): export variant_label(), level_words(), format_implied()
# and a handler_info(name) accessor, then replace the bodies below with plain
# reprokit:: calls. No statistics are computed here.

rk_internal <- function(name) {
  tryCatch(utils::getFromNamespace(name, "reprokit"), error = function(e) NULL)
}

# Readable label for a variant code ("welch" -> "Welch's t").
rk_label <- function(codes) {
  f <- rk_internal("variant_label")
  if (is.null(f) || length(codes) == 0L) return(codes)
  vapply(codes, function(code) {
    tryCatch(f(code), error = function(e) code)
  }, character(1), USE.NAMES = FALSE)
}

# The package's own sentence for a coverage level (0 to 3).
rk_level_words <- function(level) {
  f <- rk_internal("level_words")
  if (is.null(f)) return(paste("Level", level))
  f(level)
}

# "0.6364 to 0.8661", as the package prints an implied range.
rk_format_implied <- function(found) {
  f <- rk_internal("format_implied")
  if (is.null(f)) return(paste(found$lower, "to", found$upper))
  f(found)
}

# What the app needs to know about a handler: its labels, what readers lose and
# the effect-size conventions an author can declare in the appendix.
rk_handler_info <- function(name) {
  f <- rk_internal("get_handler")
  h <- if (is.null(f)) NULL else tryCatch(f(name), error = function(e) NULL)
  if (is.null(h)) {
    return(list(name = name, analysis = name, needs = NA_character_, lost = character(),
                conventions = character()))
  }
  conventions <- unique(unlist(lapply(h$relations, function(rel) rel$variants$convention)))
  list(name = h$name, analysis = h$analysis, needs = h$needs, lost = h$lost,
       conventions = if (is.null(conventions)) character() else conventions)
}

# The coverage-table row of one handler. coverage_table() has no handler column
# yet, so the row is found by the handler's analysis label.
# TODO (package side): add a `handler` column to coverage_table().
rk_coverage_row <- function(name) {
  tab <- reprokit::coverage_table()
  row <- tab[tab$test == rk_handler_info(name)$analysis, , drop = FALSE]
  if (nrow(row) == 0L) NULL else row
}
