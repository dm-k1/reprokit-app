# The test catalogue (text/tests.yml) and the small translations between the
# app and reprokit: menus, formulas, labels. Nothing here computes a
# statistic; the formula is built from the column names the user picks.

test_entry <- function(id) {
  entry <- app_store$tests[[id]]
  if (is.null(entry)) stop("Unknown test: ", id, call. = FALSE)
  entry
}

# The test ids offered in a direction ("share" or "check"), in menu order.
test_ids <- function(direction) {
  ids <- names(app_store$tests)
  ids[vapply(ids, \(id) !is.null(test_entry(id)[[direction]]), logical(1))]
}

# Choices for a drop-down menu, grouped (Compare means, Relationships,
# Counts): the test's name first, then in brackets what it is for.
test_choices <- function(direction) {
  ids <- test_ids(direction)
  groups <- vapply(ids, \(id) test_entry(id)$group, character(1))
  menus <- vapply(ids, \(id) test_entry(id)$menu, character(1))
  lapply(split(ids, factor(groups, unique(groups))), \(group_ids) {
    stats::setNames(as.list(group_ids), menus[group_ids])
  })
}

# Formulas ---------------------------------------------------------------------

# reprokit's formula for a share test, with backticks so any column name
# works: "{outcome} ~ {group}" becomes `score` ~ `condition`.
share_formula <- function(id, cols) {
  text <- test_entry(id)$share$formula
  for (role in names(cols)) {
    text <- gsub(
      paste0("{", role, "}"),
      paste0("`", cols[[role]], "`"),
      text,
      fixed = TRUE
    )
  }
  stats::as.formula(text, env = globalenv())
}

# Input problems the user can fix, in plain words (NULL when fine). These
# check the shape of the input, not statistics.
column_problem <- function(id, data, cols) {
  if (!all(cols %in% names(data))) {
    return("Pick the columns again: the data have changed.")
  }
  if (anyDuplicated(cols)) {
    return("Pick a different column for each role.")
  }
  if (id == "independent") {
    groups <- length(unique(stats::na.omit(data[[cols[["group"]]]])))
    if (groups != 2L) {
      return(paste0(
        "The group column has ", groups, " groups; this test needs exactly ",
        "2. For 3 or more groups, choose the one-way ANOVA."
      ))
    }
  }
  NULL
}

# The columns a role can use: any column for a grouping role, numeric
# columns otherwise.
column_choices <- function(data, type) {
  if (type == "group") return(names(data))
  names(data)[vapply(data, is.numeric, logical(1))]
}

# A first guess for a column role: the first factor or text column for a
# grouping role, otherwise the first numeric column not picked yet.
default_column <- function(data, type, taken = character()) {
  choices <- column_choices(data, type)
  if (type == "group") {
    levels <- vapply(data[choices], \(x) length(unique(stats::na.omit(x))),
                     integer(1))
    is_group <- levels >= 2L & levels <= 10L
    choices <- c(choices[is_group], choices[!is_group])
  }
  free <- setdiff(choices, taken)
  if (length(free) == 0L) choices[1L] else free[1L]
}

# Labels -----------------------------------------------------------------------

quantity_label <- function(q) {
  vapply(q, function(x) {
    base <- sub(":.*$", "", x)
    term <- if (grepl(":", x, fixed = TRUE)) sub("^[^:]+:", "", x)
    label <- app_store$text$quantities[[base]]$label %||% base
    if (is.null(term)) label else paste0(label, ", ", term)
  }, character(1), USE.NAMES = FALSE)
}

quantity_help <- function(q) {
  app_store$text$quantities[[q]]$help %||% ""
}

# Readable labels for variant codes: the app's own (tails, levels, test
# versions), then reprokit's conventions() labels, then the code itself.
variant_label <- function(codes) {
  own <- unlist(app_store$text$variant_labels)
  package <- reprokit::conventions()
  package <- stats::setNames(package$label, package$code)
  vapply(codes, function(code) {
    if (code %in% names(own)) return(own[[code]])
    if (code %in% names(package)) return(package[[code]])
    code
  }, character(1), USE.NAMES = FALSE)
}

# The conventions a handler accepts, as choices for a drop-down menu.
convention_choices <- function(handler, choice = "convention") {
  rows <- reprokit::conventions(handler)
  rows <- rows[rows$choice == choice, , drop = FALSE]
  stats::setNames(rows$code, rows$label)
}

# "p_from_t:wt; t_from_b_se:wt (needs se)" in words.
checked_with_words <- function(text) {
  if (is.na(text) || !nzchar(text)) return("—")
  relations <- app_store$text$relations
  parts <- strsplit(text, "; ", fixed = TRUE)[[1L]]
  words <- vapply(parts, function(part) {
    name <- sub("[: ].*$", "", part)
    rest <- substring(part, nchar(name) + 1L)
    paste0(relations[[name]] %||% gsub("_", " ", name), rest)
  }, character(1), USE.NAMES = FALSE)
  paste(words, collapse = "; ")
}

# The coverage-table row of a test, found by its label (coverage_table() has
# no handler column).
coverage_row <- function(id) {
  table <- reprokit::coverage_table()
  row <- table[table$test == test_entry(id)$coverage, , drop = FALSE]
  if (nrow(row) == 0L) NULL else row
}
