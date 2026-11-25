# The number-entry form of the Check Paper tab, built from a test's
# `check` entry in text/tests.yml. Four kinds of block:
#
#   cells   one value per group or condition  (inputs cell_<q>_<i>)
#   matrix  one value per cell of a table     (inputs mat_<q>_<row>_<col>)
#   terms   one value per coefficient or path (inputs term_<q>_<j>)
#   single  one value                         (inputs num_<q>)
#
# plus test arguments (arg_mu, arg_k, arg_effect) and, where the size is up
# to the user, shape inputs (shape_cells, shape_rows, shape_cols, shape_terms).
# collect_form() turns the inputs back into check_reported() arguments. Only
# text is moved around here; numbers stay strings, as reprokit wants them.

# Shapes -----------------------------------------------------------------------

# A size in the catalogue is a number, or [min, max, default].
size_range <- function(size) {
  size <- unlist(size)
  if (length(size) == 1L) size else seq(size[[1L]], size[[2L]])
}

size_default <- function(size) {
  size <- unlist(size)
  if (length(size) == 1L) size else size[[3L]]
}

# The size of each block: from the filled-in values when there are any,
# else from the shape inputs, else the default.
form_shape <- function(spec, input, filled = NULL) {
  reported <- filled$reported
  pick <- function(size, id, from_fill) {
    fixed <- size_default(size)
    if (length(unlist(size)) == 1L) return(fixed)
    if (!is.null(from_fill) && length(from_fill) == 1L) return(from_fill)
    chosen <- suppressWarnings(as.integer(input[[id]]))
    if (length(chosen) == 1L && chosen %in% size_range(size)) chosen else fixed
  }
  first_of <- function(quantities, measure) {
    present <- Filter(Negate(is.null), reported[unlist(quantities)])
    if (length(present) > 0L) measure(present[[1L]])
  }
  shape <- list()
  if (!is.null(spec$cells)) {
    from_fill <- first_of(spec$cells$quantities, length)
    shape$cells <- pick(spec$cells$count, "shape_cells", from_fill)
  }
  if (!is.null(spec$matrix)) {
    shape$rows <- pick(spec$matrix$rows, "shape_rows",
                       first_of(spec$matrix$quantities, nrow))
    shape$cols <- pick(spec$matrix$cols, "shape_cols",
                       first_of(spec$matrix$quantities, ncol))
  }
  if (!is.null(spec$terms)) {
    from_fill <- first_of(spec$terms$quantities, length)
    shape$terms <- pick(spec$terms$count, "shape_terms", from_fill)
  }
  shape
}

# Rendering --------------------------------------------------------------------

# `value_for(id, from_fill, default)` gives a field's starting value: the
# filled-in one, or what the field holds now.
check_form_ui <- function(ns, spec, shape, value_for) {
  tagList(
    shape_inputs(ns, spec, shape),
    if (!is.null(spec$cells)) cells_block(ns, spec$cells, shape, value_for),
    if (!is.null(spec$matrix)) matrix_block(ns, spec$matrix, shape, value_for),
    if (!is.null(spec$terms)) terms_block(ns, spec$terms, shape, value_for),
    single_block(ns, unlist(spec$single), value_for),
    args_block(ns, unlist(spec$args), value_for)
  )
}

shape_inputs <- function(ns, spec, shape) {
  one <- function(id, size, label) {
    if (length(unlist(size)) != 3L) return(NULL)
    selectInput(ns(id), label, choices = size_range(size),
                selected = shape[[sub("shape_", "", id)]], width = "100%")
  }
  is_counts <- identical(unlist(spec$matrix$quantities), "n") ||
    "p0" %in% unlist(spec$cells$quantities)
  inputs <- list(
    one("shape_cells", spec$cells$count,
        txt("check", "shape", if (is_counts) "categories" else "groups")),
    one("shape_rows", spec$matrix$rows,
        txt("check", "shape", if (is_counts) "table_rows" else "rows")),
    one("shape_cols", spec$matrix$cols,
        txt("check", "shape", if (is_counts) "table_cols" else "cols")),
    one("shape_terms", spec$terms$count, txt("check", "shape", "terms"))
  )
  inputs <- Filter(Negate(is.null), inputs)
  if (length(inputs) == 0L) return(NULL)
  do.call(layout_column_wrap, c(list(width = "14rem"), inputs))
}

cell_names <- function(block, count) {
  names <- unlist(block$names)
  if (length(names) == 1L && count > 1L) {
    return(paste(names, seq_len(count)))
  }
  rep_len(names, count)
}

field <- function(ns, id, value, label = NULL) {
  textInput(ns(id), label, value = value, width = "100%")
}

quantity_header <- function(q) {
  tags$th(label_with_help(quantity_label(q), quantity_help(q)))
}

cells_block <- function(ns, block, shape, value_for) {
  k <- shape$cells
  quantities <- unlist(block$quantities)
  title <- if (k > 1L) "cells_title" else "cells_title_one"
  div(
    class = "form-block",
    tags$h6(txt("check", title)),
    div(
      class = "table-responsive",
      tags$table(
        class = "table table-sm align-middle entry-table",
        tags$thead(tags$tr(tags$th(""), lapply(cell_names(block, k), tags$th))),
        tags$tbody(lapply(quantities, function(q) {
          tags$tr(
            quantity_header(q),
            lapply(seq_len(k), function(i) {
              id <- paste0("cell_", q, "_", i)
              start <- value_for(id, \(f) nth(f$reported[[q]], i))
              tags$td(field(ns, id, start))
            })
          )
        }))
      )
    )
  )
}

matrix_block <- function(ns, block, shape, value_for) {
  rows <- seq_len(shape$rows)
  cols <- seq_len(shape$cols)
  row_label <- block$row_label %||% "Row "
  col_label <- block$col_label %||% "Column "
  grids <- lapply(unlist(block$quantities), function(q) {
    div(
      class = "form-block",
      tags$h6(label_with_help(quantity_label(q), quantity_help(q))),
      tags$table(
        class = "table table-sm align-middle entry-table",
        tags$thead(tags$tr(
          tags$th(""),
          lapply(cols, \(j) tags$th(paste0(col_label, j)))
        )),
        tags$tbody(lapply(rows, function(i) {
          tags$tr(
            tags$th(paste0(row_label, i)),
            lapply(cols, function(j) {
              id <- paste0("mat_", q, "_", i, "_", j)
              start <- value_for(id, \(f) cell_of(f$reported[[q]], i, j))
              tags$td(field(ns, id, start))
            })
          )
        }))
      )
    )
  })
  do.call(layout_column_wrap, c(list(width = "15rem"), grids))
}

terms_block <- function(ns, block, shape, value_for) {
  quantities <- unlist(block$quantities)
  count <- shape$terms
  defaults <- rep_len(unlist(block$names), count)
  labels <- unlist(block$labels) %||% defaults
  fixed <- isTRUE(block$fixed_names)
  div(
    class = "form-block",
    tags$h6(txt("check", "terms_title")),
    div(
      class = "table-responsive",
      tags$table(
        class = "table table-sm align-middle entry-table",
        tags$thead(tags$tr(
          tags$th(txt("check", "term_name")),
          lapply(quantities, quantity_header)
        )),
        tags$tbody(lapply(seq_len(count), function(j) {
          name_id <- paste0("term_name_", j)
          name_cell <- if (fixed) {
            tags$th(labels[[j]])
          } else {
            tags$td(field(
              ns, name_id,
              value_for(name_id, \(f) nth(term_names(f$reported), j),
                        default = defaults[[j]])
            ))
          }
          tags$tr(
            name_cell,
            lapply(quantities, function(q) {
              id <- paste0("term_", q, "_", j)
              start <- value_for(id, function(f) {
                term_value(f$reported[[q]], defaults[[j]], j, fixed)
              })
              tags$td(field(ns, id, start))
            })
          )
        }))
      )
    )
  )
}

single_block <- function(ns, quantities, value_for) {
  if (length(quantities) == 0L) return(NULL)
  fields <- lapply(quantities, function(q) {
    id <- paste0("num_", q)
    field(
      ns, id, value_for(id, \(f) f$reported[[q]]),
      label = label_with_help(quantity_label(q), quantity_help(q))
    )
  })
  div(
    class = "form-block",
    tags$h6(txt("check", "results_title")),
    do.call(layout_column_wrap, c(list(width = "10rem"), fields))
  )
}

args_block <- function(ns, args, value_for) {
  inputs <- lapply(args, function(arg) {
    switch(
      arg,
      mu = numericInput(
        ns("arg_mu"),
        label_with_help(txt("check", "mu_label"), txt("check", "mu_help")),
        value = value_for("arg_mu", \(f) f$args$mu, default = 0)
      ),
      k = numericInput(
        ns("arg_k"),
        label_with_help(txt("check", "k_label"), txt("check", "k_help")),
        value = value_for("arg_k", \(f) f$args$k, default = NA),
        min = 1, step = 1
      ),
      effect = selectInput(
        ns("arg_effect"),
        label_with_help(txt("check", "effect_label"),
                        txt("check", "effect_help")),
        choices = invert(txt("check", "effect_choices")),
        selected = value_for("arg_effect", \(f) f$args$effect, default = "A")
      )
    )
  })
  if (length(inputs) == 0L) return(NULL)
  do.call(layout_column_wrap, c(list(width = "16rem"), inputs))
}

# Small helpers ----------------------------------------------------------------

nth <- function(x, i) if (length(x) >= i) unname(x)[[i]]

cell_of <- function(x, i, j) {
  if (is.matrix(x) && i <= nrow(x) && j <= ncol(x)) x[i, j]
}

term_names <- function(reported) {
  named <- Filter(\(x) !is.null(names(x)), reported)
  if (length(named) > 0L) names(named[[1L]])
}

# A coefficient's value: by name for fixed paths (a, b, c, c_prime), by
# position otherwise.
term_value <- function(x, name, j, fixed) {
  if (fixed) return(if (name %in% names(x)) x[[name]])
  nth(x, j)
}

# list(code = "label") as c(label = "code"), for selectInput().
invert <- function(choices) {
  stats::setNames(names(choices), unlist(choices))
}

# Collecting -------------------------------------------------------------------

# The form's inputs as check_reported() arguments: list(reported, args,
# problem). `problem` is a sentence the user can act on, or NULL.
collect_form <- function(input, spec, shape) {
  reported <- list()
  args <- list()
  problem <- function(text) list(reported = NULL, args = NULL, problem = text)
  value <- function(id) trimws(input[[id]] %||% "")

  for (q in unlist(spec$cells$quantities)) {
    values <- vapply(seq_len(shape$cells),
                     \(i) value(paste0("cell_", q, "_", i)), character(1))
    if (!any(nzchar(values))) next
    if (!all(nzchar(values))) {
      return(problem(fill_in(txt("check", "need_all_cells"),
                             label = quantity_label(q))))
    }
    if (q == "p0") {
      args$p0 <- suppressWarnings(as.numeric(values))
    } else {
      reported[[q]] <- values
    }
  }

  for (q in unlist(spec$matrix$quantities)) {
    ids <- outer(seq_len(shape$rows), seq_len(shape$cols),
                 \(i, j) paste0("mat_", q, "_", i, "_", j))
    values <- matrix(vapply(ids, value, character(1)), nrow = shape$rows)
    if (!any(nzchar(values))) next
    if (!all(nzchar(values))) {
      return(problem(fill_in(txt("check", "need_all_cells"),
                             label = quantity_label(q))))
    }
    reported[[q]] <- values
  }

  if (!is.null(spec$terms)) {
    count <- shape$terms
    fixed <- isTRUE(spec$terms$fixed_names)
    names <- if (fixed) {
      unlist(spec$terms$names)[seq_len(count)]
    } else {
      vapply(seq_len(count), \(j) value(paste0("term_name_", j)), character(1))
    }
    for (q in unlist(spec$terms$quantities)) {
      values <- vapply(seq_len(count),
                       \(j) value(paste0("term_", q, "_", j)), character(1))
      if (!any(nzchar(values))) next
      if (!all(nzchar(names[nzchar(values)]))) {
        return(problem(txt("check", "need_term_names")))
      }
      reported[[q]] <- stats::setNames(values, names)[nzchar(values)]
    }
  }

  for (q in unlist(spec$single)) {
    if (nzchar(value(paste0("num_", q)))) {
      reported[[q]] <- value(paste0("num_", q))
    }
  }

  for (arg in unlist(spec$args)) {
    raw <- input[[paste0("arg_", arg)]]
    if (arg == "mu") args$mu <- if (isTRUE(is.finite(raw))) raw else 0
    if (arg == "k" && isTRUE(is.finite(raw))) args$k <- as.integer(raw)
    if (arg == "effect") args$effect <- raw %||% "A"
  }
  if ("p0" %in% unlist(spec$cells$quantities) && !is.null(reported$n)) {
    args$dims <- length(reported$n)
  }

  list(reported = reported, args = args, problem = NULL)
}

# A practice example from YAML (text/examples.yml, practice/*.yml) in the
# form check_reported() takes: a list of rows becomes a matrix, a mapping a
# named vector.
parse_reported <- function(raw) {
  lapply(raw, function(value) {
    if (is.list(value) && is.null(names(value))) {
      return(do.call(rbind, lapply(value, unlist)))
    }
    unlist(value)
  })
}
