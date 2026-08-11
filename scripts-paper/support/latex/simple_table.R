# Simple booktabs/threeparttable LaTeX table with plain (l c c ...) columns.
# Unlike panel_tabular_lines (bold panel rows, column-group spanner), this is
# a flat rows-by-columns layout; cells that are not pure numbers -- e.g.
# identified-set interval strings "[lo, hi]" -- render cleanly. Reuses
# make_standalone_latex / publish_latex_artifact from table_pipeline.R for
# the standalone variant and manifest-directed writing, and the rule sets and
# spanner builder from overleaf_scaffold.R.

paper_source_once(paper_path("support", "latex", "overleaf_scaffold.R"))

#' Build the bare booktabs tabular for a plain-column table
#'
#' The tabular half of build_simple_latex_table, without any
#' float/threeparttable/caption/notes wrapper, so a caller can publish a
#' fragment that is only \\begin{tabular} ... \\end{tabular} (or wrap it itself).
#'
#' @param row_labels character vector of left-column row labels (may contain
#'   inline math)
#' @param columns list of character vectors (one per data column); each must
#'   have length(row_labels) entries, pre-formatted
#' @param col_headers character vector of column headers (length == length(columns))
#' @param stub header text over the row-label column (default empty)
#' @param rule_after integer row indices after which to insert a separator
#'   (for visually grouping blocks of rows)
#' @param spanners optional list of list(label, n) merged headers over column
#'   groups; the n's must cover all data columns
#' @param rules rule set for the opening, header, and closing rules and the
#'   spanner segment style (see PAPER_BOOKTABS_RULES)
#' @param separator separator emitted at each rule_after index, recycled to its
#'   length; the kern scaffold uses \\addlinespace between blocks and a light
#'   rule above the tail, so the two cannot share one literal
#' @return character vector of LaTeX lines from \\begin{tabular} to \\end{tabular}
simple_tabular_lines <- function(row_labels, columns, col_headers,
                                 stub = "", rule_after = integer(0),
                                 spanners = NULL,
                                 rules = PAPER_BOOKTABS_RULES,
                                 separator = "\\midrule") {
  n_col <- length(columns)
  n_row <- length(row_labels)
  separator <- rep_len(separator, max(length(rule_after), 1L))
  stopifnot(
    length(col_headers) == n_col,
    all(vapply(columns, length, integer(1)) == n_row)
  )
  col_spec <- paste0("l", paste(rep("c", n_col), collapse = ""))
  header <- paste0(
    stub, " & ",
    paste(col_headers, collapse = " & "), " \\\\"
  )
  # Optional merged spanner header row over groups of data columns, built by the
  # shared header builder so the segment ranges match the kern tables' exactly.
  header_lines <- if (is.null(spanners)) {
    header
  } else {
    c(
      paper_overleaf_column_headers(
        spanners, col_headers, rules$span_prefix, rules$span_join
      )[1:2],
      header
    )
  }
  body <- character(0)
  for (i in seq_len(n_row)) {
    cells <- vapply(columns, function(col) col[[i]], character(1))
    body <- c(
      body,
      paste0(row_labels[[i]], " & ", paste(cells, collapse = " & "), " \\\\")
    )
    if (i %in% rule_after) {
      body <- c(body, separator[[match(i, rule_after)]])
    }
  }
  c(
    paste0("\\begin{tabular}{", col_spec, "}"),
    rules$top,
    header_lines,
    rules$header,
    body,
    rules$bottom,
    "\\end{tabular}"
  )
}

#' Build a plain-column booktabs/threeparttable LaTeX table fragment
#'
#' Wraps simple_tabular_lines() in the shared float/threeparttable/caption/notes
#' environment. Callers wanting only the tabular call simple_tabular_lines().
#'
#' @inheritParams simple_tabular_lines
#' @param caption table caption (LaTeX; pdfLaTeX-safe macros, not Unicode)
#' @param label LaTeX label key
#' @param notes character vector for the tablenotes block (NULL to omit);
#'   elements are concatenated into a single Notes item
#' @param fontsize optional size command emitted before the tabular
#' @return character vector of LaTeX lines (table environment fragment)
build_simple_latex_table <- function(row_labels, columns, col_headers,
                                     caption, label, notes = NULL,
                                     stub = "", rule_after = integer(0),
                                     fontsize = "", spanners = NULL) {
  latex_table_environment(
    tabular_lines = simple_tabular_lines(
      row_labels, columns, col_headers, stub, rule_after, spanners
    ),
    caption = caption,
    label = label,
    notes = notes,
    notes_label = PAPER_TABLE_NOTES_LABEL,
    fontsize = fontsize
  )
}
