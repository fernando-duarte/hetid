# Simple booktabs/threeparttable LaTeX table with plain (l c c ...) columns.
# Unlike build_panel_latex_table (siunitx S-columns for numeric panels), this
# left-aligns row labels and centers each data column, so cells that are not
# pure numbers -- e.g. identified-set interval strings "[lo, hi]" -- render
# cleanly. Reuses make_standalone_latex / write_latex_table from
# latex_table_utils.R for the standalone variant and file writing.

#' Build a plain-column booktabs/threeparttable LaTeX table fragment
#'
#' @param row_labels character vector of left-column row labels (may contain
#'   inline math)
#' @param columns list of character vectors (one per data column); each must
#'   have length(row_labels) entries, pre-formatted
#' @param col_headers character vector of column headers (length == length(columns))
#' @param caption table caption (LaTeX; pdfLaTeX-safe macros, not Unicode)
#' @param label LaTeX label key
#' @param notes character vector for the tablenotes block (NULL to omit);
#'   elements are concatenated into a single Notes item
#' @param stub header text over the row-label column (default empty)
#' @param rule_after integer row indices after which to insert a \\midrule
#'   (for visually grouping blocks of rows)
#' @return character vector of LaTeX lines (table environment fragment)
build_simple_latex_table <- function(row_labels, columns, col_headers,
                                     caption, label, notes = NULL,
                                     stub = "", rule_after = integer(0),
                                     fontsize = "", spanners = NULL) {
  n_col <- length(columns)
  n_row <- length(row_labels)
  stopifnot(
    length(col_headers) == n_col,
    all(vapply(columns, length, integer(1)) == n_row)
  )
  col_spec <- paste0("l", paste(rep("c", n_col), collapse = ""))
  header <- paste0(
    stub, " & ",
    paste(col_headers, collapse = " & "), " \\\\"
  )
  # Optional merged spanner header row over groups of data columns. Each spanner
  # is list(label = <LaTeX>, n = <#columns>); the n's must cover all data columns.
  spanner_lines <- NULL
  if (!is.null(spanners)) {
    ns <- vapply(spanners, function(s) s$n, integer(1))
    stopifnot(sum(ns) == n_col)
    mc <- vapply(
      spanners,
      function(s) sprintf("\\multicolumn{%d}{c}{%s}", s$n, s$label),
      character(1)
    )
    start <- 2L
    cmids <- character(0)
    for (n in ns) {
      cmids <- c(cmids, sprintf("\\cmidrule(lr){%d-%d}", start, start + n - 1L))
      start <- start + n
    }
    spanner_lines <- c(
      paste0(" & ", paste(mc, collapse = " & "), " \\\\"),
      paste(cmids, collapse = " ")
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
      body <- c(body, "\\midrule")
    }
  }
  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    "\\begin{threeparttable}",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    if (nzchar(fontsize)) fontsize else NULL,
    paste0("\\begin{tabular}{", col_spec, "}"),
    "\\toprule",
    spanner_lines,
    header,
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}"
  )
  if (!is.null(notes)) {
    lines <- c(
      lines,
      "\\begin{tablenotes}[flushleft]",
      "\\scriptsize",
      paste0("\\item Notes: ", paste(notes, collapse = " ")),
      "\\end{tablenotes}"
    )
  }
  c(lines, "\\end{threeparttable}", "\\end{table}")
}
