# One panel of the paper's inference tables, in the V-FCI-Overleaf layout: a
# centered panel title, the two-tier column header repeated under it, the
# coefficient body with open space between coefficient blocks, and a light rule
# above the R^2/N tail. Shared by the combined mean-over-variance table, the
# per-estimator pages, and the Panel-B-only fragments, so those cannot drift.

paper_source_once(paper_path("support", "latex", "overleaf_scaffold.R"))
paper_source_once(paper_path("config", "reporting.R"))

# The inference tables are set at the coefficient tables' size, which is the
# contract's own value and what the paper uses. PAPER_TABLE_STYLE's
# combined_inference entry is a step smaller and is superseded; it is left in
# place rather than corrected because its file is one of
# BOOTSTRAP_STAGE_CODE_FILES, and editing it to change a font size would digest
# differently and discard the cached bootstrap draws.
PAPER_OVERLEAF_INFERENCE_FONTSIZE <- PAPER_TABLE_STYLE$coefficient$fontsize

# The point columns (the estimator's reference column and the tau = 0 Lewbel
# point) are set off from the identified-set columns by a wider gap.
PAPER_OVERLEAF_POINT_COLS <- 2L

paper_overleaf_panel_colspec <- function(n_col) {
  stopifnot(n_col > PAPER_OVERLEAF_POINT_COLS)
  paste0(
    "l",
    strrep("c", PAPER_OVERLEAF_POINT_COLS),
    "@{\\hskip 12pt}",
    strrep("c", n_col - PAPER_OVERLEAF_POINT_COLS)
  )
}

#' Lines for one titled panel
#'
#' @param title panel title, e.g. "Panel A: Mean equation"; NULL for a
#'   single-panel table, which opens straight into its column header
#' @param row_labels character vector of left-column labels
#' @param columns list of character vectors, one per data column
#' @param headers per-column headers (OLS, tau = 0, then one per slack)
#' @param set_label spanner label over the identified-set columns; the panels
#'   that report a bootstrap envelope beneath each set cell say so, and the ones
#'   that print bare ranges do not
#' @param blocks row indices after which to open vertical space
#' @param tail_after row index after which the light rule above R^2/N goes
paper_overleaf_panel_lines <- function(title, row_labels, columns, headers,
                                       set_label, blocks, tail_after) {
  n_col <- length(columns)
  stopifnot(
    length(headers) == n_col,
    all(vapply(columns, length, integer(1)) == length(row_labels)),
    tail_after <= length(row_labels)
  )
  body <- character(0)
  for (i in seq_along(row_labels)) {
    cells <- vapply(columns, function(col) col[[i]], character(1))
    body <- c(
      body,
      paste0(row_labels[[i]], " & ", paste(cells, collapse = " & "), " \\\\")
    )
    if (i %in% blocks) body <- c(body, "\\addlinespace[2.5pt]")
    if (i == tail_after) body <- c(body, paper_overleaf_inner_rule(n_col))
  }
  c(
    if (!is.null(title)) {
      c(
        paper_overleaf_panel_head(n_col, title),
        paper_overleaf_inner_rule(n_col)
      )
    },
    paper_overleaf_column_headers(
      list(
        list(
          label = "Estimate (test statistic)",
          n = PAPER_OVERLEAF_POINT_COLS
        ),
        list(label = set_label, n = n_col - PAPER_OVERLEAF_POINT_COLS)
      ),
      headers
    ),
    paper_overleaf_inner_rule(n_col),
    body
  )
}

# The two spanner labels in use. A panel whose tau > 0 cells carry a bootstrap
# confidence envelope beneath them names it; the mean-log benchmark prints bare
# ranges and does not.
PAPER_OVERLEAF_SET_LABEL <- "Identified set (confidence envelope)"
PAPER_OVERLEAF_SET_LABEL_BARE <- "Identified set"

# Drop the R^2 row from a panel's parts. An estimator without an R-squared
# prints a row of dashes for it in the combined table, where the mean panel
# above supplies a real one to line up against; a standalone variance-equation
# table has nothing to line it up against, and the paper drops the row. The
# position is asserted against both the label and the cells, so a reshaped row
# set stops the build rather than deleting a live row.
paper_overleaf_drop_r2 <- function(parts) {
  i <- length(parts$rows) - 1L
  stopifnot(
    identical(parts$rows[[i]], "$R^2$"),
    all(vapply(
      parts$columns,
      function(col) identical(col[[i]], PAPER_NA_TOKEN),
      logical(1)
    ))
  )
  parts$rows <- parts$rows[-i]
  parts$columns <- lapply(parts$columns, function(col) col[-i])
  parts
}

#' Assemble titled panels into one kern-scaffolded tabular
#'
#' @param panels list of character vectors from paper_overleaf_panel_lines
#' @param n_col number of data columns
paper_overleaf_panel_table <- function(panels, n_col) {
  separated <- panels[[1L]]
  for (panel in panels[-1L]) {
    separated <- c(
      separated,
      paste0(paper_overleaf_panel_rule(n_col), "\\addlinespace[2.5pt]"),
      panel
    )
  }
  paper_overleaf_kern_group(
    c(
      paste0("\\begin{tabular}{", paper_overleaf_panel_colspec(n_col), "}"),
      paper_overleaf_outer_rule(n_col),
      separated,
      paper_overleaf_outer_rule(n_col),
      "\\end{tabular}"
    ),
    PAPER_OVERLEAF_INFERENCE_FONTSIZE,
    panel_rule = length(panels) > 1L
  )
}
