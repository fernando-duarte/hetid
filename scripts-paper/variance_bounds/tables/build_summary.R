# Pure builder for the variance-bounds summary-statistics LaTeX table. A
# five-row (Mean/Median/Minimum/Maximum/SD) by two-column (SDF news /
# Expected SDF) booktabs fragment whose values render in plain-math
# scientific notation. Columns are extracted by name so a reordered summary
# cannot silently swap them. Sourced by the driver and by the contract test.

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))

variance_bounds_table_lines <- function(summary_matrix) {
  stopifnot(
    is.matrix(summary_matrix),
    is.numeric(summary_matrix),
    identical(dim(summary_matrix), c(5L, 2L)),
    identical(colnames(summary_matrix), c("SDF news", "Expected SDF")),
    !is.null(rownames(summary_matrix))
  )
  sci_cells <- function(col) {
    paper_format_sci(
      summary_matrix[, col],
      digits = PAPER_REPORTING_CONTROL$precision$variance_bound_sci,
      format = "e",
      na_token = PAPER_NA_TOKEN
    )
  }
  simple_tabular_lines(
    row_labels = rownames(summary_matrix),
    columns = list(sci_cells("SDF news"), sci_cells("Expected SDF")),
    col_headers = c("SDF news", "Expected SDF"),
    stub = "Statistic"
  )
}
