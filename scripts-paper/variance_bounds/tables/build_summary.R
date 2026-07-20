# Pure builder for the variance-bounds summary-statistics LaTeX table. A five-row
# (Mean/Median/Minimum/Maximum/SD) booktabs fragment whose values render in
# plain-math scientific notation. Sourced by the driver and by the contract test.

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))

variance_bounds_table_lines <- function(summary_stats) {
  stopifnot(is.numeric(summary_stats), length(summary_stats) == 5L)
  cells <- paper_format_sci(
    summary_stats,
    digits = PAPER_REPORTING_CONTROL$precision$variance_bound_sci,
    format = "e",
    na_token = PAPER_NA_TOKEN
  )
  simple_tabular_lines(
    row_labels = names(summary_stats),
    columns = list(cells),
    col_headers = "Value",
    stub = "Statistic"
  )
}
