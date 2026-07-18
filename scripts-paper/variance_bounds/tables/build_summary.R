# Pure builder for the variance-bounds summary-statistics LaTeX table. A five-row
# (Mean/Median/Minimum/Maximum/SD) booktabs fragment whose values render in siunitx
# scientific notation. Sourced by the driver and by the contract test.

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))

variance_bounds_table_lines <- function(summary_stats) {
  stopifnot(is.numeric(summary_stats), length(summary_stats) == 5L)
  fmt_sci <- function(x) ifelse(is.finite(x), sprintf("\\num{%.2e}", x), "--")
  build_simple_latex_table(
    row_labels = names(summary_stats),
    columns = list(vapply(summary_stats, fmt_sci, character(1))),
    col_headers = "Value",
    stub = "Statistic",
    caption = paste(
      "Variance Bounds Summary Statistics.",
      "Distributional properties across maturities."
    ),
    label = artifact_latex_label("variance_bound_summary_table"),
    notes = paste(
      "$U_i = \\frac{1}{4}\\,\\hat c_i(\\hat k_i + \\hat k_{2,i})$ is the",
      "SDF-news approximation-error variance bound, summarized across the",
      "quarterly maturity grid."
    )
  )
}
