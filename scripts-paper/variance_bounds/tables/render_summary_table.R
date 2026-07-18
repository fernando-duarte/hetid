# Driver: build and publish the variance-bounds summary table through the
# shared manifest-directed LaTeX publisher.

paper_source_once(paper_path("variance_bounds", "tables", "build_summary.R"))

variance_bounds_table <- variance_bounds_table_lines(variance_bounds_summary)
publish_latex_artifact(
  "variance_bound_summary_table",
  variance_bounds_table
)
