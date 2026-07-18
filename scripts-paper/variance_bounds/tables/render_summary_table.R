# Driver: build and write the variance-bounds summary table fragment plus its
# standalone document, then compile the standalone PDF. Mirrors the write/compile
# tail of render_variance_share_table.R.

paper_source_once(paper_path("variance_bounds", "tables", "build_summary.R"))

variance_bounds_table <- variance_bounds_table_lines(variance_bounds_summary)
write_latex_table(
  variance_bounds_table,
  artifact_dir("variance_bound_summary_table"),
  tools::file_path_sans_ext(artifact_basename("variance_bound_summary_table"))
)
compile_latex_pdf(artifact_path("variance_bound_summary_standalone_tex"))
