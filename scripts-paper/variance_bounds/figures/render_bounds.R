# Driver: render the log variance-bounds figure to its manifested SVG artifact.

source(paper_path("variance_bounds", "figures", "plot.R"))

variance_bounds_render_figure(
  variance_bounds_df,
  artifact_path("variance_bound_figure")
)
