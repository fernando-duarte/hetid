# Log-scale variance-bounds figure builder: log(U_i) against maturity in months.
# Pure (defining a function with no side effect on source) so the contract test can
# drive it with fixtures without touching the manifested output.

paper_source_once(paper_path("support", "graphics", "device.R"))

variance_bounds_render_figure <- function(df, path) {
  stopifnot(
    is.data.frame(df), nrow(df) > 0L,
    all(c("Maturity", "Variance_Bound") %in% names(df))
  )
  figure_style <- PAPER_FIGURE_STYLE$variance_bound
  fig <- ggplot2::ggplot(
    df, ggplot2::aes(x = Maturity, y = log(Variance_Bound))
  ) +
    ggplot2::geom_line(
      color = figure_style$line,
      linewidth = figure_style$line_width
    ) +
    ggplot2::geom_point(
      color = figure_style$point,
      size = figure_style$point_size
    ) +
    ggplot2::labs(
      title = "Log Variance Bounds Across Maturities",
      subtitle = "Log-scale visualization for trend analysis",
      x = "Maturity (months)",
      y = "Log(Variance Bound)"
    ) +
    ggplot2::theme_minimal()
  device <- PAPER_FIGURE_RENDER_CONTROL$devices$variance_bounds
  write_svg(
    path,
    device[["width"]],
    device[["height"]],
    function() print(fig)
  )
}
