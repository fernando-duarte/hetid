# Log-scale variance-bounds figure builder: log(U_i) against maturity in
# months, one series per reported bound (SDF news min, expected-SDF min).
# Pure (defining functions with no side effect on source) so the contract test
# can drive the reshape and the renderer with fixtures without touching the
# manifested output.

paper_source_once(paper_path("support", "graphics", "device.R"))

variance_bounds_plot_data <- function(df) {
  stopifnot(
    is.data.frame(df), nrow(df) > 0L,
    all(c("Maturity", "Variance_Bound", "Expected_SDF_Bound") %in% names(df))
  )
  data.frame(
    Maturity = rep(df$Maturity, 2L),
    Series = factor(
      rep(c("SDF news", "expected SDF"), each = nrow(df)),
      levels = c("SDF news", "expected SDF")
    ),
    Value = c(df$Variance_Bound, df$Expected_SDF_Bound)
  )
}

variance_bounds_render_figure <- function(df, path) {
  long_df <- variance_bounds_plot_data(df)
  figure_style <- PAPER_FIGURE_STYLE$variance_bound
  fig <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = Maturity, y = log(Value), color = Series)
  ) +
    ggplot2::geom_line(linewidth = figure_style$line_width) +
    ggplot2::geom_point(size = figure_style$point_size) +
    ggplot2::scale_color_manual(values = figure_style$series_colors) +
    ggplot2::labs(
      title = "Log Variance Bounds Across Maturities",
      subtitle = "Log-scale visualization for trend analysis",
      x = "Maturity (months)",
      y = "Log(Variance Bound)",
      color = NULL
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
