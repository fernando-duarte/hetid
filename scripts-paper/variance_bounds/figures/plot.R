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
      x = "Maturity (months)",
      y = "Log(Variance Bound)",
      color = NULL
    ) +
    # Match the paper's svglite / theme_classic figure standard: no in-figure
    # title (the LaTeX \caption supplies it), base font 11, white panel with a
    # thin border, and a top-left in-panel legend, as in the macro_dynamics
    # sibling figures. Rendered through svglite (below) so the text stays real
    # <text> that LaTeX's \includesvg can re-typeset, not baked path glyphs.
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.position = c(0.025, 0.975),
      legend.justification = c(0, 1),
      legend.direction = "vertical",
      legend.text = ggplot2::element_text(margin = ggplot2::margin(0, 6, 0, 3)),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, 0, 0, 0, unit = "pt")),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0, 5, 0, 0, unit = "pt")),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8, unit = "pt"))
    )
  device <- PAPER_FIGURE_RENDER_CONTROL$devices$variance_bounds
  ggplot2::ggsave(
    path,
    fig,
    width = device[["width"]],
    height = device[["height"]]
  )
}
