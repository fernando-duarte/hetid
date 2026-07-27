# Variance-bounds figure builder: U_i in levels against maturity in months,
# one series per reported bound (SDF news min, expected-SDF min).
# Pure (defining functions with no side effect on source) so the contract test
# can drive the reshape and the renderer with fixtures without touching the
# manifested output.

paper_source_once(paper_path("support", "graphics", "device.R"))
paper_source_once(paper_path("config", "reporting.R"))

variance_bounds_plot_data <- function(df) {
  stopifnot(
    is.data.frame(df), nrow(df) > 0L,
    all(c("Maturity", "Variance_Bound", "Expected_SDF_Bound") %in% names(df))
  )
  data.frame(
    Maturity = rep(df$Maturity, 2L),
    Series = factor(
      rep(c("SDF news", "Expected SDF"), each = nrow(df)),
      levels = c("SDF news", "Expected SDF")
    ),
    Value = c(df$Variance_Bound, df$Expected_SDF_Bound)
  )
}

# Vertical ticks in the summary table's plain-math scientific notation, so
# \includesvg re-typesets them and the figure and table read alike. Shares that
# table's precision, read as significant digits under "g" so a half-step break
# keeps its mantissa instead of rounding to a neighbouring tick's label. Zero
# prints bare rather than as a mantissa times a power.
variance_bounds_axis_labels <- function(breaks) {
  labels <- paper_format_sci(
    breaks,
    digits = PAPER_REPORTING_CONTROL$precision$variance_bound_sci,
    format = "g",
    na_token = ""
  )
  ifelse(!is.na(breaks) & breaks == 0, "$0$", labels)
}

# Horizontal ticks wrapped in math too, so both axes typeset through the same
# path and the figure carries no mixed SVG-font and LaTeX text.
variance_bounds_maturity_labels <- function(breaks) {
  ifelse(is.na(breaks), "", paste0("$", format(breaks, trim = TRUE), "$"))
}

variance_bounds_render_figure <- function(df, path) {
  long_df <- variance_bounds_plot_data(df)
  figure_style <- PAPER_FIGURE_STYLE$variance_bound
  fig <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = Maturity, y = Value, color = Series)
  ) +
    ggplot2::geom_line(linewidth = figure_style$line_width) +
    ggplot2::geom_point(size = figure_style$point_size) +
    ggplot2::scale_color_manual(values = figure_style$series_colors) +
    ggplot2::scale_y_continuous(labels = variance_bounds_axis_labels) +
    ggplot2::scale_x_continuous(labels = variance_bounds_maturity_labels) +
    ggplot2::labs(
      x = "Maturity (months)",
      y = "Variance Bound",
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
      # svglite reserves the tick column at the width of the raw LaTeX source
      # ("$4 \\times 10^{-9}$"), but \includesvg typesets that about half as
      # wide and the labels sit flush to the axis, so the surplus opens between
      # them and the title. Pull the title back over it, to the gap the x title
      # keeps from its own labels. Measured through the paper's svg/inkscape
      # path; revisit if the exponent or that font changes.
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = -33, unit = "pt"))
    )
  device <- PAPER_FIGURE_RENDER_CONTROL$devices$variance_bounds
  ggplot2::ggsave(
    path,
    fig,
    width = device[["width"]],
    height = device[["height"]]
  )
}
