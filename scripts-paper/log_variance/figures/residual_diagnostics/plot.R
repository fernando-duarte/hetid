# Rendering for the two residual-diagnostic figures: the quantile-quantile panel
# against log chi^2_1 and the matching density panel. Both are published
# figures, so they follow the paper's figure standard rather than the diagnostic
# style of the per-estimator panels: svglite through ggsave (real <text> that
# \includesvg re-typesets, not baked path glyphs), theme_classic at base size 11
# with a thin panel border, an in-panel top-left legend, and no in-figure title,
# subtitle, or caption because the LaTeX caption and notes carry them. Canvases
# are the devices$residual_qq and devices$residual_density entries in
# config/figure_rendering.R. Definitions only; sourced by run.R.

LOGVAR_RESID_DIAG_QQ_X <- "Theoretical quantile, $\\log \\chi^2_1$"
LOGVAR_RESID_DIAG_XI <- "$\\log(\\varepsilon_t^2/\\hat\\mu_t)$"

# The shared paper theme for both panels. Matches the variance-bounds figure:
# thin border instead of axis lines, tick labels held off the frame, and a
# borderless legend pinned inside the top-left of the panel.
logvar_resid_diag_theme <- function() {
  ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.position = c(0.025, 0.975),
      legend.justification = c(0, 1),
      legend.direction = "vertical",
      legend.key.size = grid::unit(11, "pt"),
      legend.text = ggplot2::element_text(
        margin = ggplot2::margin(0, 6, 0, 3)
      ),
      panel.border = ggplot2::element_rect(
        colour = "black", fill = NA, linewidth = 1
      ),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        margin = ggplot2::margin(5, 0, 0, 0, unit = "pt")
      ),
      axis.text.y = ggplot2::element_text(
        margin = ggplot2::margin(0, 5, 0, 0, unit = "pt")
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 10, unit = "pt")
      )
    )
}

# Sorted empirical quantiles against the log chi^2_1 plotting positions, one
# block per estimator. The theoretical column repeats across estimators because
# every series has the same length, which is what puts them on one common axis.
logvar_resid_diag_qq_data <- function(series) {
  levels_ <- levels(series$estimator)
  blocks <- lapply(levels_, function(nm) {
    xi <- sort(series$xi[series$estimator == nm])
    data.frame(
      estimator = factor(nm, levels = levels_),
      theoretical = log(stats::qchisq(stats::ppoints(length(xi)), df = 1)),
      empirical = xi,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, blocks)
}

# Both axes take the same padded range so the reference line runs corner to
# corner: with equal limits and no expansion, y = x meets both corners of the
# frame exactly, and any departure from the reference reads as vertical distance
# rather than as an artefact of independently scaled axes.
logvar_resid_diag_qq_limits <- function(qq, pad_fraction) {
  span <- range(qq$theoretical, qq$empirical)
  span + c(-1, 1) * pad_fraction * diff(span)
}

logvar_resid_diag_qq_render <- function(series, path) {
  style <- PAPER_FIGURE_STYLE$residual_diagnostic
  qq <- logvar_resid_diag_qq_data(series)
  limits <- logvar_resid_diag_qq_limits(qq, style$qq_pad_fraction)
  fig <- ggplot2::ggplot(
    qq, ggplot2::aes(theoretical, empirical, colour = estimator)
  ) +
    ggplot2::geom_abline(
      slope = 1, intercept = 0, colour = style$reference,
      linewidth = style$reference_linewidth, linetype = style$reference_linetype
    ) +
    ggplot2::geom_point(size = style$point_size) +
    ggplot2::scale_colour_manual(values = style$estimator_colors) +
    ggplot2::coord_fixed(xlim = limits, ylim = limits, expand = FALSE) +
    ggplot2::labs(
      x = LOGVAR_RESID_DIAG_QQ_X,
      y = paste("Empirical quantile of", LOGVAR_RESID_DIAG_XI)
    ) +
    logvar_resid_diag_theme()
  device <- PAPER_FIGURE_RENDER_CONTROL$devices$residual_qq
  ggplot2::ggsave(
    path, fig,
    width = device[["width"]], height = device[["height"]]
  )
  invisible(path)
}

# Density of log(eps^2 / mu_hat) on the reference's own scale. The reference
# curve is the exact log chi^2_1 density, f(x) = sqrt(e^x / 2 pi) exp(-e^x / 2),
# evaluated on the plotted range rather than sampled, so the comparison carries
# no simulation noise of its own.
logvar_resid_diag_reference_density <- function(grid) {
  data.frame(
    x = grid,
    y = sqrt(exp(grid) / (2 * pi)) * exp(-exp(grid) / 2),
    stringsAsFactors = FALSE
  )
}

logvar_resid_diag_density_render <- function(series, path) {
  style <- PAPER_FIGURE_STYLE$residual_diagnostic
  span <- range(series$xi)
  grid <- seq(span[1L] - 1, span[2L] + 1, length.out = style$density_grid_n)
  reference <- logvar_resid_diag_reference_density(grid)
  fig <- ggplot2::ggplot(series, ggplot2::aes(xi, colour = estimator)) +
    ggplot2::geom_line(
      data = reference, ggplot2::aes(x, y), inherit.aes = FALSE,
      colour = style$reference, linewidth = style$reference_linewidth,
      linetype = style$reference_linetype
    ) +
    ggplot2::stat_density(
      geom = "line", position = "identity",
      adjust = style$density_adjust, linewidth = style$density_linewidth
    ) +
    ggplot2::scale_colour_manual(values = style$estimator_colors) +
    ggplot2::labs(x = LOGVAR_RESID_DIAG_XI, y = "Density") +
    logvar_resid_diag_theme()
  device <- PAPER_FIGURE_RENDER_CONTROL$devices$residual_density
  ggplot2::ggsave(
    path, fig,
    width = device[["width"]], height = device[["height"]]
  )
  invisible(path)
}
