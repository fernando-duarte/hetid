# One panel carrying every swept slack. The identified sets nest in tau, so the
# envelopes nest too: painting the widest band first and the narrowest last
# leaves each visible ring as "reachable at this tau but not at the next smaller
# one". Fills are opaque because overlaid transparency turns nested ribbons into
# a single smear, and they run dark to light with tau, so tightness of
# identification reads as depth of colour.
#
# The combined panel is a published figure, so it follows the paper's figure
# standard rather than the diagnostic style of the per-tau panels: svglite (real
# <text> that \includesvg re-typesets, not baked path glyphs), the 5.5 by
# 5.5/1.618 canvas, theme_classic at base size 11 with a thin panel border and an
# in-panel legend, and no in-figure title, subtitle, or caption because the LaTeX
# caption and notes carry them.

paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "tau_sweep_layout.R"
))

# envs: envelopes keyed in any order; log_scale puts the panel on a log y axis,
# where each band becomes half its log-variance width. That width is roughly
# flat over the sample, so the tight slacks stay legible instead of collapsing
# onto the point curve wherever the level is small.
logvar_tau_sweep_render <- function(envs, path, log_scale = FALSE) {
  taus <- vapply(envs, function(e) e$metadata$tau, numeric(1))
  envs <- envs[order(taus)]
  # unname: envs carries full-precision paper_tau_key names, and ggplot2 reads a
  # named breaks vector's names as the key labels
  taus <- unname(sort(taus))
  labels <- format(taus)
  palette <- grDevices::colorRampPalette(
    PAPER_FIGURE_STYLE$identified_set$sweep_ramp
  )(length(taus))
  bands <- logvar_tau_sweep_bands(envs, labels)
  widths <- vapply(bands, function(b) {
    stats::median(b$volatility_upper - b$volatility_lower)
  }, numeric(1))
  point <- logvar_fitted_vol_plot_data(envs[[1L]]$data)$point
  logvar_style <- PAPER_FIGURE_STYLE$log_variance
  fig <- ggplot2::ggplot(mapping = ggplot2::aes(date))
  # widest tau first so the narrower, darker sets end up on top
  for (i in rev(seq_along(bands))) {
    fig <- fig + ggplot2::geom_ribbon(
      data = bands[[i]],
      ggplot2::aes(
        ymin = volatility_lower, ymax = volatility_upper,
        group = band_group, fill = tau_label
      )
    )
  }
  fig <- fig +
    ggplot2::geom_line(
      data = point,
      ggplot2::aes(
        y = volatility_point, group = run,
        color = LOGVAR_TAU_SWEEP_POINT_KEY
      ),
      linewidth = logvar_style$point_linewidth
    ) +
    ggplot2::scale_color_manual(
      values = stats::setNames(
        logvar_style$point, LOGVAR_TAU_SWEEP_POINT_KEY
      ),
      name = NULL,
      # a wider key box for this guide alone, so the red segment reads as a line
      # rather than a dash; the band swatches keep the theme's 9pt key
      guide = ggplot2::guide_legend(
        nrow = 1, order = 1,
        theme = ggplot2::theme(legend.key.width = grid::unit(20, "pt"))
      )
    ) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(palette, labels),
      # layers are added widest-first, so pin the key order to increasing tau
      limits = labels, breaks = labels,
      labels = logvar_tau_sweep_key_labels(labels), name = NULL,
      guide = ggplot2::guide_legend(nrow = 1, order = 2)
    ) +
    ggplot2::scale_x_date(
      name = NULL,
      breaks = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "10 years"),
      labels = logvar_tau_sweep_date_labels
    ) +
    ggplot2::labs(y = logvar_tau_sweep_y_label(log_scale)) +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      # One row of five keys is wider than the panel even on the widened canvas,
      # so the legend sits above the frame instead of inside it. location =
      # "panel" centres it on the panel rather than on the plot, which the right
      # centring pad would otherwise pull it off by the width of the axis block.
      # The key spacing and text margins are trimmed to keep that row inside the
      # panel's width -- and the row only reads wider than it is, because svglite
      # reserves the raw "$\\tau=0.05$" source that \includesvg then typesets far
      # narrower.
      legend.position = "top",
      legend.location = "panel",
      legend.justification = "center",
      legend.direction = "horizontal",
      # the point-fit and band guides are separate scales; keep them on one line
      legend.box = "horizontal",
      legend.box.spacing = grid::unit(2, "pt"),
      legend.spacing.x = grid::unit(4, "pt"),
      legend.margin = ggplot2::margin(0, 0, 2, 0, unit = "pt"),
      # swatches sized to the key text rather than the theme's 1.2 lines
      legend.key.size = grid::unit(9, "pt"),
      legend.key.spacing.x = grid::unit(1, "pt"),
      legend.text = ggplot2::element_text(margin = ggplot2::margin(0, 1, 0, 1)),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, 0, 0, 0, unit = "pt")),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0, 5, 0, 0, unit = "pt")),
      # the default gap leaves the title almost touching the widest tick label;
      # the ticks are bare digits, so svglite reserves what \includesvg typesets
      # and this offset carries through to the compiled figure unchanged
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10, unit = "pt"))
    )
  if (log_scale) {
    fig <- fig + ggplot2::scale_y_log10()
  }
  device <- PAPER_FIGURE_RENDER_CONTROL$devices$fitted_volatility_sweep
  half_line <- 11 / 2
  fig <- fig + ggplot2::theme(
    plot.margin = ggplot2::margin(
      half_line,
      half_line + logvar_tau_sweep_center_pad(
        fig, device[["width"]], device[["height"]]
      ),
      half_line, half_line,
      unit = "pt"
    )
  )
  ggplot2::ggsave(
    path, fig,
    width = device[["width"]], height = device[["height"]]
  )
  widths
}
