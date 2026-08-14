# One panel carrying every swept slack. The identified sets nest in tau, so the
# envelopes nest too: painting the widest band first and the narrowest last
# leaves each visible ring as "reachable at this tau but not at the next smaller
# one". Fills are opaque because overlaid transparency turns nested ribbons into
# a single smear, and they run dark to light with tau, so tightness of
# identification reads as depth of colour.
#
# The combined panel is a published figure, so it follows the paper's figure
# standard rather than the diagnostic style of the per-tau panels: svglite (real
# <text> that \includesvg re-typesets, not baked path glyphs), theme_classic at
# base size 11 with a thin panel border, a legend above the frame, and no
# in-figure title, subtitle, or caption because the LaTeX caption and notes
# carry them. The canvas is the devices$fitted_volatility_sweep entry in
# config/figure_rendering.R, which is also where its width is argued; naming the
# size here as well only gives it somewhere to go stale, which it has done twice.

paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "tau_sweep_layout.R"
))

# envs: envelopes keyed in any order; log_variance picks which of the two
# families the panel carries -- the exponent PC_R,t' theta_R itself, or its
# exponential. Both on a linear axis: the exponent panel is what a log axis on
# the exponential one used to be, so transforming the scale as well would only
# reproduce the sibling.
#
# extra_line: optional data.frame(date, value) drawn as one more line and given
# its own key in the point-fit scale. Used for the OLS benchmark path, which is
# a single fit rather than an envelope and so has no band of its own.
logvar_tau_sweep_render <- function(envs, path, log_variance = FALSE,
                                    extra_line = NULL) {
  prefix <- if (log_variance) "log_variance_" else "variance_"
  lo <- paste0(prefix, "lower")
  hi <- paste0(prefix, "upper")
  pt <- paste0(prefix, "point")
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
    stats::median(b[[hi]] - b[[lo]])
  }, numeric(1))
  point <- logvar_fitted_vol_plot_data(envs[[1L]]$data)$point
  logvar_style <- PAPER_FIGURE_STYLE$log_variance
  fig <- ggplot2::ggplot(mapping = ggplot2::aes(date))
  # widest tau first so the narrower, darker sets end up on top
  for (i in rev(seq_along(bands))) {
    fig <- fig + ggplot2::geom_ribbon(
      data = bands[[i]],
      ggplot2::aes(
        ymin = .data[[lo]], ymax = .data[[hi]],
        group = band_group, fill = tau_label
      )
    )
  }
  fig <- fig +
    ggplot2::geom_line(
      data = point,
      ggplot2::aes(
        y = .data[[pt]], group = run,
        color = LOGVAR_TAU_SWEEP_POINT_KEY
      ),
      linewidth = logvar_style$point_linewidth
    )
  line_keys <- LOGVAR_TAU_SWEEP_POINT_KEY
  line_colours <- logvar_style$point
  if (!is.null(extra_line)) {
    stopifnot(all(c("date", "value") %in% names(extra_line)))
    fig <- fig + ggplot2::geom_line(
      data = extra_line,
      ggplot2::aes(y = value, color = LOGVAR_TAU_SWEEP_OLS_KEY),
      linewidth = logvar_style$point_linewidth
    )
    line_keys <- c(line_keys, LOGVAR_TAU_SWEEP_OLS_KEY)
    line_colours <- c(line_colours, PAPER_FIGURE_STYLE$region$ols_point)
  }
  fig <- fig +
    ggplot2::scale_color_manual(
      values = stats::setNames(line_colours, line_keys),
      limits = line_keys, breaks = line_keys,
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
    ggplot2::labs(y = logvar_tau_sweep_y_label(log_variance)) +
    ggplot2::theme_classic(base_size = 11) +
    logvar_tau_sweep_theme()
  device <- PAPER_FIGURE_RENDER_CONTROL$devices$fitted_volatility_sweep
  fig <- logvar_tau_sweep_center(fig, device)
  ggplot2::ggsave(
    path, fig,
    width = device[["width"]], height = device[["height"]]
  )
  widths
}
