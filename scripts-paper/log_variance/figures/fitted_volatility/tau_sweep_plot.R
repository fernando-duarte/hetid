# One panel carrying every swept slack. The identified sets nest in tau, so the
# envelopes nest too: painting the widest band first and the narrowest last
# leaves each visible ring as "reachable at this tau but not at the next smaller
# one". Fills are opaque because overlaid transparency turns nested ribbons into
# a single smear, and they run dark to light with tau, so tightness of
# identification reads as depth of colour.

paper_source_once(paper_path("support", "graphics", "device.R"))
paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "plot.R"
))

logvar_tau_sweep_bands <- function(envs, labels) {
  lapply(seq_along(envs), function(i) {
    band <- logvar_fitted_vol_plot_data(envs[[i]]$data)$band
    stopifnot(nrow(band) > 0L)
    band$tau_label <- factor(labels[i], levels = labels)
    band$band_group <- paste(labels[i], band$run, sep = ":")
    band
  })
}

logvar_tau_sweep_caption <- function(labels, widths) {
  paste0(
    "Shading is the pointwise projection hull of the estimated plug-in ",
    "variance-equation image at each slack tau.\nThe bands nest because the ",
    "identified set grows with tau; the darkest band is the tightest slack. ",
    "The red line is the\ntau = 0 Lewbel-point fit. Finite plotted endpoints ",
    "are attained inner approximations from grid scan and local\npolish. ",
    "These are not confidence or simultaneous path bands; interior attainment ",
    "is not asserted.\nMedian band width by tau: ",
    paste(sprintf("%s = %.4f pp", labels, widths), collapse = ", "), "."
  )
}

# envs: envelopes keyed in any order; log_scale puts the panel on a log y axis,
# where each band becomes half its log-variance width. That width is roughly
# flat over the sample, so the tight slacks stay legible instead of collapsing
# onto the point curve wherever the level is small.
logvar_tau_sweep_render <- function(envs, estimator, path, log_scale = FALSE) {
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
  estimator_spec <- PAPER_LOGVAR_ESTIMATORS[[estimator]]
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
      data = point, ggplot2::aes(y = volatility_point, group = run),
      color = logvar_style$point, linewidth = logvar_style$point_linewidth
    ) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(palette, labels),
      # layers are added widest-first, so pin the key order to increasing tau
      limits = labels, breaks = labels, name = expression(tau)
    ) +
    ggplot2::labs(
      title = paste(
        estimator_spec$display_name, estimator_spec$display$title_quantity
      ),
      subtitle = paste(
        "Pointwise envelopes over the joint identified set at",
        paste(labels, collapse = ", ")
      ),
      x = NULL, y = estimator_spec$display$y_label,
      caption = logvar_tau_sweep_caption(labels, widths)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(hjust = 0, size = 7.5),
      plot.margin = ggplot2::margin(8, 10, 8, 10)
    )
  if (log_scale) {
    fig <- fig + ggplot2::scale_y_log10()
  }
  device <- PAPER_FIGURE_RENDER_CONTROL$devices$fitted_volatility
  write_svg(path, device[["width"]], device[["height"]], function() print(fig))
  widths
}
