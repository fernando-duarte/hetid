# Normalized endpoint panels, a derivative of the combined nesting panel. That
# panel shows the identified intervals nesting in tau, where the level and the
# width dominate the picture; these ask the separate question of whether the
# endpoints MOVE alike over the sample once the level and the scale are taken
# out. Each series is standardized to mean zero and unit variance over its own
# drawn dates, so only its shape survives, and the tau = 0 point fit is
# standardized the same way as the reference the swept endpoints are read
# against.
#
# One panel per side of the interval: every slack's upper endpoint together, and
# every slack's lower endpoint together. Standardizing per series is what makes
# the comparison legible and is also what it costs: nesting, width, and level are
# all gone from these panels, and only the combined panel still carries them.

paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "plot.R"
))
paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "tau_sweep_theme.R"
))

LOGVAR_TAU_SWEEP_SIDES <- c("lower", "upper")

# Level naming the point fit in the shared colour scale. Every line here is a
# line, unlike the band panel where the point fit needs a scale of its own to
# earn a key, so one scale carries the slacks and the point fit together and the
# keys read up in tau from the point-identified case.
LOGVAR_TAU_SWEEP_POINT_LEVEL <- "0"

# Mean zero, unit variance. Band columns are finite on every drawn date by
# construction, so a non-finite entry here means the caller passed something
# other than a band; a flat series has no shape to compare and is an error
# rather than a division by zero.
logvar_tau_sweep_standardize <- function(x) {
  stopifnot(length(x) > 1L, all(is.finite(x)))
  spread <- stats::sd(x)
  stopifnot(spread > 0)
  (x - mean(x)) / spread
}

# One standardized series per slack plus the tau = 0 fit, stacked long so a
# single colour scale carries the whole panel. Runs come through unchanged, so a
# fail-closed gap splits the line instead of being bridged across.
logvar_tau_sweep_normalized_data <- function(envs, labels, side) {
  column <- paste0("volatility_", side)
  series <- lapply(seq_along(envs), function(i) {
    band <- logvar_fitted_vol_plot_data(envs[[i]]$data)$band
    stopifnot(nrow(band) > 0L)
    data.frame(
      date = band$date,
      value = logvar_tau_sweep_standardize(band[[column]]),
      series = labels[i],
      run = paste(labels[i], band$run, sep = ":"),
      row.names = NULL
    )
  })
  # the point fit is one curve shared by every slack, so it is read off the
  # first envelope rather than repeated per slack
  point <- logvar_fitted_vol_plot_data(envs[[1L]]$data)$point
  stopifnot(nrow(point) > 0L)
  rbind(
    do.call(rbind, series),
    data.frame(
      date = point$date,
      value = logvar_tau_sweep_standardize(point$volatility_point),
      series = LOGVAR_TAU_SWEEP_POINT_LEVEL,
      run = paste(LOGVAR_TAU_SWEEP_POINT_LEVEL, point$run, sep = ":"),
      row.names = NULL
    )
  )
}

# The series is the standardized conditional standard deviation, so the axis
# carries no unit; the caption supplies the standardization.
logvar_tau_sweep_normalized_y_label <- function(side) {
  paste0("Normalized conditional volatility\n(", side, " envelope)")
}

logvar_tau_sweep_normalized_render <- function(envs, path, side) {
  stopifnot(side %in% LOGVAR_TAU_SWEEP_SIDES)
  taus <- vapply(envs, function(e) e$metadata$tau, numeric(1))
  envs <- envs[order(taus)]
  # unname: envs carries full-precision paper_tau_key names, and ggplot2 reads a
  # named breaks vector's names as the key labels
  labels <- format(unname(sort(taus)))
  levels <- c(LOGVAR_TAU_SWEEP_POINT_LEVEL, labels)
  logvar_style <- PAPER_FIGURE_STYLE$log_variance
  palette <- grDevices::colorRampPalette(
    PAPER_FIGURE_STYLE$identified_set$sweep_ramp
  )(length(labels))
  rows <- logvar_tau_sweep_normalized_data(envs, labels, side)
  rows$series <- factor(rows$series, levels = levels)
  is_point <- rows$series == LOGVAR_TAU_SWEEP_POINT_LEVEL
  fig <- ggplot2::ggplot(
    mapping = ggplot2::aes(date, value, colour = series, group = run)
  ) +
    ggplot2::geom_line(
      data = rows[!is_point, ],
      linewidth = logvar_style$point_linewidth
    ) +
    # The reference is painted last. At the tight slacks the standardized
    # endpoint is indistinguishable from it, so whichever of the two goes down
    # second is the one the reader sees, and the reference losing that race
    # leaves the panel with nothing to read the endpoints against.
    ggplot2::geom_line(
      data = rows[is_point, ],
      linewidth = logvar_style$point_linewidth
    ) +
    ggplot2::scale_colour_manual(
      values = stats::setNames(c(logvar_style$point, palette), levels),
      limits = levels, breaks = levels,
      labels = logvar_tau_sweep_key_labels(levels), name = NULL,
      guide = ggplot2::guide_legend(nrow = 1)
    ) +
    ggplot2::scale_x_date(
      name = NULL,
      breaks = seq(as.Date("1960-01-01"), as.Date("2020-01-01"), by = "10 years"),
      labels = logvar_tau_sweep_date_labels
    ) +
    ggplot2::labs(y = logvar_tau_sweep_normalized_y_label(side)) +
    ggplot2::theme_classic(base_size = 11) +
    logvar_tau_sweep_theme()
  device <- PAPER_FIGURE_RENDER_CONTROL$devices$fitted_volatility_sweep
  fig <- logvar_tau_sweep_center(fig, device)
  ggplot2::ggsave(
    path, fig,
    width = device[["width"]], height = device[["height"]]
  )
  # correlation of each swept endpoint with the point fit over their common
  # dates, which is the number the panel is drawn to make visible
  point <- rows[rows$series == LOGVAR_TAU_SWEEP_POINT_LEVEL, ]
  vapply(labels, function(lab) {
    one <- rows[rows$series == lab, ]
    # match on the Date columns directly: intersect() strips the Date class and
    # would leave match() comparing a numeric against a Date
    hit <- match(point$date, one$date)
    ok <- !is.na(hit)
    stats::cor(one$value[hit[ok]], point$value[ok])
  }, numeric(1))
}
