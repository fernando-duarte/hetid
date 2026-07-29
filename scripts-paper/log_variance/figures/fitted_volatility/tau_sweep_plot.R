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

# Decade ticks in the "1960 Q1" form the paper's other quarterly time-series
# figures carry. Seven labels this long need roughly 275pt of panel, so they fit
# only because the canvas is wide enough to leave the centred panel above that;
# on a narrower panel they collide and the year alone has to do.
logvar_tau_sweep_date_labels <- function(breaks) {
  quarter <- (as.integer(format(breaks, "%m")) - 1L) %/% 3L + 1L
  ifelse(is.na(breaks), "", paste0(format(breaks, "%Y"), " Q", quarter))
}

# Each key names its own slack, so the legend needs no title. svglite reserves
# every key at the width of the raw LaTeX source, which \includesvg then typesets
# far narrower, so five of these in one row lay out wider than the panel and the
# last falls off the canvas: hence two rows, and no space around the "=" (the
# math mode adds its own, so "$\\tau=0.05$" typesets exactly like "$\\tau = 0.05$"
# while reserving two characters less).
logvar_tau_sweep_key_labels <- function(labels) sprintf("$\\tau=%s$", labels)

# Two lines, so the rotated title reads as two stacked columns. The plotted
# series is the conditional standard deviation in levels (exp(eta/2)), NOT its
# logarithm -- only the axis is transformed -- so the log belongs to the scale
# note and never to the quantity. The linear-y sibling drops that note.
logvar_tau_sweep_y_label <- function(log_scale) {
  paste0(
    "Conditional volatility\n(percentage points",
    if (log_scale) ", log scale" else "", ")"
  )
}

# Right padding that puts the PANEL, not the whole canvas, at the centre of the
# figure. \centering centres the file, and the axis title and tick labels hang
# off the left of the panel, so an unpadded figure sits visibly right of the text
# block -- the wider the y title, the worse. Measured off a throwaway device at
# the real canvas size, because the column widths come from font metrics that
# only resolve on an open device; recomputing beats a constant, which would go
# stale the next time the axis text changes.
logvar_tau_sweep_center_pad <- function(fig, width, height) {
  scratch <- tempfile(fileext = ".svg")
  svglite::svglite(scratch, width = width, height = height)
  on.exit(
    {
      grDevices::dev.off()
      unlink(scratch)
    },
    add = TRUE
  )
  gt <- ggplot2::ggplotGrob(fig)
  panel <- min(gt$layout$l[gt$layout$name == "panel"])
  to_pt <- function(w) sum(grid::convertWidth(w, "pt", valueOnly = TRUE))
  left <- to_pt(gt$widths[seq_len(panel - 1L)])
  right <- to_pt(gt$widths[seq.int(panel + 1L, length(gt$widths))])
  max(0, left - right)
}

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
      data = point, ggplot2::aes(y = volatility_point, group = run),
      color = logvar_style$point, linewidth = logvar_style$point_linewidth
    ) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(palette, labels),
      # layers are added widest-first, so pin the key order to increasing tau
      limits = labels, breaks = labels,
      labels = logvar_tau_sweep_key_labels(labels), name = NULL,
      guide = ggplot2::guide_legend(nrow = 1)
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
      # the bands leave the top of the panel empty, so one horizontal row of
      # keys sits above them rather than over the widest slack
      legend.position = c(0.025, 0.975),
      legend.justification = c(0, 1),
      legend.direction = "horizontal",
      legend.text = ggplot2::element_text(margin = ggplot2::margin(0, 6, 0, 3)),
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
