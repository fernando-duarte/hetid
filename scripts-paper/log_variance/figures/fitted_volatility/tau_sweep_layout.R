# Band assembly and layout text for the combined slack panel: the per-slack
# ribbons, the axis and legend labels, and the padding that centres the panel
# rather than the canvas. Split from tau_sweep_plot.R for the repository line
# cap. Definitions only; sourced there.

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

# The point fit is one fixed-colour line, so it earns a key only by being mapped
# to a scale of its own. Its guide sits first, left of the band swatches, so the
# row reads up in tau from the point-identified case.
LOGVAR_TAU_SWEEP_POINT_KEY <- "$\\tau=0$"

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
