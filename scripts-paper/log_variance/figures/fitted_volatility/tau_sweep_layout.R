# Band assembly, layout text, and shared theme for the combined slack panels:
# the per-slack ribbons, the axis and legend labels, the padding that centres
# the panel rather than the canvas, and the theme itself. Split from
# tau_sweep_plot.R for the repository line cap, and shared from there with the
# normalized-endpoint panels (tau_sweep_normalized_plot.R) so the two exhibits
# cannot drift apart. Definitions only; sourced by both.

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

# Key for the OLS benchmark line: the same log-variance path read off the
# two-step log-OLS map at the mean equation's OLS news coefficients rather than
# anywhere in the identified set. Plain text, since it names an estimator and
# not a slack.
LOGVAR_TAU_SWEEP_OLS_KEY <- "OLS"

# Which of the two families the panel carries. Both sit on a linear axis, so the
# title names the plotted series outright and no scale note is needed; the
# log-variance panel is the one the single-slack exhibits also carry.
logvar_tau_sweep_y_label <- function(log_variance) {
  if (log_variance) {
    LOGVAR_FITTED_VOL_Y_LABEL_TEX
  } else {
    LOGVAR_FITTED_VOL_Y_LABEL_EXP_TEX
  }
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

# theme_classic at base size 11 with a thin panel border and no in-figure title,
# plus the legend geometry both panels need: one row of keys sitting above the
# frame rather than inside it, because that row is wider than the panel even on
# the widened canvas. location = "panel" centres it on the panel rather than on
# the plot, which the right centring pad would otherwise pull it off by the width
# of the axis block. The key spacing and text margins are trimmed to keep the row
# inside the panel's width -- and it only reads wider than it is, because svglite
# reserves the raw "$\\tau=0.05$" source that \includesvg then typesets narrower.
logvar_tau_sweep_theme <- function() {
  ggplot2::theme(
    legend.background = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
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
}

# Centring pad applied last, because it has to measure the finished figure.
logvar_tau_sweep_center <- function(fig, device) {
  half_line <- 11 / 2
  fig + ggplot2::theme(
    plot.margin = ggplot2::margin(
      half_line,
      half_line + logvar_tau_sweep_center_pad(
        fig, device[["width"]], device[["height"]]
      ),
      half_line, half_line,
      unit = "pt"
    )
  )
}
