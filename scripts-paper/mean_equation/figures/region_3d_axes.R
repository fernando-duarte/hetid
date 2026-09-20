# The three cube edges that carry the axes of the region figure. The edges and
# ticks are drawn on the device; the tick labels and titles are typeset once
# the device has closed (latex_labels.R) and placed by their rendered ink.
# Every offset past a tick is in points, which svglite writes one-to-one, so
# the labels sit at the same distances whatever the figure is scaled to.

# Uniform axis scaling cancels in the normalized 3D projection, so only the
# displayed tick values and titles change. Precision follows the scaled ticks.
region_3d_axis_labels <- function(render, units, ticks) {
  multiplier <- render$axis_multiplier
  stopifnot(length(multiplier) == 1L, is.finite(multiplier), multiplier > 0)
  ticks <- lapply(ticks, `*`, multiplier)
  digits <- vapply(ticks, region_tick_digits, integer(1))
  titles <- paste0("$", multiplier, " \\times ", substring(render$axis_labels[[units]], 2L))
  list(
    ticks = Map(function(at, places) {
      paste0("$", formatC(at, format = "f", digits = places), "$")
    }, ticks, digits),
    titles = titles
  )
}

# One call per figure keeps the edge geometry beside the pane geometry it has
# to agree with. Returns the three label specs for place_region_labels.
draw_region_axes <- function(pmat, lo, hi, ticks, labels, titles, center) {
  list(
    draw_region_axis(
      pmat, c(lo[1], hi[2], lo[3]), c(hi[1], hi[2], lo[3]),
      ticks[[1]], labels[[1]], titles[[1]], center
    ),
    draw_region_axis(
      pmat, c(lo[1], lo[2], lo[3]), c(lo[1], hi[2], lo[3]),
      ticks[[2]], labels[[2]], titles[[2]], center
    ),
    draw_region_axis(
      pmat, c(lo[1], lo[2], lo[3]), c(lo[1], lo[2], hi[3]),
      ticks[[3]], labels[[3]], titles[[3]], center
    )
  )
}

# Draws one edge with its ticks and returns where its labels go, in the file's
# coordinates: each tick label's anchor one gap past its tick tip, the edge of
# the label pinned there, and the ladder midpoint and outward normal the title
# is set from once the labels' ink is known.
draw_region_axis <- function(pmat, start, end, at, labels, title, center,
                             tick_pt = 4, gap_pt = 5) {
  axis_points <- t(vapply(at, function(value) {
    start + (end - start) * (value - start[which(start != end)]) /
      (end[which(start != end)] - start[which(start != end)])
  }, numeric(3)))
  projected <- project_region_3d(rbind(start, end, axis_points), pmat)
  line <- projected[1:2, 1:2, drop = FALSE]
  points <- projected[-(1:2), 1:2, drop = FALSE]
  center_2d <- project_region_3d(matrix(center, nrow = 1), pmat)[1, 1:2]
  direction <- line[2, ] - line[1, ]
  normal <- c(-direction[2], direction[1]) / sqrt(sum(direction^2))
  midpoint <- colMeans(line)
  if (sum(normal * (midpoint - center_2d)) < 0) normal <- -normal
  tips <- sweep(points, 2, tick_pt * graphics::xinch(1 / 72) * normal, "+")
  graphics::segments(line[1, 1], line[1, 2], line[2, 1], line[2, 2], lwd = 1.05)
  graphics::segments(points[, 1], points[, 2], tips[, 1], tips[, 2], lwd = 1.05)
  angle <- atan2(direction[2], direction[1]) * 180 / pi
  if (angle > 90) angle <- angle - 180
  if (angle < -90) angle <- angle + 180
  # from here on in the file's coordinates, where y runs down the page
  device <- function(xy) {
    cbind(
      graphics::grconvertX(xy[, 1], "user", "device"),
      graphics::grconvertY(xy[, 2], "user", "device")
    )
  }
  down <- normal * c(1, -1)
  anchors <- sweep(device(tips), 2, gap_pt * down, "+")
  ends <- device(points[c(which.min(at), which.max(at)), , drop = FALSE])
  along <- ends[2, ] - ends[1, ]
  list(
    labels = data.frame(text = labels, x = anchors[, 1], y = anchors[, 2]),
    # the edge facing the axis is the one pinned: right edge under a leftward
    # normal, top edge under a downward one; along a page axis the normal
    # barely leaves, the label stays centred on its tick
    adj = ifelse(normal < -0.3, 1, ifelse(normal > 0.3, 0, 0.5)),
    title = title, mid = colMeans(ends), normal = down,
    along = along / sqrt(sum(along^2)), angle = angle
  )
}

# Typesets every label of the figure in one LaTeX run, places the tick labels
# at their anchors, and sets each title title_gap_pt past the farthest
# tick-label corner along the axis normal, turned to the edge and centred on
# the ladder, or slid title_shift_pt[k] along axis k toward larger values.
# Returns the groups to write and every placed corner, for the crop.
place_region_labels <- function(axes, pointsize, title_gap_pt, title_shift_pt) {
  strings <- unlist(lapply(axes, function(axis) c(axis$labels$text, axis$title)))
  glyphs <- typeset_latex_labels(strings, pointsize)
  placed <- list()
  offset <- 0L
  for (k in seq_along(axes)) {
    axis <- axes[[k]]
    n <- nrow(axis$labels)
    ticks <- lapply(seq_len(n), function(i) {
      place_latex_label(glyphs[[offset + i]], axis$labels$x[i], axis$labels$y[i], axis$adj)
    })
    glyph <- glyphs[[offset + n + 1L]]
    offset <- offset + n + 1L
    corners <- do.call(rbind, lapply(ticks, `[[`, "corners"))
    far <- max(sweep(corners, 2, axis$mid) %*% axis$normal)
    where <- axis$mid + title_shift_pt[k] * axis$along +
      (far + title_gap_pt + glyph$box[4] / 2) * axis$normal
    title <- place_latex_label(glyph, where[1], where[2], rot = axis$angle)
    placed <- c(placed, ticks, list(title))
  }
  list(
    groups = vapply(placed, `[[`, character(1), "group"),
    corners = do.call(rbind, lapply(placed, `[[`, "corners"))
  )
}
