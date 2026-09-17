# Drawing helpers for the three-dimensional identified-region plot.

draw_projected_line <- function(xyz, pmat, ...) {
  p <- project_region_3d(xyz, pmat)
  graphics::lines(p[, "x"], p[, "y"], ...)
}

draw_region_point <- function(pmat, xyz, ...) {
  p <- project_region_3d(matrix(unname(xyz), nrow = 1), pmat)
  graphics::points(p[, "x"], p[, "y"], ...)
}

# Dotted rays dropping an interior point onto each coordinate wall, each ending
# in a hollow marker of that point's own shape, so a reader can pick its three
# coordinates off the walls.
draw_region_projections <- function(pmat, xyz, offsets, col, pch) {
  xyz <- unname(xyz)
  ray <- grDevices::adjustcolor(col, alpha.f = 0.65)
  for (perp in seq_along(offsets)) {
    wall_point <- xyz
    wall_point[perp] <- offsets[perp]
    draw_projected_line(
      rbind(xyz, wall_point), pmat,
      col = ray, lty = 3, lwd = 1.3
    )
    draw_region_point(pmat, wall_point,
      pch = pch, bg = "white", col = col, cex = 1.1, lwd = 1.8
    )
  }
}

draw_region_panes <- function(pmat, lims, ticks) {
  lo <- vapply(lims, `[`, numeric(1), 1)
  hi <- vapply(lims, `[`, numeric(1), 2)
  panes <- list(
    rbind(
      c(hi[1], lo[2], lo[3]), c(hi[1], hi[2], lo[3]),
      c(hi[1], hi[2], hi[3]), c(hi[1], lo[2], hi[3])
    ),
    rbind(
      c(lo[1], lo[2], lo[3]), c(hi[1], lo[2], lo[3]),
      c(hi[1], lo[2], hi[3]), c(lo[1], lo[2], hi[3])
    ),
    rbind(
      c(lo[1], lo[2], lo[3]), c(hi[1], lo[2], lo[3]),
      c(hi[1], hi[2], lo[3]), c(lo[1], hi[2], lo[3])
    )
  )
  pane_colors <- c("#f9f9f9", "#f2f2f2", "#f5f5f5")
  for (i in seq_along(panes)) {
    pane <- panes[[i]]
    p <- project_region_3d(pane, pmat)
    graphics::polygon(p[, "x"], p[, "y"], col = pane_colors[i], border = NA)
  }
  grid_col <- "#b0b0b0"
  for (x in ticks[[1]]) {
    draw_projected_line(rbind(c(x, lo[2], lo[3]), c(x, lo[2], hi[3])),
      pmat,
      col = grid_col, lwd = 1
    )
    draw_projected_line(rbind(c(x, lo[2], lo[3]), c(x, hi[2], lo[3])),
      pmat,
      col = grid_col, lwd = 1
    )
  }
  for (y in ticks[[2]]) {
    draw_projected_line(rbind(c(hi[1], y, lo[3]), c(hi[1], y, hi[3])),
      pmat,
      col = grid_col, lwd = 1
    )
    draw_projected_line(rbind(c(lo[1], y, lo[3]), c(hi[1], y, lo[3])),
      pmat,
      col = grid_col, lwd = 1
    )
  }
  for (z in ticks[[3]]) {
    draw_projected_line(rbind(c(hi[1], lo[2], z), c(hi[1], hi[2], z)),
      pmat,
      col = grid_col, lwd = 1
    )
    draw_projected_line(rbind(c(lo[1], lo[2], z), c(hi[1], lo[2], z)),
      pmat,
      col = grid_col, lwd = 1
    )
  }
  for (pane in panes) {
    p <- project_region_3d(rbind(pane, pane[1, ]), pmat)
    graphics::lines(p[, "x"], p[, "y"], col = grid_col, lwd = 1)
  }
}

# The three cube edges that carry the axes. One call per figure keeps the edge
# geometry beside the pane geometry it has to agree with.
draw_region_axes <- function(pmat, lo, hi, ticks, labels, titles, center) {
  draw_region_axis(
    pmat, c(lo[1], hi[2], lo[3]), c(hi[1], hi[2], lo[3]),
    ticks[[1]], labels[[1]], titles[[1]], center
  )
  draw_region_axis(
    pmat, c(lo[1], lo[2], lo[3]), c(lo[1], hi[2], lo[3]),
    ticks[[2]], labels[[2]], titles[[2]], center
  )
  draw_region_axis(
    pmat, c(lo[1], lo[2], lo[3]), c(lo[1], lo[2], hi[3]),
    ticks[[3]], labels[[3]], titles[[3]], center
  )
}

# Tick labels hang off the tick tips by the edge facing the axis, and the title
# sits past the widest label, so the spacing drawn here survives the paper's
# \includesvg re-typesetting every string at its own width. Lengths are in
# points; persp's user coordinates are isotropic, so one conversion serves both
# axes of the page.
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
  pt <- graphics::xinch(1 / 72)
  tips <- sweep(points, 2, tick_pt * pt * normal, "+")
  graphics::segments(line[1, 1], line[1, 2], line[2, 1], line[2, 2], lwd = 1.05)
  graphics::segments(points[, 1], points[, 2], tips[, 1], tips[, 2], lwd = 1.05)
  anchor <- sweep(tips, 2, gap_pt * pt * normal, "+")
  # the right edge is pinned under a leftward normal and the top edge under a
  # downward one; along a page axis the normal barely leaves, the label stays
  # centred on its tick
  adj <- ifelse(normal < -0.3, 1, ifelse(normal > 0.3, 0, 0.5))
  graphics::text(anchor[, 1], anchor[, 2], labels, adj = adj)
  # how far the label block reaches along the normal; the dollar signs are
  # svglite's raw-source width, which LaTeX never sets
  bare <- gsub("$", "", labels, fixed = TRUE)
  reach <- max(
    graphics::strwidth(bare) * abs(normal[1]) +
      graphics::strheight(bare) * abs(normal[2])
  )
  angle <- atan2(direction[2], direction[1]) * 180 / pi
  if (angle > 90) angle <- angle - 180
  if (angle < -90) angle <- angle + 180
  # centred on the labelled span rather than the cube edge, which the frame's
  # padding leaves longer on one side of the ladder than the other
  labelled <- colMeans(points[c(which.min(at), which.max(at)), , drop = FALSE])
  title_offset <- (tick_pt + 3 * gap_pt) * pt + reach +
    graphics::strheight(title)
  title_position <- labelled + title_offset * normal
  graphics::text(title_position[1], title_position[2], title, srt = angle)
}
