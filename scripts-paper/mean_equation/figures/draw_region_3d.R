# Drawing helpers for the three-dimensional identified-region plot.

draw_projected_line <- function(xyz, pmat, ...) {
  p <- project_region_3d(xyz, pmat)
  graphics::lines(p[, "x"], p[, "y"], ...)
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

draw_region_axis <- function(pmat, start, end, at, labels, title, center,
                             tick_side = 1, tick_gap = 0.005,
                             title_nudge = c(0, 0), title_gap = 0.013,
                             title_angle = NULL) {
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
  tick_normal <- tick_side * normal
  graphics::segments(line[1, 1], line[1, 2], line[2, 1], line[2, 2], lwd = 1.05)
  graphics::segments(
    points[, 1], points[, 2], points[, 1] + 0.003 * tick_normal[1],
    points[, 2] + 0.003 * tick_normal[2],
    lwd = 1.05
  )
  graphics::text(points[, 1] + tick_gap * tick_normal[1],
    points[, 2] + tick_gap * tick_normal[2], labels,
    cex = 0.83
  )
  angle <- atan2(direction[2], direction[1]) * 180 / pi
  if (angle > 90) angle <- angle - 180
  if (angle < -90) angle <- angle + 180
  title_position <- midpoint + title_gap * normal + title_nudge
  graphics::text(
    title_position[1], title_position[2], title,
    srt = if (is.null(title_angle)) angle else title_angle, cex = 0.9
  )
}
