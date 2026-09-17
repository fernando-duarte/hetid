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

# The set's shadow on each coordinate wall: the zero contour of the projection
# margin over that wall's grid, filled and outlined.
draw_region_walls <- function(pmat, sys, lims, offsets, n_wall, fill) {
  axes <- seq_along(lims)
  for (perp in axes) {
    keep <- setdiff(axes, perp)
    first <- seq(lims[[keep[1]]][1], lims[[keep[1]]][2], length.out = n_wall)
    second <- seq(lims[[keep[2]]][1], lims[[keep[2]]][2], length.out = n_wall)
    grid <- region_grid(first, second)
    margin <- region_envelope(sys, perp, grid$X, grid$Y)$M
    for (contour in grDevices::contourLines(first, second, margin, levels = 0)) {
      xyz <- matrix(0, length(contour$x), length(lims))
      xyz[, perp] <- offsets[perp]
      xyz[, keep[1]] <- contour$x
      xyz[, keep[2]] <- contour$y
      p <- project_region_3d(xyz, pmat)
      graphics::polygon(p[, "x"], p[, "y"], col = fill, border = "black", lwd = 1.8)
    }
  }
}
