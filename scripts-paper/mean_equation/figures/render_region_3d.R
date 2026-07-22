# The joint baseline identified set for b_N in standard-deviation units.
# The closed triangular shell and its coordinate-wall shadows follow the visual
# construction of the paper's reference rendering. The set envelope itself is
# evaluated analytically by prepare_region_geometry.R.
# Writes the region SVG to the typed figure directory.

local({
  baseline_tau <- PAPER_ANALYSIS_CONTRACT$tau$baseline
  dimension <- PAPER_ANALYSIS_CONTRACT$figure$region_dimension
  render <- PAPER_FIGURE_RENDER_CONTROL$region_3d
  stopifnot(
    identical(dimension, 3L),
    identical(PAPER_ANALYSIS_CONTRACT$model$n_mean_pc, dimension)
  )
  paper_source_once(
    paper_path(
      "mean_equation",
      "figures",
      "build_region_3d_geometry.R"
    ),
    envir = environment()
  )
  paper_source_once(
    paper_path(
      "mean_equation",
      "figures",
      "draw_region_3d.R"
    ),
    envir = environment()
  )

  elevation <- render$camera$elevation
  azimuth <- render$camera$azimuth
  theta_view <- azimuth + render$camera$azimuth_offset
  n_wall <- render$wall_grid_points
  sys <- region_sd_system(baseline_tau)
  box0 <- region_sd_box(baseline_tau)
  expand_limit <- function(k) {
    values <- c(box0$lo[k], box0$hi[k])
    values + c(-1, 1) * render$limit_padding * diff(values)
  }
  axes <- seq_len(dimension)
  natural_lims <- lapply(axes, expand_limit)
  lims <- natural_lims
  lims[[1]][2] <- render$manual_limits$x_upper
  lims[[2]][1] <- render$manual_limits$y_lower
  stopifnot(all(vapply(axes, function(axis) {
    box0$lo[[axis]] >= lims[[axis]][1L] &&
      box0$hi[[axis]] <= lims[[axis]][2L]
  }, logical(1))))
  ticks <- render$ticks
  tick_labels <- lapply(
    ticks,
    formatC,
    format = "f",
    digits = render$tick_digits
  )
  mesh <- build_region_mesh(sys, natural_lims, seed = render$seed)

  svglite::svglite(
    filename = artifact_path("mean_region_figure"),
    width = render$device$width,
    height = render$device$height
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(3.2, 4.5, 2.0, 2.2), xpd = NA, family = "sans")
  pmat <- graphics::persp(
    x = lims[[1]],
    y = lims[[2]],
    z = matrix(lims[[3]][1], 2, 2),
    zlim = lims[[3]],
    theta = theta_view,
    phi = elevation,
    r = render$camera$radius,
    d = render$camera$distance,
    scale = TRUE,
    expand = render$camera$expand,
    col = NA,
    border = NA,
    axes = FALSE,
    box = FALSE
  )
  draw_region_panes(pmat, lims, ticks)

  lo <- vapply(lims, `[`, numeric(1), 1)
  hi <- vapply(lims, `[`, numeric(1), 2)
  offsets <- c(hi[1], lo[2], lo[3])
  palette <- PAPER_FIGURE_STYLE$region
  wall_fill <- grDevices::adjustcolor(palette$wall_fill, alpha.f = 0.4)
  for (perp in axes) {
    keep <- setdiff(axes, perp)
    first <- seq(lims[[keep[1]]][1], lims[[keep[1]]][2], length.out = n_wall)
    second <- seq(lims[[keep[2]]][1], lims[[keep[2]]][2], length.out = n_wall)
    grid <- region_grid(first, second)
    margin <- region_envelope(sys, perp, grid$X, grid$Y)$M
    contours <- grDevices::contourLines(first, second, margin, levels = 0)
    for (contour in contours) {
      xyz <- matrix(0, length(contour$x), dimension)
      xyz[, perp] <- offsets[perp]
      xyz[, keep[1]] <- contour$x
      xyz[, keep[2]] <- contour$y
      p <- project_region_3d(xyz, pmat)
      graphics::polygon(
        p[, "x"], p[, "y"],
        col = wall_fill, border = "black", lwd = 1.8
      )
    }
  }

  point0 <- unname(region_sd_point())
  projection_col <- grDevices::adjustcolor(palette$tau0_point, alpha.f = 0.65)
  for (perp in axes) {
    wall_point <- point0
    wall_point[perp] <- offsets[perp]
    draw_projected_line(
      rbind(point0, wall_point), pmat,
      col = projection_col, lty = 3, lwd = 1.3
    )
    projected <- project_region_3d(matrix(wall_point, nrow = 1), pmat)
    graphics::points(
      projected[, "x"], projected[, "y"],
      pch = 21, bg = "white",
      col = palette$tau0_point, cex = 1.1, lwd = 1.8
    )
  }

  face_depth <- vapply(mesh$faces, function(face) {
    mean(project_region_3d(face, pmat)[, "depth"])
  }, numeric(1))
  face_fill <- grDevices::adjustcolor(palette$face_fill, alpha.f = 0.10)
  for (face in mesh$faces[order(face_depth, decreasing = TRUE)]) {
    projected <- project_region_3d(face, pmat)
    graphics::polygon(
      projected[, "x"], projected[, "y"],
      col = face_fill, border = NA
    )
  }
  segment_depth <- vapply(mesh$segments, function(segment) {
    mean(project_region_3d(segment, pmat)[, "depth"])
  }, numeric(1))
  for (segment in mesh$segments[order(segment_depth, decreasing = TRUE)]) {
    draw_projected_line(segment, pmat, col = palette$mesh_segment, lwd = 0.9)
  }
  projected_point <- project_region_3d(matrix(point0, nrow = 1), pmat)
  graphics::points(
    projected_point[, "x"], projected_point[, "y"],
    pch = 21,
    bg = palette$tau0_point, col = "black", cex = 1.35, lwd = 0.5
  )

  center <- (lo + hi) / 2
  axis_labels <- list(
    expression(tilde(b)[1 * "," * N]),
    expression(tilde(b)[2 * "," * N]),
    expression(tilde(b)[3 * "," * N])
  )
  draw_region_axis(
    pmat, c(lo[1], hi[2], lo[3]), c(hi[1], hi[2], lo[3]),
    ticks[[1]], tick_labels[[1]], axis_labels[[1]], center,
    tick_gap = 0.006, title_nudge = c(-0.005, 0)
  )
  draw_region_axis(
    pmat, c(lo[1], lo[2], lo[3]), c(lo[1], hi[2], lo[3]),
    ticks[[2]], tick_labels[[2]], axis_labels[[2]], center,
    tick_side = 1
  )
  draw_region_axis(
    pmat, c(lo[1], lo[2], lo[3]), c(lo[1], lo[2], hi[3]),
    ticks[[3]], tick_labels[[3]], axis_labels[[3]], center,
    tick_gap = 0.008
  )
})

cat("set_id_region_3d: wrote", artifact_path("mean_region_figure"), "\n")
