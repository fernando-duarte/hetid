# The joint baseline identified set for b_N in standard-deviation units.
# The closed triangular shell and its coordinate-wall shadows follow the visual
# construction of the paper's reference rendering. The set envelope itself is
# evaluated analytically by prepare_region_geometry.R.
# Writes the region SVG to the typed figure directory, plus an _ols variant
# marking the OLS benchmark point.

local({
  baseline_tau <- PAPER_ANALYSIS_CONTRACT$tau$baseline
  widest_tau <- max(PAPER_ANALYSIS_CONTRACT$tau$display)
  dimension <- PAPER_ANALYSIS_CONTRACT$figure$region_dimension
  render <- PAPER_FIGURE_RENDER_CONTROL$region_3d
  stopifnot(
    identical(dimension, 3L),
    identical(PAPER_ANALYSIS_CONTRACT$model$n_mean_pc, dimension)
  )
  paper_source_once(
    paper_path("mean_equation", "figures", "build_region_3d_geometry.R"),
    envir = environment()
  )
  paper_source_once(
    paper_path("mean_equation", "figures", "draw_region_3d.R"),
    envir = environment()
  )

  elevation <- render$camera$elevation
  azimuth <- render$camera$azimuth
  theta_view <- azimuth + render$camera$azimuth_offset
  n_wall <- render$wall_grid_points
  axes <- seq_len(dimension)
  palette <- PAPER_FIGURE_STYLE$region
  ols_point <- region_sd_ols_point()

  # ols = "point" adds the OLS benchmark marker and "projected" also drops it
  # onto the walls. It clears the baseline slack's third axis, so that axis grows
  # to hold it and the tick ladder continues at its own spacing. The hand-set
  # frame and ticks belong to the baseline; a wider slack outgrows both and
  # derives them from its own box, so every variant needs its own pass.
  draw <- function(artifact, ols = c("none", "point", "projected"),
                   tau = baseline_tau) {
    ols <- match.arg(ols)
    baseline <- identical(tau, baseline_tau)
    sys <- region_sd_system(tau)
    box0 <- region_sd_box(tau)
    lims <- lapply(axes, function(k) {
      values <- c(box0$lo[k], box0$hi[k])
      values + c(-1, 1) * render$limit_padding * diff(values)
    })
    # the shell is meshed over the padded box, before any display override
    mesh <- build_region_mesh(sys, lims, seed = render$seed)
    ticks <- render$ticks
    if (baseline) {
      lims[[1]][2] <- render$manual_limits$x_upper
      lims[[2]][1] <- render$manual_limits$y_lower
    } else {
      ticks <- lapply(lims, function(l) {
        at <- pretty(l, render$derived_tick_n)
        at[at > l[1] & at < l[2]]
      })
    }
    if (!identical(ols, "none")) {
      lims[[3]][2] <- max(
        lims[[3]][2],
        ols_point[[3]] + render$limit_padding * diff(lims[[3]])
      )
      step <- diff(ticks[[3]])[1L]
      top <- max(ticks[[3]])
      n_extra <- max(floor((lims[[3]][2] - top) / step), 0)
      ticks[[3]] <- c(ticks[[3]], top + step * seq_len(n_extra))
    }
    stopifnot(all(vapply(axes, function(axis) {
      box0$lo[[axis]] >= lims[[axis]][1L] &&
        box0$hi[[axis]] <= lims[[axis]][2L]
    }, logical(1))))
    tick_labels <- lapply(ticks, function(t) {
      paste0("$", formatC(t, format = "f", digits = render$tick_digits), "$")
    })

    svglite::svglite(
      filename = artifact_path(artifact),
      width = 7,
      height = 6.1
    )
    on.exit(grDevices::dev.off(), add = TRUE)
    graphics::par(mar = c(3.2, 4.5, 2.0, 3.6), xpd = NA, family = "sans")
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
    draw_region_projections(pmat, point0, offsets, palette$tau0_point, 21)
    if (identical(ols, "projected")) {
      draw_region_projections(
        pmat, ols_point, offsets, palette$ols_point, palette$ols_pch
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
    draw_region_point(pmat, point0,
      pch = 21, bg = palette$tau0_point,
      col = "black", cex = 1.35, lwd = 0.5
    )
    if (!identical(ols, "none")) {
      draw_region_point(pmat, ols_point,
        pch = palette$ols_pch, bg = palette$ols_point,
        col = "black", cex = 1.35, lwd = 0.5
      )
    }

    center <- (lo + hi) / 2
    axis_labels <- list(
      "$\\sigma(PC_{1,N})\\, b_{1,N}$",
      "$\\sigma(PC_{2,N})\\, b_{2,N}$",
      "$\\sigma(PC_{3,N})\\, b_{3,N}$"
    )
    draw_region_axis(
      pmat, c(lo[1], hi[2], lo[3]), c(hi[1], hi[2], lo[3]),
      ticks[[1]], tick_labels[[1]], axis_labels[[1]], center,
      tick_gap = 0.009, title_gap = 0.024
    )
    draw_region_axis(
      pmat, c(lo[1], lo[2], lo[3]), c(lo[1], hi[2], lo[3]),
      ticks[[2]], tick_labels[[2]], axis_labels[[2]], center,
      tick_side = 1, tick_gap = 0.009, title_gap = 0.024
    )
    draw_region_axis(
      pmat, c(lo[1], lo[2], lo[3]), c(lo[1], lo[2], hi[3]),
      ticks[[3]], tick_labels[[3]], axis_labels[[3]], center,
      tick_gap = 0.011, title_gap = 0.028
    )
  }
  draw("mean_region_figure", "none")
  draw("mean_region_ols_figure", "point")
  draw("mean_region_ols_projected_figure", "projected")
  draw("mean_region_ols_projected_tau0p2_figure", "projected", widest_tau)
})

for (id in artifact_manifest$id[artifact_manifest$producer ==
  "mean_equation/figures/render_region_3d.R"]) {
  cat("set_id_region_3d: wrote", artifact_path(id), "\n")
}
rm(id)
