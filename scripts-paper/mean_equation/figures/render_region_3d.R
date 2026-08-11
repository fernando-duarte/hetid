# The joint identified set for b_N, drawn as a closed triangular shell with its
# coordinate-wall shadows, following the visual construction of the paper's
# reference rendering. The set envelope itself is evaluated analytically by
# prepare_region_geometry.R.
#
# One figure per slack in the render control's tau vector, per unit system
# (standardized sigma(PC_{k,N}) b_{k,N}, and raw b_{k,N}), drawn plain and with
# the OLS benchmark projected onto the walls beside the tau = 0 point.

local({
  render <- PAPER_FIGURE_RENDER_CONTROL$region_3d
  baseline_tau <- PAPER_ANALYSIS_CONTRACT$tau$baseline
  widest_tau <- max(PAPER_ANALYSIS_CONTRACT$tau$display)
  region_taus <- render$taus
  dimension <- PAPER_ANALYSIS_CONTRACT$figure$region_dimension
  stopifnot(
    identical(dimension, 3L),
    identical(PAPER_ANALYSIS_CONTRACT$model$n_mean_pc, dimension),
    # a slack at or above the bounded -> unbounded transition has no finite box
    # to frame, so it is a contract error rather than a rendering fallback
    all(region_taus < set_id_mean_eq$tau_star),
    # the hand-set frames below key off these two slacks; drop either from the
    # region vector and its tuned frame silently stops being drawn
    all(c(baseline_tau, widest_tau) %in% region_taus)
  )
  paper_source_once(
    paper_path("mean_equation", "figures", "build_region_3d_geometry.R"),
    envir = environment()
  )
  paper_source_once(
    paper_path("mean_equation", "figures", "draw_region_3d.R"),
    envir = environment()
  )
  paper_source_once(
    paper_path("mean_equation", "figures", "region_3d_frames.R"),
    envir = environment()
  )

  elevation <- render$camera$elevation
  azimuth <- render$camera$azimuth
  theta_view <- azimuth + render$camera$azimuth_offset
  n_wall <- render$wall_grid_points
  axes <- seq_len(dimension)
  palette <- PAPER_FIGURE_STYLE$region

  # The hand-tuned frames belong to the SD axes at the baseline and the widest
  # display slack. Every other slack, and every raw-unit figure, is framed from
  # its own padded box.
  frame_mode <- function(units, tau) {
    if (!identical(units, "sd")) {
      "auto"
    } else if (identical(tau, baseline_tau)) {
      "baseline"
    } else if (identical(tau, widest_tau)) {
      "widest"
    } else {
      "auto"
    }
  }

  draw <- function(ols, units, tau) {
    scale <- region_axis_scale(units)
    sys <- region_sd_system(tau, scale)
    box0 <- region_sd_box(tau, scale)
    point0 <- unname(region_sd_point(scale))
    marked <- if (identical(ols, "none")) NULL else region_sd_ols_point(scale)
    lims <- lapply(axes, function(k) {
      values <- c(box0$lo[k], box0$hi[k])
      values + c(-1, 1) * render$limit_padding * diff(values)
    })
    # the shell is meshed over the padded box, before any display override
    mesh <- build_region_mesh(sys, lims, seed = render$seed)
    frame <- region_3d_frame(
      lims, render, frame_mode(units, tau), marked,
      render$auto_frame_adjust[[units]][[region_figure_tau_token(tau)]]
    )
    lims <- frame$lims
    ticks <- frame$ticks
    holds <- function(v) {
      all(vapply(axes, function(k) {
        v[[k]] >= lims[[k]][1L] && v[[k]] <= lims[[k]][2L]
      }, logical(1)))
    }
    stopifnot(
      holds(box0$lo), holds(box0$hi),
      is.null(marked) || holds(marked)
    )
    tick_labels <- Map(function(at, places) {
      paste0("$", formatC(at, format = "f", digits = places), "$")
    }, ticks, frame$digits)

    svglite::svglite(
      filename = artifact_path(region_figure_id(ols, units, tau)),
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

    draw_region_projections(pmat, point0, offsets, palette$tau0_point, 21)
    if (!is.null(marked)) {
      draw_region_projections(
        pmat, marked, offsets, palette$ols_point, palette$ols_pch
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
    if (!is.null(marked)) {
      draw_region_point(pmat, marked,
        pch = palette$ols_pch, bg = palette$ols_point,
        col = "black", cex = 1.35, lwd = 0.5
      )
    }

    draw_region_axes(
      pmat, lo, hi, ticks, tick_labels,
      render$axis_labels[[units]], (lo + hi) / 2
    )
  }
  # See region_3d_draw_or_skip (prepare_region_geometry.R): region_envelope
  # handles the observed non-convex cases directly, so this only fires on a
  # genuine two-piece split -- not yet seen for any (tau, axis, instrument)
  # tested, but a data fact this renderer still cannot draw if it occurs.
  skipped_ids <- character(0)
  for (units in REGION_FIGURE_UNITS) {
    for (tau in region_taus) {
      for (ols in REGION_FIGURE_OLS) {
        skip_id <- region_3d_draw_or_skip(draw, ols, units, tau)
        if (!is.null(skip_id)) skipped_ids <- c(skipped_ids, skip_id)
      }
    }
  }
  skipped_ids
}) -> region_3d_skipped_ids

for (id in artifact_manifest$id[artifact_manifest$producer ==
  "mean_equation/figures/render_region_3d.R"]) {
  status <- if (id %in% region_3d_skipped_ids) "skipped (non-convex)" else "wrote"
  cat("set_id_region_3d:", status, artifact_path(id), "\n")
}
rm(id, region_3d_skipped_ids)
