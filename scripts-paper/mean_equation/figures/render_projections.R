# Projected identified sets for b_N in standard-deviation units: coordinate-plane
# projections at the contract's projection slack values, each
# with its bounding box (the marginal identified interval per coefficient -- the
# tight axis-aligned bounding box of the projected set) and the tau = 0 point.
# Panels share one x-range and one y-range and are drawn square. The projected
# set boundary is the closed-form zero level of the free-coordinate margin M
# (see prepare_region_geometry.R). The box is taken from the SAME contour geometry
# that is drawn, so it always contains the region -- the reported profile-bound
# intervals come from a local solver that can undershoot the true extent of this
# non-convex plate, which would let the region cross a solver-derived box.
# Writes the projected-set SVG to the typed figure directory.
# Run after the identified set and shared region geometry are available.

local({
  taus <- PAPER_ANALYSIS_CONTRACT$tau$projection
  dimension <- PAPER_ANALYSIS_CONTRACT$figure$region_dimension
  render <- PAPER_FIGURE_RENDER_CONTROL$projections
  tcols <- render$tau_colors
  tau0_col <- PAPER_FIGURE_STYLE$region$tau0_point
  stopifnot(length(tcols) == length(taus))
  m <- render$grid_points
  axes <- seq_len(dimension)
  labs <- lapply(axes, function(k) sprintf("$\\sigma(PC_{%d,N})\\, b_{%d,N}$", k, k))
  math_num <- function(v) paste0("$", formatC(v, format = "f", digits = 1), "$")
  panels <- lapply(axes, function(perp) {
    keep <- setdiff(axes, perp)
    list(perp = perp, x = keep[1], y = keep[2])
  })

  systems <- lapply(taus, region_sd_system)
  point0 <- region_sd_point()
  ols_point <- region_sd_ols_point()
  ols_col <- PAPER_FIGURE_STYLE$region$ols_point
  ols_pch <- PAPER_FIGURE_STYLE$region$ols_pch

  # Shared ranges use the widest displayed projection slack.
  widest_tau <- max(taus)
  box2 <- region_sd_box(widest_tau)
  pad <- function(r) {
    r + c(-1, 1) * render$range_padding * diff(r)
  }
  xr <- pad(range(box2$lo[1:2], box2$hi[1:2]))
  yr <- pad(range(box2$lo[2:3], box2$hi[2:3]))
  xg <- seq(xr[1], xr[2], length.out = m)
  yg <- seq(yr[1], yr[2], length.out = m)
  grid <- region_grid(xg, yg)

  # envelope margin per (panel, tau), cached so the drawn contour and the box are
  # computed from one source
  envs <- lapply(panels, function(p) {
    lapply(systems, function(s) region_envelope(s, p$perp, grid$X, grid$Y))
  })
  # marginal identified interval per (tau, coordinate): union of the drawn-contour
  # extents over both projections exposing that coordinate, so a single interval
  # is shown for each coefficient and it contains the region in every panel
  marg <- lapply(seq_along(taus), function(ti) {
    mm <- matrix(NA_real_, dimension, 2L)
    for (pi in seq_along(panels)) {
      p <- panels[[pi]]
      cl <- grDevices::contourLines(xg, yg, envs[[pi]][[ti]]$M, levels = 0)
      if (!length(cl)) next
      xs <- unlist(lapply(cl, `[[`, "x"))
      ys <- unlist(lapply(cl, `[[`, "y"))
      mm[p$x, ] <- range(c(mm[p$x, ], xs), na.rm = TRUE)
      mm[p$y, ] <- range(c(mm[p$y, ], ys), na.rm = TRUE)
    }
    mm
  })

  # show_ols adds the OLS benchmark point; every other element is identical, so
  # the two variants share one pass over the cached envelopes
  draw <- function(artifact, show_ols) {
    # The panel row keeps the height it had when a seventh of the device was a
    # dedicated legend row; that row is gone because the legend now rides in the
    # strip the square panels leave above their boxes.
    svglite::svglite(artifact_path(artifact),
      width = 8,
      height = 3.8 * 6 / 7
    )
    on.exit(grDevices::dev.off(), add = TRUE)
    graphics::layout(render$layout)
    graphics::par(oma = c(0, 0, 0, 0)) # no baked title; device-scoped, dies at dev.off

    for (pi in seq_along(panels)) {
      p <- panels[[pi]]
      xt <- pretty(xr, 4)
      xt <- xt[xt > xr[1] & xt < xr[2]]
      yt <- pretty(yr, 4)
      yt <- yt[yt > yr[1] & yt < yr[2]]
      graphics::par(mar = c(4.6, 5, 1.2, 0.8), pty = "s", cex.lab = 0.8)
      plot(NA,
        xlim = xr, ylim = yr, xlab = labs[[p$x]], ylab = labs[[p$y]],
        main = "", axes = FALSE
      )
      graphics::box()
      graphics::axis(1, at = xt, labels = math_num(xt), cex.axis = 0.8)
      graphics::axis(2, at = yt, labels = math_num(yt), cex.axis = 0.8)
      for (ti in seq_along(taus)) {
        graphics::contour(xg, yg, envs[[pi]][[ti]]$M,
          levels = 0, add = TRUE,
          drawlabels = FALSE, col = tcols[ti], lwd = 2
        )
        mm <- marg[[ti]]
        graphics::rect(mm[p$x, 1], mm[p$y, 1], mm[p$x, 2], mm[p$y, 2],
          border = tcols[ti], lty = 2, lwd = 1
        )
      }
      graphics::points(point0[p$x], point0[p$y],
        pch = 21, bg = "white",
        col = tau0_col, cex = 1.3, lwd = 1.6
      )
      if (show_ols) {
        graphics::points(ols_point[p$x], ols_point[p$y],
          pch = ols_pch, bg = ols_col,
          col = "black", cex = 1.3, lwd = 0.8
        )
      }
    }

    # par("plt")[4] is the top of the square plot box as a fraction of the panel
    # region, so the leftover strip above it spans the device and holds the
    # legend in one row: the point estimates, then the sets, then their boxes
    strip <- graphics::par("plt")[4]
    # drop the square aspect before claiming the strip, or the region collapses
    # to a 32.66pt box in the middle of the device
    graphics::par(mar = c(0, 0, 0, 0), pty = "m")
    graphics::par(fig = c(0, 1, strip, 1), new = TRUE)
    plot.new()
    legend_items <- c(
      "$\\tau = 0$", if (show_ols) "OLS",
      sprintf("$\\tau = %s$", paper_format_tau(taus)), "marginal interval"
    )
    legend("center",
      ncol = length(legend_items), bty = "n", cex = 1.0,
      legend = legend_items,
      col = c(tau0_col, if (show_ols) "black", tcols, "grey40"),
      lty = c(NA, if (show_ols) NA, 1, 1, 1, 2),
      lwd = c(NA, if (show_ols) NA, 2, 2, 2, 1),
      pch = c(21, if (show_ols) ols_pch, NA, NA, NA, NA),
      pt.bg = c("white", if (show_ols) ols_col, NA, NA, NA, NA),
      pt.lwd = 1.6
    )
  }
  draw("mean_projections_figure", FALSE)
  draw("mean_projections_ols_figure", TRUE)
})

cat(
  "set_id_projections: wrote", artifact_path("mean_projections_figure"),
  "and", artifact_path("mean_projections_ols_figure"),
  "\n"
)
