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
  # viridis at the same fractions as the original (0.12, 0.435, 0.75):
  # purple / teal / green
  tcols <- c("#472B7A", "#26818E", "#5DC863")
  m <- 300L
  axes <- seq_len(dimension)
  labs <- lapply(axes, function(k) bquote(tilde(b)[.(k) * "," * N]))
  panels <- lapply(axes, function(perp) {
    keep <- setdiff(axes, perp)
    list(perp = perp, x = keep[1], y = keep[2])
  })

  systems <- lapply(taus, region_sd_system)
  point0 <- region_sd_point()

  # Shared ranges use the widest displayed projection slack.
  widest_tau <- max(taus)
  box2 <- region_sd_box(widest_tau)
  pad <- function(r) r + c(-1, 1) * 0.06 * diff(r)
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

  grDevices::svg(artifact_path("mean_projections_figure"),
    width = 11, height = 4.7
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::layout(matrix(c(1, 2, 3, 4, 4, 4), 2, 3, byrow = TRUE),
    heights = c(6, 1)
  )
  graphics::par(oma = c(0, 0, 2, 0)) # device-scoped; dies at dev.off

  for (pi in seq_along(panels)) {
    p <- panels[[pi]]
    graphics::par(mar = c(4, 4.2, 2.2, 1), pty = "s")
    plot(NA,
      xlim = xr, ylim = yr, xlab = labs[[p$x]], ylab = labs[[p$y]],
      main = bquote("project out " * tilde(b)[.(p$perp) * "," * N]),
      cex.main = 1
    )
    graphics::grid(col = "grey92", lty = 1)
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
      col = "#d81b3f", cex = 1.3, lwd = 1.6
    )
  }

  graphics::par(mar = c(0, 0, 0, 0), pty = "m")
  plot.new()
  legend_items <- c(
    sprintf("tau = %g set boundary", taus),
    "marginal interval (per tau)", "tau = 0 point"
  )
  legend("center",
    horiz = TRUE, bty = "n", cex = 1.0,
    legend = legend_items,
    col = c(tcols, "grey40", "#d81b3f"),
    lty = c(1, 1, 1, 2, NA), lwd = c(2, 2, 2, 1, NA),
    pch = c(NA, NA, NA, NA, 21), pt.bg = "white", pt.lwd = 1.6
  )
  graphics::mtext(
    "Projected identified sets in SD units: set boundaries and marginal intervals by tau",
    outer = TRUE, cex = 1.05, line = 0.2
  )
})

cat(
  "set_id_projections: wrote", artifact_path("mean_projections_figure"),
  "\n"
)
