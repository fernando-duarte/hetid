# Projected identified sets for b_N: coordinate-plane projections at the
# contract's projection slack values, each
# with its bounding box (the marginal identified interval per coefficient -- the
# tight axis-aligned bounding box of the projected set) and the tau = 0 point.
# Panels are drawn square; how they share ranges depends on the unit system
# (projection_panel_geometry.R). The projected
# set boundary is the closed-form zero level of the free-coordinate margin M
# (see prepare_region_geometry.R). The box is taken from the SAME contour geometry
# that is drawn, so it always contains the region -- the reported profile-bound
# intervals come from a local solver that can undershoot the true extent of this
# non-convex plate, which would let the region cross a solver-derived box.
# Writes the projected-set SVGs to the typed figure directory, once per unit
# system: "sd" scales each coefficient by its news-PC standard deviation and "b"
# plots the coefficient itself, on the same axes the 3D region figures use.
# Run after the identified set and shared region geometry are available.

paper_source_once(paper_path(
  "mean_equation", "figures", "projection_panel_geometry.R"
))

local({
  taus <- PAPER_ANALYSIS_CONTRACT$tau$projection
  dimension <- PAPER_ANALYSIS_CONTRACT$figure$region_dimension
  render <- PAPER_FIGURE_RENDER_CONTROL$projections
  tcols <- render$tau_colors
  tau0_col <- PAPER_FIGURE_STYLE$region$tau0_point
  stopifnot(length(tcols) == length(taus))
  axes <- seq_len(dimension)
  panels <- lapply(axes, function(perp) {
    keep <- setdiff(axes, perp)
    list(perp = perp, x = keep[1], y = keep[2])
  })
  ols_col <- PAPER_FIGURE_STYLE$region$ols_point
  ols_pch <- PAPER_FIGURE_STYLE$region$ols_pch
  # One definition per marker, read by both the panel and its legend key, so a
  # key cannot drift from the marker it names. legend() takes point size and
  # border width through pt.cex/pt.lwd rather than cex/lwd, and pt.cex defaults
  # to the text cex: leaving it unset drew both keys at 1.0 against markers at
  # 1.3, and a single pt.lwd drew the OLS key's border at twice its panel width.
  marker_cex <- 1.3
  tau0_lwd <- 1.6
  ols_lwd <- 0.8

  render_units <- function(units) {
    geom <- projection_panel_geometry(units, taus, panels, axes, render)
    labs <- geom$labs
    point0 <- geom$point0
    ols_point <- geom$ols_point
    panel_axis <- geom$panel_axis
    grids <- geom$grids
    envs <- geom$envs
    marg <- geom$marg

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
        g <- grids[[pi]]
        xa <- panel_axis(p$x, "x")
        ya <- panel_axis(p$y, "y")
        xr <- xa$range
        yr <- ya$range
        xt <- xa$ticks
        yt <- ya$ticks
        graphics::par(mar = c(4.6, 5, 1.2, 0.8), pty = "s", cex.lab = 0.8)
        plot(NA,
          xlim = xr, ylim = yr, xlab = labs[[p$x]], ylab = labs[[p$y]],
          main = "", axes = FALSE
        )
        graphics::box()
        # Ticks and their labels are drawn separately because the labels carry a
        # size macro: axis() measures the raw source, decides the ladder steps
        # collide, and silently drops every other one -- gap.axis only softens
        # that, it does not switch it off. mtext draws what it is given, and the
        # ladder is already spaced for the panel width.
        graphics::axis(1, at = xt, labels = FALSE)
        graphics::axis(2, at = yt, labels = FALSE)
        graphics::mtext(projection_tick_labels(xt),
          side = 1, at = xt, line = 1, cex = 0.8
        )
        graphics::mtext(projection_tick_labels(yt),
          side = 2, at = yt, line = 1, cex = 0.8
        )
        for (ti in seq_along(taus)) {
          graphics::contour(g$xg, g$yg, envs[[pi]][[ti]]$M,
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
          col = tau0_col, cex = marker_cex, lwd = tau0_lwd
        )
        if (show_ols) {
          graphics::points(ols_point[p$x], ols_point[p$y],
            pch = ols_pch, bg = ols_col,
            col = "black", cex = marker_cex, lwd = ols_lwd
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
      # The two point markers lead, OLS first, so the row reads markers then
      # sets then boxes. These six vectors are positional: reordering one
      # without the others silently relabels a key.
      legend_items <- c(
        if (show_ols) "OLS", "$\\tau = 0$",
        sprintf("$\\tau = %s$", paper_format_tau(taus)), "marginal interval"
      )
      # Raised a tenth of the strip off its centre line, which opens a gap above
      # the panel boxes and groups the legend with the caption above it.
      #
      # text.width measures the labels WITHOUT their size macro. svglite reserves
      # text at the width of the raw source characters, so letting legend() see
      # the wrapped strings widens the row past the device and pushes the first
      # key off the canvas at negative x. The floor stops a short label -- "OLS",
      # three characters of source that typeset wider than that -- from taking a
      # slot too narrow to clear the next key.
      legend(
        x = 0.5, y = 0.6, xjust = 0.5, yjust = 0.5,
        ncol = length(legend_items), bty = "n", cex = 1.0,
        legend = paste0("{\\footnotesize ", legend_items, "}"),
        text.width = pmax(
          graphics::strwidth(legend_items),
          graphics::strwidth("$\\tau = 0$")
        ),
        col = c(if (show_ols) "black", tau0_col, tcols, "grey40"),
        lty = c(if (show_ols) NA, NA, 1, 1, 1, 2),
        lwd = c(if (show_ols) NA, NA, 2, 2, 2, 1),
        pch = c(if (show_ols) ols_pch, 21, NA, NA, NA, NA),
        pt.bg = c(if (show_ols) ols_col, "white", NA, NA, NA, NA),
        # positional like the five vectors above; the trailing entries are line
        # keys, where pch is NA and these are ignored
        pt.cex = marker_cex,
        pt.lwd = c(
          if (show_ols) ols_lwd, tau0_lwd, rep(tau0_lwd, length(tcols) + 1L)
        )
      )
    }
    for (ols in REGION_FIGURE_OLS) {
      draw(projection_figure_id(ols, units), identical(ols, "projected"))
    }
  }
  for (units in REGION_FIGURE_UNITS) {
    render_units(units)
  }
})

for (id in artifact_manifest$id[artifact_manifest$producer ==
  "mean_equation/figures/render_projections.R"]) {
  cat("set_id_projections: wrote", artifact_path(id), "\n")
}
rm(id)
