# Per-unit-system panel geometry for the projected identified-set figures.
# Split from render_projections.R for the repository line cap: that file owns
# the drawing and this one owns what is drawn -- axis labels and ranges, the
# grid each panel is evaluated on, the closed-form envelope margin per
# (panel, tau), and the marginal identified interval per coefficient.
#
# region_tick_digits comes from the 3D frames module: a projection range spans
# coefficients of different magnitudes, and in raw b units a fixed one decimal
# collapses neighbouring tick labels into the same text.
paper_source_once(paper_path("mean_equation", "figures", "region_3d_frames.R"))

projection_axis_labels <- function(units, axes) {
  lapply(axes, function(k) {
    if (identical(units, "sd")) {
      sprintf("$\\sigma(PC_{%d,N})\\, b_{%d,N}$", k, k)
    } else {
      sprintf("$b_{%d,N}$", k)
    }
  })
}

# Ladder for a frame. A ladder needing three decimals -- raw b_{1,N}, whose
# whole range is 0.026 wide -- prints labels too wide for a square panel at four
# steps, so it drops to the next coarser pretty() step. One- and two-decimal
# ladders keep their four.
projection_ladder <- function(r) {
  at <- pretty(r, 4)
  if (region_tick_digits(at) >= 3L) {
    at <- pretty(r, 3)
  }
  at
}

# strictly inside the frame, so no label sits on the box line
projection_ticks <- function(r) {
  at <- projection_ladder(r)
  at[at > r[1] & at < r[2]]
}

# Frame and ladder carried together, the frame reaching out for a rung that
# only just misses it. pretty() brackets the span it is given, so its end rungs
# always sit outside -- but by wildly different amounts. 0.10 clears the padded
# b_{2,N} frame by 6% of a step and 0 clears b_{1,N} by 6%, while -0.15 and -1.0
# clear theirs by 96% and 83%. Reaching a quarter of a step takes in the first
# kind, which costs a sliver of whitespace and buys an endpoint label, and
# leaves the second kind clipped, which is what keeps b_{3,N} off [-1, 0].
#
# The ladder travels with the frame rather than being recomputed from it. Asking
# pretty() again for the widened frame is not a fixpoint: it answers with a
# wider ladder whose new end rung again falls outside, so a recomputing version
# clips exactly the tick this is here to keep.
projection_widened_axis <- function(r) {
  at <- projection_ladder(r)
  reach <- 0.25 * diff(at)[1L]
  lo <- min(at[at >= r[1] - reach])
  hi <- max(at[at <= r[2] + reach])
  at <- at[at >= lo & at <= hi]
  margin <- 0.01 * diff(r)
  frame <- c(min(r[1], lo - margin), max(r[2], hi + margin))
  # Then centre the ladder in the frame, growing only the short side. b_{1,N}
  # ends its ticks at 0.02 well before a frame reaching 0.026, which reads as
  # the labels being shoved to the left rather than sitting under the axis.
  mid <- mean(range(at))
  half <- max(mid - frame[1], frame[2] - mid)
  list(range = c(mid - half, mid + half), ticks = at)
}

projection_tick_labels <- function(v) {
  paste0("$", formatC(v, format = "f", digits = region_tick_digits(v)), "$")
}

# Ranges come off the widest displayed projection slack. The SD axes are
# commensurate, so every panel takes one x-range and one y-range and the row
# reads as a grid. Raw b coefficients span two orders of magnitude, where that
# layout leaves b_{1,N} in a sliver of its axis, so there each coefficient
# carries ONE range wherever it appears -- the same coefficient then covers the
# same interval in both panels that show it.
#
# The OLS benchmark joins the set box before padding. It sits outside the raw-b
# b_{3,N} box by less than the marker's own radius, and a range built from the
# box alone drew it straddling the top rule. Both variants share one geometry
# pass, so the plain figure carries the same headroom for a point it does not
# draw; recomputing the envelopes per variant would cost far more than the
# whitespace.
projection_panel_axes <- function(units, box, ols, axes, padding) {
  span <- function(k) {
    r <- range(box$lo[k], box$hi[k], ols[k])
    r + c(-1, 1) * padding * diff(r)
  }
  if (identical(units, "sd")) {
    # the SD pair shares one frame across panels and is already published
    # against the plain clip, so it keeps the frame its data span gives
    shared <- lapply(list(x = span(1:2), y = span(2:3)), function(r) {
      list(range = r, ticks = projection_ticks(r))
    })
    return(function(k, side) shared[[side]])
  }
  per_coef <- lapply(axes, function(k) projection_widened_axis(span(k)))
  stopifnot(vapply(per_coef, function(a) {
    length(a$ticks) >= 2L &&
      all(a$ticks > a$range[1] & a$ticks < a$range[2])
  }, logical(1)))
  function(k, side) per_coef[[k]]
}

projection_panel_geometry <- function(units, taus, panels, axes, render) {
  scale <- region_axis_scale(units)
  systems <- lapply(taus, region_sd_system, s = scale)
  point0 <- region_sd_point(scale)
  ols_point <- region_sd_ols_point(scale)
  panel_axis <- projection_panel_axes(
    units, region_sd_box(max(taus), scale), ols_point, axes,
    render$range_padding
  )
  # every marker the renderer draws has to land inside the panel drawing it,
  # or it is clipped to the frame and reads as a point on the boundary
  for (p in panels) {
    for (kv in list(c(p$x, "x"), c(p$y, "y"))) {
      k <- as.integer(kv[1])
      r <- panel_axis(k, kv[2])$range
      stopifnot(
        point0[k] >= r[1], point0[k] <= r[2],
        ols_point[k] >= r[1], ols_point[k] <= r[2]
      )
    }
  }
  m <- render$grid_points
  grids <- lapply(panels, function(p) {
    xr <- panel_axis(p$x, "x")$range
    yr <- panel_axis(p$y, "y")$range
    xg <- seq(xr[1], xr[2], length.out = m)
    yg <- seq(yr[1], yr[2], length.out = m)
    list(xg = xg, yg = yg, cells = region_grid(xg, yg))
  })
  # envelope margin per (panel, tau), cached so the drawn contour and the box
  # are computed from one source
  envs <- lapply(seq_along(panels), function(pi) {
    p <- panels[[pi]]
    cells <- grids[[pi]]$cells
    lapply(systems, function(s) region_envelope(s, p$perp, cells$X, cells$Y))
  })
  # marginal identified interval per (tau, coordinate): union of the
  # drawn-contour extents over both projections exposing that coordinate, so a
  # single interval is shown for each coefficient and it contains the region in
  # every panel
  marg <- lapply(seq_along(taus), function(ti) {
    mm <- matrix(NA_real_, length(axes), 2L)
    for (pi in seq_along(panels)) {
      p <- panels[[pi]]
      g <- grids[[pi]]
      cl <- grDevices::contourLines(g$xg, g$yg, envs[[pi]][[ti]]$M, levels = 0)
      if (!length(cl)) next
      xs <- unlist(lapply(cl, `[[`, "x"))
      ys <- unlist(lapply(cl, `[[`, "y"))
      mm[p$x, ] <- range(c(mm[p$x, ], xs), na.rm = TRUE)
      mm[p$y, ] <- range(c(mm[p$y, ], ys), na.rm = TRUE)
    }
    mm
  })
  list(
    labs = projection_axis_labels(units, axes),
    point0 = point0,
    ols_point = ols_point,
    panel_axis = panel_axis,
    grids = grids,
    envs = envs,
    marg = marg
  )
}
