# Cube frames and tick ladders for the 3D identified-region figures. Two slacks
# of the SD-unit family carry frames hand-tuned for the paper; every other slack
# and unit system derives its frame from its own padded set box.

# Fewest decimals that render every tick exactly. The hand-set frames print a
# fixed two, but an auto ladder can be finer: raw b_{1,N} ticks land on
# thousandths, where two decimals would collapse neighbouring labels into the
# same text.
region_tick_digits <- function(at) {
  for (places in 0:6) {
    if (all(abs(at - round(at, places)) < 1e-9)) {
      return(places)
    }
  }
  6L
}

# Ladder for one axis: the coarsest pretty() ladder that still leaves n_min
# ticks inside the frame. pretty() puts its end values outside the range as
# often as not, so clipping its n_min-tick ladder back to the frame can strand a
# single label on the axis; asking for a denser one and clipping that recovers a
# readable ladder without letting a tick escape the cube.
region_auto_ticks <- function(lim, n_min) {
  at <- numeric(0)
  for (n in seq(n_min, 3L * n_min)) {
    at <- pretty(lim, n = n)
    at <- at[at >= lim[1] & at <= lim[2]]
    if (length(at) >= n_min) {
      break
    }
  }
  at
}

# Frame from the padded set box, nudged by any per-figure adjustment, grown to
# hold a marked point, on a pretty ladder clipped back inside the frame.
region_3d_auto_frame <- function(lims, render, extra, adjust = NULL) {
  if (!is.null(adjust$x_lower)) {
    lims[[1]][1] <- adjust$x_lower
  }
  if (!is.null(adjust$x_upper)) {
    lims[[1]][2] <- adjust$x_upper
  }
  if (!is.null(adjust$y_lower_drop)) {
    lims[[2]][1] <- lims[[2]][1] - adjust$y_lower_drop
  }
  if (!is.null(extra)) {
    lims <- lapply(seq_along(lims), function(k) {
      pad <- render$limit_padding * diff(lims[[k]])
      c(
        min(lims[[k]][1], extra[[k]] - pad),
        max(lims[[k]][2], extra[[k]] + pad)
      )
    })
  }
  ticks <- lapply(lims, region_auto_ticks, n_min = render$auto_tick_n)
  list(
    lims = lims,
    ticks = ticks,
    digits = vapply(ticks, region_tick_digits, integer(1))
  )
}

# Grows one axis's limits/ticks just enough to contain `value` with padding,
# only on the ends listed in `sides` — the caller keeps any hand-tuned end
# fixed by leaving it out.
region_3d_grow_axis <- function(lims, ticks, k, value, padding, sides = c("lo", "hi")) {
  span <- diff(lims[[k]])
  needed_hi <- value + padding * span
  needed_lo <- value - padding * span
  grow_hi <- "hi" %in% sides && needed_hi > lims[[k]][2]
  grow_lo <- "lo" %in% sides && needed_lo < lims[[k]][1]
  if (!grow_hi && !grow_lo) {
    return(list(lims = lims[[k]], ticks = ticks[[k]]))
  }
  step <- diff(ticks[[k]])[1L]
  if (grow_hi) {
    lims[[k]][2] <- max(lims[[k]][2], needed_hi)
    top <- max(ticks[[k]])
    n_extra <- max(floor((lims[[k]][2] - top) / step), 0)
    ticks[[k]] <- c(ticks[[k]], top + step * seq_len(n_extra))
  }
  if (grow_lo) {
    lims[[k]][1] <- min(lims[[k]][1], needed_lo)
    bottom <- min(ticks[[k]])
    n_extra <- max(floor((bottom - lims[[k]][1]) / step), 0)
    ticks[[k]] <- c(bottom - step * rev(seq_len(n_extra)), ticks[[k]])
  }
  list(lims = lims[[k]], ticks = ticks[[k]])
}

# Cube frame for one slack. "baseline" pins two hand-set endpoints (axis 1
# upper, axis 2 lower) and grows every axis's free end to hold the OLS point,
# continuing each ladder at its own spacing over the headroom. "widest" takes
# its hand-set frame and ladder outright. "auto" derives both from the padded
# box, plus this figure's entry in auto_frame_adjust. The caller asserts the
# result still contains everything it draws.
region_3d_frame <- function(lims, render, mode, ols_point, units, tau) {
  if (identical(mode, "auto")) {
    adjust <- render$auto_frame_adjust[[units]][[region_figure_tau_token(tau)]]
    return(region_3d_auto_frame(lims, render, ols_point, adjust))
  }
  ticks <- render$ticks
  if (identical(mode, "baseline")) {
    lims[[1]][2] <- render$manual_limits$x_upper
    lims[[2]][1] <- render$manual_limits$y_lower
    if (!is.null(ols_point)) {
      # axis 1's upper and axis 2's lower are hand-tuned just above and must
      # never move; their free ends (axis 1 lower, axis 2 upper) and axis 3's
      # fully free pair all grow to hold the OLS point.
      baseline_sides <- list("lo", "hi", c("lo", "hi"))
      for (k in seq_along(lims)) {
        grown <- region_3d_grow_axis(
          lims, ticks, k, ols_point[[k]], render$limit_padding,
          sides = baseline_sides[[k]]
        )
        lims[[k]] <- grown$lims
        ticks[[k]] <- grown$ticks
      }
    }
  } else {
    lims <- render$widest_limits
    ticks <- render$widest_ticks
  }
  # A hand-set ladder is shared across the figures of its mode, but the frame
  # around it is each figure's own padded box, so a rung can land outside the
  # cube -- and neither draw_region_panes nor draw_region_axis clips, so it is
  # drawn out there, its gridline crossing open space and its label landing on
  # the neighbouring axis title. Clip, as region_auto_ticks already does for the
  # derived ladders. A ladder left with fewer than two rungs is a frame that has
  # drifted off its figure rather than a sparse axis.
  ticks <- Map(function(at, lim) at[at >= lim[1] & at <= lim[2]], ticks, lims)
  stopifnot(all(lengths(ticks) >= 2L))
  list(
    lims = lims,
    ticks = ticks,
    digits = rep(render$tick_digits, length(ticks))
  )
}
