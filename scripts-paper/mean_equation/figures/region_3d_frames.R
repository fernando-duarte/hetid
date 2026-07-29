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

# Frame from the padded set box, grown to hold a marked point, on a pretty
# ladder clipped back inside the frame.
region_3d_auto_frame <- function(lims, render, extra) {
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

# Cube frame for one slack. "baseline" overrides two endpoints, keeps its
# hand-set ladder, and grows the third axis to hold the OLS point, continuing
# that ladder at its own spacing over the headroom. "widest" takes its hand-set
# frame and ladder outright. "auto" derives both from the padded box. The caller
# asserts the result still contains everything it draws.
region_3d_frame <- function(lims, render, mode, ols_point) {
  if (identical(mode, "auto")) {
    return(region_3d_auto_frame(lims, render, ols_point))
  }
  ticks <- render$ticks
  if (identical(mode, "baseline")) {
    lims[[1]][2] <- render$manual_limits$x_upper
    lims[[2]][1] <- render$manual_limits$y_lower
    if (!is.null(ols_point)) {
      lims[[3]][2] <- max(
        lims[[3]][2],
        ols_point[[3]] + render$limit_padding * diff(lims[[3]])
      )
      step <- diff(ticks[[3]])[1L]
      top <- max(ticks[[3]])
      n_extra <- max(floor((lims[[3]][2] - top) / step), 0)
      ticks[[3]] <- c(ticks[[3]], top + step * seq_len(n_extra))
    }
  } else {
    lims <- render$widest_limits
    ticks <- render$widest_ticks
  }
  list(
    lims = lims,
    ticks = ticks,
    digits = rep(render$tick_digits, length(ticks))
  )
}
