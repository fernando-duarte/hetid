# Sampling-uncertainty helpers for the set-identified coefficients: percentile
# bands over bootstrap draws, and confidence intervals for the true
# coefficient anchored at the exact identified-set endpoints with robust
# (MAD-based) endpoint scales. The interval uses a Stoye (2009)-style joint
# calibration when the endpoint correlation is estimable, falling back to the
# Imbens-Manski (2004) interpolation otherwise. Consumed by
# scripts-paper/set_id_bootstrap.R; tested in
# scripts/utils/tests/test_set_id_inference.R.

# Median and 90% percentile band of the finite draws (the house p05/p95 form).
boot_band <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(c(median = NA_real_, p05 = NA_real_, p95 = NA_real_, n = 0))
  }
  c(
    median = stats::median(x),
    p05 = unname(stats::quantile(x, 0.05)),
    p95 = unname(stats::quantile(x, 0.95)),
    n = length(x)
  )
}

# Imbens-Manski critical value: c solves Phi(c + width/s) - Phi(-c) = 1 - alpha
# with s the larger endpoint standard error. Interpolates between the two-sided
# normal quantile at width 0 and the one-sided quantile as the set widens.
# A degenerate s falls back to the one-sided value (the interval then adds
# nothing beyond the exact set).
im_critical <- function(width, se_lower, se_upper, alpha = 0.10) {
  stopifnot(length(width) == 1L, alpha > 0, alpha < 1)
  s <- max(se_lower, se_upper)
  if (!is.finite(s) || s <= 0) {
    return(stats::qnorm(1 - alpha))
  }
  delta <- max(width, 0) / s
  f <- function(c) stats::pnorm(c + delta) - stats::pnorm(-c) - (1 - alpha)
  stats::uniroot(f, interval = c(0, 10), tol = 1e-10)$root
}

# P(X <= a, Y >= b) for a standard bivariate normal pair with correlation rho,
# via one-dimensional integration (keeps the dependency footprint at base R).
# Near-degenerate correlations use the comonotone closed forms instead of
# integrating a near-step function.
pbvn_le_ge <- function(a, b, rho) {
  stopifnot(is.finite(rho), abs(rho) <= 1)
  if (rho > 0.999) {
    return(max(0, stats::pnorm(a) - stats::pnorm(b)))
  }
  if (rho < -0.999) {
    return(stats::pnorm(min(a, -b)))
  }
  f <- function(t) {
    stats::dnorm(t) * (1 - stats::pnorm((b - rho * t) / sqrt(1 - rho^2)))
  }
  stats::integrate(f, -Inf, a, rel.tol = 1e-9)$value
}

# Stoye (2009)-style joint calibration: the common critical value c at which
# the bivariate-normal coverage of the true value, evaluated at whichever
# endpoint binds, equals 1 - alpha. Removes the Imbens-Manski superefficient-
# width assumption; coincides with the IM interpolation as the endpoint errors
# become comonotone (the binding case -- the two one-sided misses are then
# disjoint) and shortens as they decouple. Degenerate inputs fall back to the
# one-sided value, like im_critical.
stoye_critical <- function(width, se_lower, se_upper, rho, alpha = 0.10) {
  stopifnot(length(width) == 1L, alpha > 0, alpha < 0.5)
  if (!is.finite(se_lower) || !is.finite(se_upper) ||
    se_lower <= 0 || se_upper <= 0 || !is.finite(rho)) {
    return(stats::qnorm(1 - alpha))
  }
  stopifnot(abs(rho) <= 1)
  w <- max(width, 0)
  cov_at <- function(c) {
    min(
      pbvn_le_ge(c, -c - w / se_upper, rho),
      pbvn_le_ge(c, -c - w / se_lower, rho)
    )
  }
  # coverage rises with c and sits below 1 - alpha near c = 0 for any alpha
  # under one half (the binding side is at most one half there), so the
  # bracket always straddles the root
  stats::uniroot(
    function(c) cov_at(c) - (1 - alpha),
    interval = c(1e-6, 8), tol = 1e-8
  )$root
}

# Robust endpoint scale: the normal-consistent median absolute deviation of
# the finite draws. The plain standard deviation has a zero breakdown point,
# and near the boundedness frontier one near-degenerate resample inflates it
# by orders of magnitude, so every reported standard error uses this scale.
robust_scale <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) {
    return(NA_real_)
  }
  stats::mad(x)
}

# Robust endpoint correlation: Pearson on the draw pairs jointly within six
# robust scales of their medians (the raw correlation inherits the same
# single-draw fragility as the raw standard deviation).
robust_endpoint_cor <- function(lower, upper) {
  ok <- is.finite(lower) & is.finite(upper)
  l <- lower[ok]
  u <- upper[ok]
  if (length(l) < 8L) {
    return(NA_real_)
  }
  s_l <- stats::mad(l)
  s_u <- stats::mad(u)
  if (!is.finite(s_l) || !is.finite(s_u) || s_l <= 0 || s_u <= 0) {
    return(NA_real_)
  }
  keep <- abs(l - stats::median(l)) < 6 * s_l & abs(u - stats::median(u)) < 6 * s_u
  if (sum(keep) < 8L) {
    return(NA_real_)
  }
  stats::cor(l[keep], u[keep])
}

# Robust endpoint scales and a Stoye-calibrated nominal interval for every
# coefficient at one display tau. lower/upper are B x n_coef matrices of
# endpoint draws (NA marks a failed or uncertified draw); set_table is the
# full-sample coef/set_lower/set_upper/status frame the draws re-estimate.
# Rows without a certified nondegenerate full-sample interval, or with fewer
# than min_reps finite draw pairs, keep their scales but report NA intervals
# (degenerate width-0 rows are point-identified; their cells stay blank).
# When the endpoint correlation cannot be estimated the Imbens-Manski
# interpolation is used in place of the Stoye calibration.
endpoint_inference <- function(lower, upper, set_table, alpha = 0.10,
                               min_reps = nrow(lower) %/% 2L) {
  stopifnot(identical(dim(lower), dim(upper)), ncol(lower) == nrow(set_table))
  rows <- lapply(seq_len(nrow(set_table)), function(k) {
    ok <- is.finite(lower[, k]) & is.finite(upper[, k])
    se_lo <- robust_scale(lower[ok, k])
    se_up <- robust_scale(upper[ok, k])
    rho <- robust_endpoint_cor(lower[, k], upper[, k])
    width <- set_table$set_upper[k] - set_table$set_lower[k]
    usable <- set_table$status[k] == "bounded" && is.finite(width) &&
      width > 0 && sum(ok) >= min_reps &&
      is.finite(se_lo) && is.finite(se_up) && se_lo > 0 && se_up > 0
    if (usable) {
      c_im <- im_critical(width, se_lo, se_up, alpha)
      c_st <- if (is.finite(rho)) {
        stoye_critical(width, se_lo, se_up, rho, alpha)
      } else {
        c_im
      }
      ci_lo <- set_table$set_lower[k] - c_st * se_lo
      ci_up <- set_table$set_upper[k] + c_st * se_up
    } else {
      c_im <- NA_real_
      c_st <- NA_real_
      ci_lo <- NA_real_
      ci_up <- NA_real_
    }
    data.frame(
      coef = set_table$coef[k], se_lower = se_lo, se_upper = se_up,
      rho = rho, n_finite = sum(ok), c_im = c_im, c_stoye = c_st,
      ci_lower = ci_lo, ci_upper = ci_up,
      row.names = NULL, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
