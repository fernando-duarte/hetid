# Calibrations and robust endpoint summaries used by identified-set inference.

# Imbens-Manski interpolation between two-sided and one-sided critical values.
im_critical <- function(
  width, se_lower, se_upper,
  alpha = PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha,
  control = PAPER_INFERENCE_SEARCH_CONTROL
) {
  stopifnot(length(width) == 1L, alpha > 0, alpha < 1)
  s <- max(se_lower, se_upper)
  if (!is.finite(s) || s <= 0) {
    return(stats::qnorm(1 - alpha))
  }
  delta <- max(width, 0) / s
  f <- function(c) stats::pnorm(c + delta) - stats::pnorm(-c) - (1 - alpha)
  root <- control$im_root
  stats::uniroot(
    f,
    interval = c(root$bracket_lower, root$bracket_upper),
    tol = root$tolerance
  )$root
}

# P(X <= a, Y >= b) for a standard bivariate normal pair.
pbvn_le_ge <- function(a, b, rho, control = PAPER_INFERENCE_SEARCH_CONTROL) {
  stopifnot(is.finite(rho), abs(rho) <= 1)
  cutoff <- control$bvn$degenerate_correlation
  if (rho > cutoff) {
    return(max(0, stats::pnorm(a) - stats::pnorm(b)))
  }
  if (rho < -cutoff) {
    return(stats::pnorm(min(a, -b)))
  }
  f <- function(t) {
    stats::dnorm(t) * (1 - stats::pnorm((b - rho * t) / sqrt(1 - rho^2)))
  }
  stats::integrate(
    f,
    -Inf,
    a,
    rel.tol = control$bvn$integration_relative_tolerance
  )$value
}

# Stoye joint calibration with a common critical value for both endpoints.
stoye_critical <- function(
  width, se_lower, se_upper, rho,
  alpha = PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha,
  control = PAPER_INFERENCE_SEARCH_CONTROL
) {
  stopifnot(length(width) == 1L, alpha > 0, alpha < 0.5)
  if (!is.finite(se_lower) || !is.finite(se_upper) ||
    se_lower <= 0 || se_upper <= 0 || !is.finite(rho)) {
    return(stats::qnorm(1 - alpha))
  }
  stopifnot(abs(rho) <= 1)
  w <- max(width, 0)
  cov_at <- function(c) {
    min(
      pbvn_le_ge(c, -c - w / se_upper, rho, control),
      pbvn_le_ge(c, -c - w / se_lower, rho, control)
    )
  }
  root <- control$stoye_root
  stats::uniroot(
    function(c) cov_at(c) - (1 - alpha),
    interval = c(root$bracket_lower, root$bracket_upper),
    tol = root$tolerance
  )$root
}

# Normal-consistent median absolute deviation of the finite draws.
robust_scale <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) {
    return(NA_real_)
  }
  stats::mad(x)
}

# Pearson correlation after a robust, jointly paired MAD trim.
robust_endpoint_cor <- function(
  lower, upper, control = PAPER_INFERENCE_SEARCH_CONTROL
) {
  ok <- is.finite(lower) & is.finite(upper)
  l <- lower[ok]
  u <- upper[ok]
  cor_control <- control$robust_endpoint_correlation
  if (length(l) < cor_control$minimum_pairs) {
    return(NA_real_)
  }
  s_l <- stats::mad(l)
  s_u <- stats::mad(u)
  if (!is.finite(s_l) || !is.finite(s_u) || s_l <= 0 || s_u <= 0) {
    return(NA_real_)
  }
  trim <- cor_control$mad_trim_multiple
  keep <- abs(l - stats::median(l)) < trim * s_l &
    abs(u - stats::median(u)) < trim * s_u
  if (sum(keep) < cor_control$minimum_pairs) {
    return(NA_real_)
  }
  stats::cor(l[keep], u[keep])
}
