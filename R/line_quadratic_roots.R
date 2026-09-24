#' Real Roots Retaining Constraint Identity and Multiplicity
#'
#' Explicitly separates constant, linear, and quadratic constraints. A
#' nonzero leading coefficient remains quadratic, however small it is.
#' The discriminant is computed in units of a power of two near
#' \eqn{\max(|\beta|, \sqrt{|a|}\sqrt{|\gamma|})}, avoiding the
#' overflow or underflow of squaring the original coefficients. Binary
#' scaling avoids additional rounding from an arbitrary normalization.
#'
#' The cancellation-resistant formula uses a scaled
#' \eqn{q = -(\beta/m + s\sqrt{disc/m^2})/2}, where \eqn{s} is one
#' for nonnegative \eqn{\beta} and minus one otherwise. Roots are
#' \eqn{qm/a} and \eqn{\gamma/(mq)}. Alternative grouping avoids an
#' intermediate overflow or underflow when the final root is representable.
#' Finite roots outside the numeric range fail rather than masquerading as
#' unboundedness. Root rounding near a repeated root remains unavoidable.
#'
#' @param coefs Numeric matrix with columns a, beta, gamma
#' @return Numeric two-column matrix, one row per constraint. Missing roots
#'   are \code{NA}; a double root appears twice so it cannot flip a sign
#' @noRd
line_quadratic_roots <- function(coefs) {
  if (any(!is.finite(coefs))) {
    stop_hetid("Line constraint coefficients exceed the numeric range")
  }
  a_val <- coefs[, 1]
  beta_val <- coefs[, 2]
  gamma_val <- coefs[, 3]
  roots <- matrix(NA_real_, nrow(coefs), 2L)
  linear <- a_val == 0 & beta_val != 0
  roots[linear, 1L] <- -gamma_val[linear] / beta_val[linear]
  if (any(!is.finite(roots[linear, 1L])) ||
    any(roots[linear, 1L] == 0 & gamma_val[linear] != 0)) {
    stop_hetid("A finite line constraint root is outside the numeric range")
  }
  roots[a_val != 0 & beta_val == 0 & gamma_val == 0, ] <- 0
  rows <- which(a_val != 0 & (beta_val != 0 | gamma_val != 0))
  if (length(rows)) {
    roots[rows, ] <- scaled_quadratic_roots(
      a_val[rows], beta_val[rows], gamma_val[rows]
    )
  }
  roots
}

#' Scaled Roots of Constraints with Nonzero Curvature
#'
#' @param a_val,beta_val,gamma_val Coefficient vectors; a is nonzero and
#'   beta and gamma are not both zero
#' @return Two-column root matrix with NA for negative discriminants
#' @noRd
scaled_quadratic_roots <- function(a_val, beta_val, gamma_val) {
  product <- sqrt(abs(a_val)) * sqrt(abs(gamma_val))
  exponent <- pmin(
    floor(log2(pmax(abs(beta_val), product))), .Machine$double.max.exp - 1
  )
  root_scale <- 2^exponent
  if (any(!is.finite(root_scale)) || any(root_scale == 0)) {
    stop_hetid("Line constraint root scaling exceeds the numeric range")
  }
  ac_scaled <- (a_val / root_scale) * (gamma_val / root_scale)
  fallback <- !is.finite(ac_scaled)
  ac_scaled[fallback] <- sign(a_val[fallback]) * sign(gamma_val[fallback]) *
    (product[fallback] / root_scale[fallback])^2
  disc <- (beta_val / root_scale)^2 - 4 * ac_scaled
  roots <- matrix(NA_real_, length(a_val), 2L)
  keep <- disc >= 0
  q_val <- -(beta_val[keep] / root_scale[keep] +
    ifelse(beta_val[keep] >= 0, 1, -1) * sqrt(disc[keep])) / 2
  first <- q_val * (root_scale[keep] / a_val[keep])
  retry <- !is.finite(first) | first == 0
  first[retry] <- (q_val[retry] * root_scale[keep][retry]) / a_val[keep][retry]
  second <- (gamma_val[keep] / root_scale[keep]) / q_val
  retry <- !is.finite(second) | (second == 0 & gamma_val[keep] != 0)
  second[retry] <- (gamma_val[keep][retry] / q_val[retry]) / root_scale[keep][retry]
  second[disc[keep] == 0] <- first[disc[keep] == 0]
  if (any(!is.finite(first)) || any(!is.finite(second)) ||
    any(first == 0) || any(second == 0 & gamma_val[keep] != 0)) {
    stop_hetid("A finite line constraint root is outside the numeric range")
  }
  roots[keep, ] <- cbind(first, second)
  roots
}
