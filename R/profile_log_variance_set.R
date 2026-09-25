#' Profile the Log-Variance Coefficients Over an Identified Set
#'
#' Fits the log-variance equation at points of the mean-equation
#' identified set and reports, for each volatility coefficient, the range
#' the fits span. This is the slack-\eqn{\tau} counterpart of
#' \code{\link{fit_log_variance_at_b}}, which fits at a single \eqn{b}.
#'
#' @details
#' Candidate points are the box's attaining witnesses together with steps
#' from the center toward each of them, and every candidate is re-checked
#' against the constraints before it is fitted, so no fit is ever run
#' outside the set up to a constraint-relative feasibility tolerance.
#' Nonfinite candidate-membership arithmetic raises a structured error rather
#' than admitting or silently dropping a point.
#'
#' Fits that fail are skipped rather than fatal, and the counts are
#' reported. Skipping can only narrow the reported range, never widen it:
#' the range is over points that were fitted successfully, and every one
#' of those satisfies the same relative feasibility check. Warm starts use the last
#' \emph{successful} fit; both registered estimators minimize a convex
#' criterion, so a start affects whether a fit converges but never which
#' answer it converges to.
#'
#' @section Interpretation:
#' The range is \strong{attained over the sampled points}, not the profile
#' over the whole set. It is an inner approximation on both counts: the
#' box itself is one, and the sample is finite. Raising \code{n_points}
#' or the box's \code{n_grid} samples different points and can reveal wider
#' ranges; the reported range need not grow.
#'
#' @param box A \code{hetid_theta_box} from
#'   \code{\link{compute_identified_set_box}}
#' @param x_var Volatility-equation design, without an intercept column.
#'   This is a different design from the mean equation's \code{x}
#' @param estimator Estimator id passed through to
#'   \code{\link{fit_log_variance_at_b}}, which owns the valid set
#' @param n_points Steps from the center toward each witness; defaults to
#'   \code{IDENTIFIED_SET_CONTROL$N_POINTS}
#' @return A data frame with \code{term}, \code{lower} and \code{upper},
#'   one row per volatility coefficient, all \code{NA} when no candidate
#'   could be fitted or the box has an infinite side. Attributes
#'   \code{n_attempted}, \code{n_failed} and \code{estimator} record the
#'   sampling. Use \code{\link{sample_log_variance_set}} to retain joint fits
#'   and predict sampled envelopes.
#' @seealso \code{\link{compute_identified_set_box}} for the box,
#'   \code{\link{fit_log_variance_at_b}} for the single-\eqn{b} fit
#' @export
#'
#' @examples
#' set.seed(42)
#' n_obs <- 200
#' x <- cbind(x1 = rnorm(n_obs), x2 = rnorm(n_obs))
#' z <- rnorm(n_obs)
#' e2 <- sqrt(exp(0.5 + 0.9 * z)) * matrix(rnorm(n_obs * 2), n_obs, 2)
#' y2 <- x %*% matrix(c(1, 0.5, -0.3, 0.7), 2, 2) + e2
#' colnames(y2) <- c("news1", "news2")
#' y1 <- drop(0.3 + x %*% c(0.2, -0.1) + y2 %*% c(0.8, -0.5) + rnorm(n_obs))
#' x_var <- cbind(v1 = rnorm(n_obs), v2 = rnorm(n_obs))
#'
#' fit <- compute_tau0_system(y1, y2, x, z)
#' box <- compute_identified_set_box(fit, tau = 0.05, n_grid = 11L)
#' profile_log_variance_set(box, x_var)
profile_log_variance_set <- function(box, x_var, estimator = "ppml",
                                     n_points =
                                       IDENTIFIED_SET_CONTROL$N_POINTS) {
  assert_hetid_theta_box(box)
  assert_scalar_integer_in_range(n_points, "n_points", 1, .Machine$integer.max)
  coef_labels <- colnames(log_variance_design(x_var))

  candidates <- profile_set_candidates(box, n_points)
  if (is.null(candidates)) {
    return(empty_log_variance_profile(coef_labels, 0L, 0L, estimator))
  }
  fits <- fit_over_candidates(candidates, box, x_var, estimator)
  log_variance_profile_bounds(fits, nrow(candidates), coef_labels, estimator)
}
