#' Solve the Point-Identified System at Tau Zero
#'
#' At \eqn{\tau = 0} every maturity constraint degenerates from a quadratic
#' inequality to a linear equality, \eqn{Q_i^\top \theta = L_i}, so stacking
#' the constraints across maturities gives a single linear system for
#' \eqn{\theta}. A full-rank, consistent stack identifies \eqn{\theta} as a
#' point rather than a set; anything else (rank-deficient, under-determined,
#' or inconsistent) is a valid statistical outcome with no point solution.
#'
#' @param components A \code{hetid_components} object from
#'   \code{\link{compute_identified_set_components}}.
#' @param tol Positive numeric scalar, the rank and residual tolerance
#'   (default \code{HETID_CONSTANTS$TAU0_POINT_TOLERANCE}, \code{1e-8}).
#'   Passed to \code{\link[base]{qr}} and \code{\link[base]{qr.solve}} for
#'   the rank check and solve, and scales the residual-consistency gate.
#'
#' @return A list, or \code{NULL} when the stacked system has no unique
#'   consistent point solution (under-determined, rank-deficient, or the
#'   solved \eqn{\theta} does not satisfy the system to within \code{tol}):
#' \describe{
#'   \item{theta}{Numeric vector of length \code{n_components}: the
#'     solved point \eqn{\theta}.}
#'   \item{cond}{Numeric scalar: the condition number of the stacked
#'     \eqn{Q} matrix (\code{\link[base]{kappa}}), a diagnostic for how
#'     sensitive \code{theta} is to perturbations in \code{Q} or \code{L}.}
#' }
#'
#' @details
#' Stacking \code{Q_i} (one row per maturity) into \eqn{Q} and \code{L_i}
#' into \eqn{L} gives:
#' \deqn{Q \theta = L}
#' solved by \code{\link[base]{qr.solve}} when \eqn{Q} is square
#' (\code{nrow(Q) == n_components}) and full rank.
#'
#' @export
#'
#' @examples
#' set.seed(42)
#' n_obs <- 100
#' J <- 3
#' I <- 3
#' w1 <- rnorm(n_obs)
#' w2 <- matrix(rnorm(n_obs * I), nrow = n_obs, ncol = I)
#' pcs <- matrix(rnorm(n_obs * J), nrow = n_obs, ncol = J)
#' gamma <- matrix(rnorm(J * I), nrow = J, ncol = I)
#'
#' moments <- compute_identification_moments(w1, w2, pcs)
#' components <- compute_identified_set_components(gamma, moments)
#' point <- compute_tau0_point(components)
compute_tau0_point <- function(components,
                               tol = HETID_CONSTANTS$TAU0_POINT_TOLERANCE) {
  assert_bad_argument_ok(
    inherits(components, "hetid_components"),
    "components must be a hetid_components object from compute_identified_set_components()",
    arg = "components"
  )
  # public boundary runs the full shape sweep, per the container convention
  validate_hetid_components(components)
  assert_scalar_finite(tol, "tol")
  assert_bad_argument_ok(tol > 0, "tol must be positive", arg = "tol")
  qmat <- do.call(rbind, components$Q_i)
  lvec <- unname(components$L_i)
  # hardening beyond the paper source: an Inf in lvec makes qr.solve return
  # zeros and the residual gate compares Inf > Inf, silently passing a bogus
  # point; the container validator does not check finiteness, and a non-finite
  # system is misuse or upstream overflow, not a no-point statistical outcome
  assert_bad_argument_ok(
    all(is.finite(qmat)) && all(is.finite(lvec)),
    "components contain non-finite values in Q_i or L_i",
    arg = "components"
  )
  # at tau = 0 the constraints are perfect squares, so a full-rank and
  # consistent Q theta = L system identifies a point; anything else is a
  # valid statistical outcome (no point), returned as NULL rather than an error
  if (nrow(qmat) < ncol(qmat) || qr(qmat, tol = tol)$rank < ncol(qmat)) {
    return(NULL)
  }
  point <- qr.solve(qmat, lvec, tol = tol)
  if (any(!is.finite(point))) {
    return(NULL)
  }
  if (max(abs(qmat %*% point - lvec)) > tol * max(1, max(abs(lvec)))) {
    return(NULL)
  }
  list(theta = as.numeric(point), cond = kappa(qmat))
}
