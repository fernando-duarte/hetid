#' Linear-Functional Bounds over an Identified Set
#'
#' Searches for the lower and upper values of named affine functionals
#' \eqn{a_k + l_k'\theta} over the same fitted-system domain as
#' \code{\link{compute_identified_set_box}}. Finite values are attained
#' inner approximations, not certified global extrema or an outer enclosure.
#'
#' @param fit A \code{hetid_tau0_fit} from \code{\link{compute_tau0_system}}.
#' @param tau Scalar slack in \code{(0, 1)}.
#' @param objectives Finite numeric matrix with one row per column of
#'   \code{fit$w2} and one uniquely named column per objective. Rows are
#'   positional when unnamed; supplied row names must match \code{fit$w2}
#'   column names exactly in order. No loading is snapped to zero.
#' @param offsets Finite numeric vector with one value per objective, or
#'   \code{NULL} for zeros. Supplied names must match objective names in order.
#' @param n_grid Odd number of grid nodes per gridded coordinate, at least three.
#' @param center Optional finite center, strictly inside the set. The default
#'   is the fit's tau-zero point. Supplied names must match the theta axis.
#' @param max_growth Maximum number of passes per growth phase, at least one.
#' @param search_limit Largest window half-width in frame units, at least two.
#'
#' @details
#' A singular or nonsquare Q frame is unsupported. Coordinates drive the first
#' growth phase and all tracked objectives the second. The frame and the attained
#' values can depend on the supplied center.
#' The same line-hull kernel as the box omits isolated feasible tangencies.
#' Grid refinement and a wider window may reveal further attained values;
#' neither a stopped search nor absence of a recession witness proves boundedness.
#' Finite witnesses are checked through the system checker with relative tolerance
#' \code{IDENTIFIED_SET_CONTROL$FEAS_TOL} times the absolute quadratic terms.
#' Overflow in finite objective or witness arithmetic raises a structured error.
#' Extremely wide windows can also overflow the line-polynomial coefficients and
#' abort the call with a structured error.
#' At extreme scales, cancellation can lose feasible line cells or make membership
#' unresolved within this relative tolerance. Widening the window cannot remove
#' those precision limits and is not guaranteed to find the full set.
#' The direction sampler restores \code{.Random.seed}; samples depend on the caller's
#' \code{RNGkind()}. R's cached Box-Muller normal deviate is not restored.
#'
#' An infinite side has either a \code{line_tail} record, with an origin and
#' an oriented direction whose sufficiently distant tail is feasible, or a
#' \code{strict_curvature} record with a direction satisfying
#' \eqn{v'A_i v < 0} for every constraint. Strict negativity holds on an open
#' cone, which cannot lie in a nonzero objective's zero hyperplane. Thus it
#' implies both sides of every nonzero objective are infinite even when the
#' recorded direction itself is orthogonal to that particular objective.
#' A line origin need not be feasible; its sufficiently distant tail is.
#' These are floating-point witnesses, not exact-arithmetic certificates.
#'
#' @return A \code{hetid_functional_bounds} list with \code{bounds} (columns
#'   \code{coef}, \code{lower}, \code{upper}), \code{arg_lower} and
#'   \code{arg_upper} (one theta row per objective, NA for infinite sides),
#'   and \code{evidence_lower} and \code{evidence_upper} (named lists; NULL
#'   for attained finite sides, otherwise the witness described above).
#'   It retains \code{objectives}, \code{offsets}, \code{quadratic},
#'   \code{center}, \code{basis}, \code{tau}, \code{n_grid}, and
#'   \code{search}. The latter records controls, per-phase pass counts,
#'   final half-widths and edge flags per frame coordinate (one per column of
#'   \code{basis}, not per theta coordinate). The stop reason is \code{pass_limit},
#'   \code{search_limit}, or \code{no_boundary_improvement}: no driving objective
#'   improved at a window-boundary node in that phase's final sweep.
#'   \code{pass_limit} takes precedence when both limits apply; the edge flags and
#'   half-widths retain both facts. These are joint phase stops, not per-side
#'   reliability labels. Every finite side may be curtailed, and no stop reason
#'   certifies boundedness or global optimality.
#' @seealso \code{\link{make_system_checker}},
#'   \code{\link{compute_identified_set_box}}
#' @export
#' @examples
#' set.seed(42)
#' n <- 100
#' z <- rnorm(n)
#' x <- cbind(x = rnorm(n))
#' y2 <- cbind(news = exp(z / 2) * rnorm(n))
#' y1 <- 0.3 + x[, 1] + 0.5 * y2[, 1] + rnorm(n)
#' fit <- compute_tau0_system(y1, y2, x, z)
#' objectives <- matrix(c(1, -2, 0), 1, dimnames = list("news", c("news", "twice", "flat")))
#' compute_linear_functional_bounds(fit, 0.05, objectives, c(0, 1, 3))$bounds
compute_linear_functional_bounds <- function(
  fit, tau, objectives, offsets = NULL,
  n_grid = IDENTIFIED_SET_CONTROL$N_GRID, center = NULL,
  max_growth = IDENTIFIED_SET_CONTROL$MAX_GROWTH,
  search_limit = IDENTIFIED_SET_CONTROL$SEARCH_LIMIT
) {
  search_frame <- linear_bounds_frame(fit, tau, n_grid, center, max_growth, search_limit)
  offsets <- validate_linear_objectives(objectives, offsets, colnames(fit$w2))
  n_components <- nrow(objectives)
  all_objectives <- cbind(diag(n_components), objectives)
  found <- identified_set_search(
    search_frame$center, search_frame$basis, search_frame$quadratic, n_grid, all_objectives,
    n_primary = n_components, evidence = TRUE,
    max_growth = max_growth, search_limit = search_limit
  )
  found <- apply_recession_bounds(found, search_frame$quadratic, all_objectives)
  rows <- n_components + seq_len(ncol(objectives))
  bounds <- identified_set_bounds_frame(colnames(objectives), offsets, found, rows)
  finite <- cbind(is.finite(found$lower[rows]), is.finite(found$upper[rows]))
  if (any(!is.finite(as.matrix(bounds[c("lower", "upper")])) & finite)) {
    stop_hetid("Finite affine bound exceeds the numeric range")
  }
  lo <- found$arg_lower[rows, , drop = FALSE]
  hi <- found$arg_upper[rows, , drop = FALSE]
  dimnames(lo) <- dimnames(hi) <- list(colnames(objectives), colnames(fit$w2))
  validate_theta_box_bounds(bounds, "bounds")
  validate_theta_box_witnesses(bounds, lo, hi, n_components, c("arg_lower", "arg_upper"))
  validate_linear_witnesses(rbind(
    lo[finite[, 1], , drop = FALSE],
    hi[finite[, 2], , drop = FALSE]
  ), search_frame$quadratic)
  structure(c(list(
    bounds = bounds, arg_lower = lo, arg_upper = hi,
    evidence_lower = linear_bounds_evidence(found$tail_lower[rows], objectives, colnames(fit$w2)),
    evidence_upper = linear_bounds_evidence(found$tail_upper[rows], objectives, colnames(fit$w2)),
    objectives = objectives, offsets = offsets, tau = tau, n_grid = n_grid,
    search = found$search
  ), search_frame), class = "hetid_functional_bounds")
}
