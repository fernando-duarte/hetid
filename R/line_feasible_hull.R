#' Exact Feasible Hull Along One Line
#'
#' Internal kernel returning the exact hull of the feasible set restricted
#' to the line \eqn{\theta(t) = center + t \cdot dir}. Each constraint
#' becomes a univariate quadratic in \eqn{t}, so the feasible set on the
#' line is a union of closed intervals and its hull is exact rather than
#' sampled. This is what lets the box search solve one coordinate as a
#' continuum instead of gridding it.
#'
#' The sign of the leading coefficient is never branched on. Roots come
#' from the cancellation-resistant form, and feasibility is then decided
#' by evaluating the true polynomials at one interior point of every cell
#' the roots cut the line into. A vanishing or near-vanishing leading
#' coefficient therefore needs no special case: the lost root is
#' non-finite and drops out, leaving the linear root behind.
#'
#' Isolated tangency points (a constraint touching zero from below with
#' \eqn{a > 0}) are dropped, because a zero-width cell has no interior
#' point to test. The loss sits inside the grid error the caller already
#' accepts.
#'
#' @param center Numeric length-I point on the line
#' @param dir Numeric length-I direction, need not be normalised
#' @param quadratic Quadratic form list with \code{A_i}, \code{b_i},
#'   \code{c_i}, as returned by \code{build_quadratic_system()}
#' @param tol Feasibility tolerance; a cell counts as feasible when its
#'   largest constraint value is at most \code{tol}
#' @return Numeric \code{c(lower, upper)} hull of the feasible set on the
#'   line, with \code{-Inf} or \code{Inf} when an outer cell is feasible,
#'   or \code{NULL} when no cell is feasible
#' @noRd
line_feasible_hull <- function(center, dir, quadratic, tol) {
  coefs <- line_quadratic_coefficients(center, dir, quadratic)
  roots <- line_quadratic_roots(coefs)
  ends <- c(-Inf, roots, Inf)
  feasible <- vapply(
    line_cell_probes(roots),
    function(t) max(coefs[, 1] * t^2 + coefs[, 2] * t + coefs[, 3]) <= tol,
    logical(1)
  )
  if (!any(feasible)) {
    return(NULL)
  }
  c(
    min(ends[-length(ends)][feasible]),
    max(ends[-1][feasible])
  )
}

#' Univariate Coefficients of Every Constraint Along a Line
#'
#' Substituting \eqn{center + t \cdot dir} into
#' \eqn{\theta' A \theta + b'\theta + c} gives \eqn{a t^2 + \beta t +
#' \gamma}. The cross term uses \eqn{2 \cdot dir' A center}, which is
#' exact because \code{A_i} is symmetrized when the system is assembled.
#'
#' @param center,dir Numeric length-I vectors
#' @param quadratic Quadratic form list
#' @return Numeric matrix with one row per constraint and columns
#'   \code{a}, \code{beta}, \code{gamma}
#' @noRd
line_quadratic_coefficients <- function(center, dir, quadratic) {
  n_constraints <- length(quadratic$c_i)
  coefs <- matrix(0, nrow = n_constraints, ncol = 3)
  for (i in seq_len(n_constraints)) {
    a_mat <- quadratic$A_i[[i]] # nolint: object_name_linter.
    b_vec <- quadratic$b_i[[i]]
    a_dir <- drop(a_mat %*% dir)
    coefs[i, ] <- c(
      sum(dir * a_dir),
      2 * sum(center * a_dir) + sum(b_vec * dir),
      sum(center * drop(a_mat %*% center)) + sum(b_vec * center) +
        quadratic$c_i[i]
    )
  }
  coefs
}

#' Real Roots of Every Constraint Along a Line
#'
#' Uses \eqn{q = -(\beta + s\sqrt{disc})/2} with \eqn{s = 1} when
#' \eqn{\beta \ge 0} and \eqn{-1} otherwise, then \eqn{r_1 = q/a} and
#' \eqn{r_2 = \gamma/q}. This avoids the cancellation the textbook
#' formula suffers when \eqn{4a\gamma} is small beside \eqn{\beta^2}, and
#' it degrades correctly as \eqn{a} vanishes: one root runs off to
#' infinity and is discarded, the other is the linear root
#' \eqn{-\gamma/\beta}.
#'
#' \code{sign()} must not be used for \eqn{s}: \code{sign(0)} is zero,
#' which would send \eqn{q} to zero and lose both roots whenever
#' \eqn{\beta} is exactly zero.
#'
#' @param coefs Numeric matrix from \code{line_quadratic_coefficients()}
#' @return Sorted numeric vector of the distinct finite roots
#' @noRd
line_quadratic_roots <- function(coefs) {
  disc <- coefs[, 2]^2 - 4 * coefs[, 1] * coefs[, 3]
  keep <- is.finite(disc) & disc >= 0
  if (!any(keep)) {
    return(numeric(0))
  }
  a_val <- coefs[keep, 1]
  beta_val <- coefs[keep, 2]
  gamma_val <- coefs[keep, 3]
  q_val <- -(beta_val + ifelse(beta_val >= 0, 1, -1) * sqrt(disc[keep])) / 2
  roots <- c(q_val / a_val, gamma_val / q_val)
  sort(unique(roots[is.finite(roots)]))
}

#' One Interior Probe Point Per Cell
#'
#' The roots cut the line into cells on which every constraint keeps a
#' constant sign, so one interior point decides each cell. Probes are
#' taken at midpoints and one unit beyond the extreme roots, never at a
#' root itself: the constraint owning that root is zero there, so the
#' tolerance rather than the geometry would decide.
#'
#' @param roots Sorted numeric vector of distinct finite roots
#' @return Numeric vector of probe points, one per cell
#' @noRd
line_cell_probes <- function(roots) {
  n_roots <- length(roots)
  if (n_roots == 0L) {
    return(0)
  }
  c(
    roots[1] - 1,
    if (n_roots > 1L) (roots[-1] + roots[-n_roots]) / 2,
    roots[n_roots] + 1
  )
}
