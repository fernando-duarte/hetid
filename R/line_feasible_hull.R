#' Feasible Interval Hull Along One Line
#'
#' Internal kernel returning the interval hull of the feasible set restricted
#' to the line \eqn{\theta(t) = center + t \cdot dir}. Each constraint
#' becomes a univariate quadratic in \eqn{t}, so the feasible set on the
#' line is a union of closed intervals. Roots partition it into open cells;
#' the sign of each polynomial is determined by its leading coefficient
#' and changes at each simple root. Counting a repeated root twice leaves
#' the sign unchanged. This solves one coordinate without sampling it.
#'
#' Cell classification uses no feasibility tolerance: a small positive
#' value at one point cannot establish feasibility of a whole cell or an
#' infinite tail. Only an exactly zero leading coefficient is linear.
#' Roots use scaled, cancellation-resistant arithmetic; an unrepresentable
#' root signals an error rather than supplying an infinite bound.
#' Non-finite line coefficients also raise a \code{hetid_error}.
#'
#' Isolated feasible points, including convex tangencies, are omitted
#' because they contain no open cell. Thus this is a hull of the retained
#' intervals, subject to root rounding, rather than a claim that every
#' lower-dimensional part of the set is found.
#'
#' @param center Finite numeric length-I point on the line, where I is
#'   the theta-axis dimension.
#' @param dir Finite numeric length-I direction; need not be normalised.
#' @param quadratic Quadratic form list with \code{A_i}, \code{b_i},
#'   \code{c_i}, as in the \code{quadratic} element returned by
#'   \code{build_quadratic_system()}. Matrices in \code{A_i} must be
#'   symmetric and dimensions must match \code{center} and \code{dir}.
#' @return Unnamed numeric vector \code{c(lower, upper)} bounding the
#'   retained feasible intervals in the line parameter \eqn{t}, with
#'   \code{-Inf} or \code{Inf} only when the polynomial signs establish
#'   feasibility of that tail, or \code{NULL} when no cell is feasible.
#'   The hull may span infeasible gaps between retained intervals.
#' @noRd
line_feasible_hull <- function(center, dir, quadratic) {
  coefs <- line_quadratic_coefficients(center, dir, quadratic)
  roots <- line_quadratic_roots(coefs)
  cuts <- unique(c(roots))
  cuts <- cuts[!is.na(cuts)]
  cuts <- cuts[order(cuts)]
  left <- c(-Inf, cuts)
  right <- c(cuts, Inf)
  # the leading sign: curvature, else minus the slope, else the constant
  leading <- sign(coefs[, 3])
  linear <- coefs[, 2] != 0
  leading[linear] <- -sign(coefs[linear, 2])
  curved <- coefs[, 1] != 0
  leading[curved] <- sign(coefs[curved, 1])
  feasible <- rep(TRUE, length(left))
  for (i in seq_len(nrow(coefs))) {
    # roots at or left of each cell's left end, a repeated root counted twice
    passed <- integer(length(left))
    for (root in roots[i, ]) {
      if (!is.na(root)) passed <- passed + (left >= root)
    }
    feasible <- feasible & leading[i] * (-1)^passed <= 0
  }
  if (!any(feasible)) {
    return(NULL)
  }
  c(
    min(left[feasible]),
    max(right[feasible])
  )
}

#' Univariate Coefficients of Every Constraint Along a Line
#'
#' Substituting \eqn{center + t \cdot dir} into
#' \eqn{\theta' A \theta + b'\theta + c} gives \eqn{a t^2 + \beta t +
#' \gamma}. The cross term uses \eqn{2 \cdot dir' A center}, which is
#' exact because \code{A_i} is symmetrized when the system is assembled.
#'
#' @param center,dir Finite numeric length-I vectors, where I is the
#'   theta-axis dimension.
#' @param quadratic Quadratic form list with symmetric \code{A_i}
#'   matrices and matching \code{b_i} vectors and \code{c_i} constants.
#' @return Numeric matrix with one row per constraint, in the order of
#'   \code{quadratic$c_i}, and three unnamed columns containing \code{a},
#'   \code{beta}, and \code{gamma}, respectively. No values are removed
#'   or replaced when arithmetic produces non-finite coefficients.
#' @noRd
line_quadratic_coefficients <- function(center, dir, quadratic) {
  n_constraints <- length(quadratic$c_i)
  coefs <- matrix(0, nrow = n_constraints, ncol = 3)
  for (i in seq_len(n_constraints)) {
    a_mat <- quadratic$A_i[[i]]
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
