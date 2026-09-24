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
#'
#' Isolated feasible points, including convex tangencies, are omitted
#' because they contain no open cell. Thus this is a hull of the retained
#' intervals, subject to root rounding, rather than a claim that every
#' lower-dimensional part of the set is found.
#'
#' @param center Numeric length-I point on the line
#' @param dir Numeric length-I direction, need not be normalised
#' @param quadratic Quadratic form list with \code{A_i}, \code{b_i},
#'   \code{c_i}, as returned by \code{build_quadratic_system()}
#' @return Numeric \code{c(lower, upper)} hull of the feasible set on the
#'   line, with \code{-Inf} or \code{Inf} only when the polynomial signs
#'   establish feasibility of that tail,
#'   or \code{NULL} when no cell is feasible
#' @noRd
line_feasible_hull <- function(center, dir, quadratic) {
  coefs <- line_quadratic_coefficients(center, dir, quadratic)
  roots <- line_quadratic_roots(coefs)
  cuts <- sort(unique(c(roots)))
  left <- c(-Inf, cuts)
  right <- c(cuts, Inf)
  leading <- ifelse(
    coefs[, 1] != 0, sign(coefs[, 1]),
    ifelse(coefs[, 2] != 0, -sign(coefs[, 2]), sign(coefs[, 3]))
  )
  feasible <- rep(TRUE, length(left))
  for (i in seq_len(nrow(coefs))) {
    passed <- findInterval(left, sort(roots[i, ]))
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
