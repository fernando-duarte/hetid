#' Harvey Solver Primitives
#'
#' The math and linear-algebra core of the Harvey Gaussian
#' multiplicative-heteroskedasticity log-variance solve: the zero-safe ratio
#' \eqn{r = y / \exp(X\theta)}, the guarded
#' single-point evaluation every step is judged on, and the Cholesky
#' triangular solve behind the Fisher direction. No clamping, no epsilon added to
#' \code{y}, no \eqn{\eta} capping: a non-finite quantity is a hard trial
#' failure for the caller to reject, never a value this layer silences.
#'
#' @name harvey_solver
#' @keywords internal
NULL

#' Evaluate One Candidate Coefficient Vector
#'
#' The single gate every start, line-search trial, and accepted point passes
#' through, so no downstream step ever sees a non-finite criterion or score.
#' The upper-overflow guard on \eqn{\eta} is deliberate: \code{exp()} of
#' anything past \code{log(.Machine$double.xmax)} is \code{Inf}, and a fitted
#' variance that large is a runaway trial, not a solution.
#'
#' @inheritParams compute_harvey_ratio
#' @param pos Logical vector \code{y > 0} of length \code{length(y)}.
#' @param col_abs Numeric vector \code{colSums(abs(x_mat))}, the per-coordinate
#'   scale the moment is judged on. Each entry must be positive.
#' @param log_y_pos Numeric vector \code{log(y[pos])}; a fit passes the copy it
#'   computed once instead of taking the log on every evaluation.
#'
#' @return \code{NULL} for non-finite coefficients or linear predictors,
#'   overflowing fitted variances, non-finite ratios, criterion, or scaled
#'   score. Otherwise, a list with coefficient vector \code{theta},
#'   observation-length vectors \code{eta} and \code{r}, scalar criterion
#'   \code{q}, coefficient-length vector \code{moment} (\eqn{X'(r - 1)}), and
#'   scalar \code{score_norm} (\code{max(abs(moment) / col_abs)}).
#' @keywords internal
harvey_eval <- function(theta, y, x_mat, pos, col_abs, log_y_pos = log(y[pos])) {
  if (!all(is.finite(theta))) {
    return(NULL)
  }
  eta <- drop(x_mat %*% theta)
  if (!all(is.finite(eta)) || any(eta > log(.Machine$double.xmax))) {
    return(NULL)
  }
  # the ratio from the eta already in hand, in compute_harvey_ratio()'s order
  r <- numeric(length(y))
  r[pos] <- exp(log_y_pos - eta[pos])
  if (anyNA(r) || !all(is.finite(r[pos]))) {
    return(NULL)
  }
  crit <- 0.5 * (sum(eta) + sum(r[pos]))
  moment <- drop(crossprod(x_mat, r - 1))
  score_norm <- max(abs(moment) / col_abs)
  if (!is.finite(crit) || !is.finite(score_norm)) {
    return(NULL)
  }
  list(
    theta = theta, eta = eta, r = r, q = crit, moment = moment,
    score_norm = score_norm
  )
}

#' Solve Through a Cholesky Factor
#'
#' \code{chol_xx} is the upper triangular factor of a positive definite
#' matrix; the forward and back substitutions return that matrix's solve
#' applied to \code{m}, without ever forming an explicit inverse.
#'
#' @param chol_xx Numeric square upper triangular Cholesky factor of a
#'   positive definite matrix.
#' @param m Numeric vector of length \code{nrow(chol_xx)}, or numeric matrix
#'   with that many rows, giving the right-hand side.
#'
#' @return Numeric vector or matrix with the same dimensions as \code{m},
#'   solving the system whose matrix is \code{crossprod(chol_xx)}.
#' @keywords internal
harvey_chol_solve <- function(chol_xx, m) {
  backsolve(
    chol_xx,
    forwardsolve(chol_xx, m, upper.tri = TRUE, transpose = TRUE)
  )
}
