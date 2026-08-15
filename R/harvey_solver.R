#' Harvey Solver Primitives
#'
#' The math and linear-algebra core of the Harvey Gaussian
#' multiplicative-heteroskedasticity log-variance solve: the zero-safe ratio
#' \eqn{r = y / \exp(X\theta)}, the observed information, the guarded
#' single-point evaluation every step is judged on, and the Cholesky
#' triangular solve behind the Fisher direction. Ported from the paper
#' pipeline (\code{scripts-paper/log_variance/estimators/harvey/likelihood.R}
#' and \code{.../solver_primitives.R}). No clamping, no epsilon added to
#' \code{y}, no \eqn{\eta} capping: a non-finite quantity is a hard trial
#' failure for the caller to reject, never a value this layer silences.
#'
#' @name harvey_solver
#' @keywords internal
NULL

#' Zero-Safe Ratio r = y / exp(X theta)
#'
#' The evaluation order is contractual: form \eqn{\eta}, mark the positive
#' rows, seed \code{r} with zeros, and only then fill the positive rows on the
#' log scale. A zero response row stays an exact zero without ever forming
#' \code{0 * Inf}, and a non-finite positive row is left as it is (\code{Inf},
#' never \code{NaN}) for the caller to treat as a failed trial. \code{y} is not
#' re-validated here: the exported boundary \code{\link{fit_log_variance}}
#' already required it finite and nonnegative.
#'
#' @param theta Numeric coefficient vector of length \code{ncol(x_mat)}
#' @param y Numeric nonnegative response
#' @param x_mat Numeric design matrix, intercept column included
#'
#' @return Numeric vector of length \code{length(y)}
#' @keywords internal
harvey_ratio <- function(theta, y, x_mat) {
  eta <- drop(x_mat %*% theta)
  pos <- y > 0
  r <- numeric(length(y))
  r[pos] <- exp(log(y[pos]) - eta[pos])
  r
}

#' Observed Information of the Harvey Criterion
#'
#' \eqn{0.5 X' diag(r) X}, the Hessian of
#' \eqn{Q = 0.5 (\sum_t \eta_t + \sum_t r_t)}. The expected information
#' \eqn{0.5 X'X} is a trivial expression and gets no helper.
#'
#' @inheritParams harvey_ratio
#'
#' @return Numeric \code{ncol(x_mat)} square matrix
#' @keywords internal
harvey_info <- function(theta, y, x_mat) {
  0.5 * crossprod(x_mat, harvey_ratio(theta, y, x_mat) * x_mat)
}

#' Evaluate One Candidate Coefficient Vector
#'
#' The single gate every start, line-search trial, and accepted point passes
#' through, so no downstream step ever sees a non-finite criterion or score.
#' The upper-overflow guard on \eqn{\eta} is deliberate: \code{exp()} of
#' anything past \code{log(.Machine$double.xmax)} is \code{Inf}, and a fitted
#' variance that large is a runaway trial, not a solution.
#'
#' @inheritParams harvey_ratio
#' @param pos Logical vector marking the positive-response rows
#' @param col_abs Numeric vector \code{colSums(abs(x_mat))}, the per-coordinate
#'   scale the moment is judged on
#'
#' @return \code{NULL} when the point is unusable, otherwise a list with
#'   \code{theta}, \code{eta}, \code{r}, \code{q} (the criterion),
#'   \code{moment} (\eqn{X'(r - 1)}), and \code{score_norm}
#' @keywords internal
harvey_eval <- function(theta, y, x_mat, pos, col_abs) {
  if (!all(is.finite(theta))) {
    return(NULL)
  }
  eta <- drop(x_mat %*% theta)
  if (!all(is.finite(eta)) || any(eta > log(.Machine$double.xmax))) {
    return(NULL)
  }
  r <- harvey_ratio(theta, y, x_mat)
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
#' @param chol_xx Upper triangular Cholesky factor
#' @param m Numeric vector or matrix to solve against
#'
#' @return The solve of the factored matrix applied to \code{m}
#' @keywords internal
harvey_chol_solve <- function(chol_xx, m) {
  backsolve(
    chol_xx,
    forwardsolve(chol_xx, m, upper.tri = TRUE, transpose = TRUE)
  )
}
