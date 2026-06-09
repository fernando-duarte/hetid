#' Statistics Computation Utilities
#'
#' Higher-order functions for per-maturity statistics computation
#'
#' @name statistics_utils
#' @keywords internal
NULL

#' Compute Per-Maturity Statistics
#'
#' Validates inputs and applies a computation function to each
#' maturity, returning a named list of results. Encapsulates the
#' validate-then-iterate pattern shared by the statistics functions.
#'
#' @param w1 Numeric vector of W1 residuals
#' @param w2 Matrix of W2 residuals (T x I)
#' @param maturities Vector of maturity indices (NULL for all)
#' @param compute_fn Function called for each maturity with args
#'   (w1, w2, w2_i, t_obs, idx, i, ...)
#' @param ... Extra arguments forwarded to compute_fn
#' @return Named list of per-maturity results from compute_fn
#' @keywords internal
compute_per_maturity <- function(w1, w2, maturities,
                                 compute_fn, ...) {
  validated <- validate_statistics_inputs(
    w1, w2, maturities
  )
  w2 <- validated$w2
  t_obs <- validated$t_obs
  maturities <- validated$maturities

  results <- lapply(
    seq_along(maturities),
    function(idx) {
      i <- maturities[idx]
      w2_i <- w2[, i]
      compute_fn(
        w1 = w1, w2 = w2, w2_i = w2_i,
        t_obs = t_obs, idx = idx, i = i, ...
      )
    }
  )
  names(results) <- maturity_names(maturities)
  results
}

#' Centered Sample Covariance (1/T normalization)
#'
#' Computes the centered sample covariance between the columns of two
#' conformable inputs (vectors or matrices), using the \eqn{1/T}
#' normalization of the sample analog in Lewbel multivariate set
#' identification (centered cov/var; see the package spec sections on
#' moment notation and centering), \strong{not} the \eqn{1/(T-1)}
#' convention of [stats::cov()]. For inputs \eqn{A} (T x a) and
#' \eqn{B} (T x b),
#' \deqn{\widehat{\mathrm{Cov}}(A, B) = \frac{1}{T} A^\top B -
#'   \bar{A}\,\bar{B}^\top \in \mathbb{R}^{a \times b},}
#' where \eqn{\bar{A}} and \eqn{\bar{B}} are the column means. Centering
#' both arguments makes each entry a true covariance regardless of whether
#' the inputs were already centered.
#'
#' @param a Numeric vector or matrix (T x a).
#' @param b Numeric vector or matrix (T x b).
#' @param t_obs Number of observations T used for normalization.
#' @return An \eqn{a \times b} matrix of centered covariances.
#' @keywords internal
centered_cov <- function(a, b, t_obs) {
  a <- as.matrix(a)
  b <- as.matrix(b)
  crossprod(a, b) / t_obs - tcrossprod(colMeans(a), colMeans(b))
}
