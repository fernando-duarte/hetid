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
