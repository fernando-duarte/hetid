#' Validate Statistics Function Inputs
#'
#' Shared validation for compute_scalar_statistics,
#' compute_matrix_statistics, and compute_vector_statistics.
#'
#' @param w1 Numeric vector of residuals
#' @param w2 Matrix or data frame of residuals
#' @param maturities Maturity indices or NULL for all columns
#'
#' @return List with validated components:
#'   \describe{
#'     \item{w2}{w2 coerced to matrix}
#'     \item{t_obs}{Number of observations}
#'     \item{maturities}{Validated maturity vector}
#'   }
#' @keywords internal
validate_statistics_inputs <- function(w1, w2,
                                       maturities = NULL) {
  if (!is.numeric(w1) || !is.vector(w1)) {
    stop_bad_argument(
      "w1 must be a numeric vector",
      arg = "w1"
    )
  }
  assert_tabular(w2, "w2")
  w2 <- as.matrix(w2)

  t_obs <- length(w1)
  if (nrow(w2) != t_obs) {
    stop_dimension_mismatch(paste0(
      "w1 and w2 must have the same number of ",
      "observations"
    ))
  }

  if (is.null(maturities)) {
    maturities <- seq_len(ncol(w2))
  }
  if (!is.numeric(maturities) ||
    any(!is.finite(maturities)) ||
    any(maturities %% 1 != 0)) {
    stop_bad_argument(
      "maturities must be finite integer values",
      arg = "maturities"
    )
  }
  if (any(maturities < 1) ||
    any(maturities > ncol(w2))) {
    stop_bad_argument(
      "maturities must be between 1 and ncol(w2)",
      arg = "maturities"
    )
  }

  list(w2 = w2, t_obs = t_obs, maturities = maturities)
}
