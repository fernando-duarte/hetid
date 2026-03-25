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
  assert_bad_argument_ok(
    is.numeric(w1) && is.vector(w1),
    "w1 must be a numeric vector",
    arg = "w1"
  )
  assert_tabular(w2, "w2")
  w2 <- as.matrix(w2)

  t_obs <- length(w1)
  assert_dimension_ok(
    nrow(w2) == t_obs,
    paste0(
      "w1 and w2 must have the same number of ",
      "observations"
    )
  )

  if (is.null(maturities)) {
    maturities <- seq_len(ncol(w2))
  }
  assert_bad_argument_ok(
    is.numeric(maturities) &&
      all(is.finite(maturities)) &&
      all(maturities %% 1 == 0),
    "maturities must be finite integer values",
    arg = "maturities"
  )
  assert_bad_argument_ok(
    all(maturities >= 1) &&
      all(maturities <= ncol(w2)),
    "maturities must be between 1 and ncol(w2)",
    arg = "maturities"
  )

  list(w2 = w2, t_obs = t_obs, maturities = maturities)
}
