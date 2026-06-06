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
  validate_numeric_inputs(w1 = w1)
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
  validate_maturities(
    maturities,
    max_value = ncol(w2),
    max_label = "ncol(w2)"
  )

  list(w2 = w2, t_obs = t_obs, maturities = maturities)
}
