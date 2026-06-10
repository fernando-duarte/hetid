#' Validate Statistics Function Inputs
#'
#' Shared validation for compute_scalar_statistics,
#' compute_matrix_statistics, compute_vector_statistics, and
#' compute_identification_moments. Checks types, finiteness, a minimum
#' number of observations, dimension agreement, and the maturity vector.
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
  assert_numeric_finite_values(w1, "w1")
  assert_numeric_finite_values(w2, "w2")

  t_obs <- length(w1)
  assert_insufficient_data_ok(
    t_obs >= 2,
    paste0(
      "At least 2 observations are required to compute ",
      "centered variances; got ", t_obs
    )
  )
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

#' Assert All Values Are Finite Numerics
#'
#' Guards against non-numeric content surviving as.matrix coercion
#' (e.g. a data frame with a character column) and against NA, NaN, or
#' infinite values that would otherwise propagate silently into the
#' moment statistics.
#'
#' @param x Numeric vector or matrix to check
#' @param arg Argument name for the structured error
#' @return Invisible TRUE when validation passes
#' @noRd
assert_numeric_finite_values <- function(x, arg) {
  assert_bad_argument_ok(
    is.numeric(x),
    paste0(arg, " must contain only numeric values"),
    arg = arg
  )
  assert_bad_argument_ok(
    all(is.finite(x)),
    paste0(arg, " must not contain NA, NaN, or infinite values"),
    arg = arg
  )
  invisible(TRUE)
}

#' Validate the Principal Components Input
#'
#' Shared pcs validation for compute_vector_statistics and
#' compute_identification_moments: tabular type, numeric finite
#' content, and row count equal to the number of observations.
#'
#' @param pcs Matrix or data frame of principal components (T x J)
#' @param t_obs Number of observations in w1/w2
#' @return pcs coerced to a numeric matrix
#' @noRd
validate_pcs_input <- function(pcs, t_obs) {
  assert_tabular(pcs, "pcs")
  pcs <- as.matrix(pcs)
  assert_bad_argument_ok(
    ncol(pcs) >= 1,
    "pcs must have at least one column",
    arg = "pcs"
  )
  assert_numeric_finite_values(pcs, "pcs")
  assert_dimension_ok(
    nrow(pcs) == t_obs,
    paste0(
      "pcs must have the same number of ",
      "observations as w1 and w2"
    )
  )
  pcs
}
