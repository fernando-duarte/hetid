#' Validate Statistics Function Inputs
#'
#' Shared validation for \code{compute_scalar_statistics()},
#' \code{compute_matrix_statistics()}, \code{compute_vector_statistics()}, and
#' \code{compute_identification_moments()}. Checks types, finiteness, a minimum
#' number of observations, dimension agreement, and the maturity vector.
#'
#' @param w1 Numeric vector of finite residuals with at least two observations.
#' @param w2 Numeric matrix or data frame of finite residuals, with
#'   \code{length(w1)} rows and at least one column.
#' @param maturities Nonempty numeric vector of distinct integer-valued
#'   \code{w2} column indices between 1 and \code{ncol(w2)}, inclusive.
#'   These identify the maturity constraint axis, not bond maturities in months.
#'   The default \code{NULL} selects all columns in their original order.
#'
#' @return A list with the following validated components.
#'   \describe{
#'     \item{w2}{A numeric matrix with the same dimensions as the input.}
#'     \item{t_obs}{The number of observations, equal to \code{length(w1)}.}
#'     \item{maturities}{The selected column indices in the supplied order,
#'       or \code{seq_len(ncol(w2))} when the input is \code{NULL}.}
#'   }
#' @details Missing, \code{NaN}, and infinite residuals are rejected rather
#'   than removed. Invalid types, values, or maturity indices signal a
#'   \code{hetid_error_bad_argument}; fewer than two observations signal a
#'   \code{hetid_error_insufficient_data}; unequal observation counts signal a
#'   \code{hetid_error_dimension_mismatch}.
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
#' @param x Numeric object to check for missing, \code{NaN}, or infinite values.
#' @param arg Argument name for the structured error.
#' @return Invisible \code{TRUE} when validation passes; otherwise signals a
#'   \code{hetid_error_bad_argument}.
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
#' Shared \code{pcs} validation for \code{compute_vector_statistics()} and
#' \code{compute_identification_moments()}: tabular type, numeric finite
#' content, and row count equal to the number of observations.
#'
#' @param pcs Numeric matrix or data frame of finite instruments, with
#'   \code{t_obs} rows and at least one column. In the VFCI application,
#'   these are principal components of nominal financial asset returns.
#' @param t_obs Number of observations in \code{w1} and \code{w2}.
#' @return The \code{pcs} input coerced to a numeric matrix with unchanged
#'   dimensions. Invalid types, missing or non-finite values, or no columns
#'   signal a \code{hetid_error_bad_argument}; an unequal row count signals a
#'   \code{hetid_error_dimension_mismatch}.
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
