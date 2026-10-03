#' Compute Zero-Safe Harvey Response Ratios
#'
#' Evaluate \eqn{y / \exp(X\theta)} on the log scale for positive responses,
#' keeping zero responses exactly zero. This is a numerical primitive, not
#' a fitting or convergence check.
#'
#' @param theta Finite numeric coefficient vector with one entry per column
#'   of \code{x_mat}. Coefficient order is positional; names do not reorder it.
#' @param y Finite nonnegative numeric response vector with one entry per
#'   row of \code{x_mat}.
#' @param x_mat Finite numeric matrix with at least one row and one column.
#'   Supply the complete design, including any intercept; none is added.
#'   Rows must be aligned with \code{y}.
#'
#' @return An unnamed numeric vector of length \code{length(y)}. Zero
#'   responses give exact zeros. Positive-response ratios may overflow to
#'   \code{Inf} or underflow to zero; they are not clamped.
#' @details
#' Finite inputs can produce nonfinite linear predictors or ratios.
#' Malformed input types, shapes, or values raise
#' \code{hetid_error_bad_argument}; incompatible lengths raise
#' \code{hetid_error_dimension_mismatch}. No rows are removed or rescaled.
#' @seealso \code{\link{precheck_harvey_starts}},
#'   \code{\link{fit_log_variance}}
#' @export
#' @examples
#' x_mat <- cbind(1, c(-1, 0, 1))
#' compute_harvey_ratio(c(0.2, -0.1), c(0, 1, 2), x_mat)
compute_harvey_ratio <- function(theta, y, x_mat) {
  validate_harvey_design(x_mat)
  validate_numeric_inputs(theta = theta)
  assert_numeric_finite_values(theta, "theta")
  assert_dimension_ok(
    length(theta) == ncol(x_mat), "theta must have ncol(x_mat) entries"
  )
  validate_log_variance_response(y, nrow(x_mat), 1)
  harvey_ratio(theta, y, x_mat)
}

#' @noRd
validate_harvey_design <- function(x_mat) {
  assert_bad_argument_ok(
    is.matrix(x_mat) && nrow(x_mat) > 0L && ncol(x_mat) > 0L,
    "x_mat must be a matrix with at least one row and one column",
    arg = "x_mat"
  )
  assert_numeric_finite_values(x_mat, "x_mat")
  invisible(TRUE)
}
