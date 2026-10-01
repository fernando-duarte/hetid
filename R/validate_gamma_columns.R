#' Reject All-Zero Constrained Columns of Gamma
#'
#' Checks that each constrained column has at least one nonzero instrument weight.
#'
#' @details
#' A zero column makes \eqn{L_i = V_i = Q_i = 0}, hence
#' \eqn{A_i = b_i = c_i = 0}. The constraint \eqn{0 \leq 0} then holds at
#' every \eqn{\theta}, so that maturity supplies no identifying restriction.
#' Only constrained columns are checked, matching the general path's
#' \code{as_lambda_list()} guard in \code{R/validate_general_lambda.R}.
#' Nonzero weights are detected exactly, without a numerical tolerance.
#' The caller must validate the matrix type, finite values, dimensions, and
#' column indices before invoking this helper; missing values are not removed.
#'
#' @param gamma Numeric instrument weight matrix with one row per instrument
#'   and one column per system component, already validated by the caller.
#' @param maturities Integer vector of constrained system column indices in
#'   \code{gamma}, not bond maturities in months or years.
#' @param arg Character string naming the argument in the error message and
#'   structured condition; defaults to \code{"gamma"}.
#'
#' @return \code{TRUE}, invisibly, when every constrained column is nonzero.
#'   Otherwise, signals a \code{hetid_error_bad_argument} condition naming
#'   all constrained columns with zero weights.
#' @noRd
assert_gamma_columns_nonzero <- function(gamma, maturities, arg = "gamma") {
  zero_cols <- maturities[colSums(gamma[, maturities, drop = FALSE] != 0) == 0]
  assert_bad_argument_ok(
    length(zero_cols) == 0,
    paste0(
      arg, " has all-zero column(s) ", paste(zero_cols, collapse = ", "),
      "; every constrained column needs a nonzero weight direction"
    ),
    arg = arg
  )
}
