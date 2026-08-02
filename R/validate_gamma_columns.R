#' Reject All-Zero Constrained Columns of Gamma
#'
#' A zero column makes L_i = V_i = Q_i = 0, hence A_i = b_i = c_i = 0, so the
#' constraint 0 <= 0 holds at every theta and the maturity drops out silently,
#' widening the identified set. Only constrained columns are checked, matching
#' the general path's as_lambda_list() guard in R/validate_general_lambda.R.
#'
#' @param gamma Instrument weight matrix
#' @param maturities Integer vector of constrained column indices
#' @param arg Argument name for the structured error
#'
#' @return Invisible TRUE when every constrained column is nonzero
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
