#' Input-Unit Validation for the Bond-Pricing Entry Points
#'
#' Internal guardrail for yields supplied to the bond-pricing calculations.
#'
#' @name validate_units
#' @keywords internal
NULL

#' Validate Percentage-Point Yield Units
#'
#' Warns when yield magnitudes suggest decimal input to term-structure
#' formulas that assume \strong{annualized percentage points}. Those formulas
#' divide by \code{HETID_CONSTANTS$PERCENT_TO_DECIMAL}, so decimal input would
#' distort subsequent exponentiated quantities, including \eqn{e^{\hat\mu}}
#' and \eqn{\hat C_i}.
#'
#' @param yields A matrix or data frame that converts to a numeric matrix,
#'   containing yields in annualized percentage points.
#'
#' @return The logical scalar \code{TRUE}, returned invisibly after the check.
#'
#' @details
#' Emits a \code{hetid_warning_unit_scale} warning when the maximum absolute
#' yield is finite and strictly below one. This is a heuristic: a single short
#' maturity at the zero lower bound can also have sub-unity percentage-point
#' yields. The function warns rather than rejecting such values and does not
#' rescale or modify the input.
#'
#' Missing values (\code{NA} and \code{NaN}) are ignored for the magnitude check.
#' No unit-scale warning is emitted for an empty numeric matrix, an input with
#' only missing values, or an input containing an infinite value. This check
#' does not establish that yields are finite or that their units are correct.
#' Non-tabular input or an input that does not become a numeric matrix signals
#' a \code{hetid_error_bad_argument} condition.
#' @keywords internal
validate_percent_units <- function(yields) {
  assert_tabular(yields, "yields")
  mat <- as.matrix(yields)
  assert_bad_argument_ok(
    is.numeric(mat),
    "yields must contain only numeric columns",
    arg = "yields"
  )
  y_max <- suppressWarnings(max(abs(mat), na.rm = TRUE))
  if (is.finite(y_max) && y_max < 1) {
    warn_hetid(
      paste0(
        "Yields look like decimals (max |yield| < 1). The term-structure ",
        "formulas assume annualized percentage points; multiply decimal ",
        "inputs by 100."
      ),
      "hetid_warning_unit_scale"
    )
  }
  invisible(TRUE)
}
