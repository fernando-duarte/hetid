#' Input-Unit Validation for the Bond-Pricing Entry Points
#'
#' @name validate_units
#' @keywords internal
NULL

#' Validate Percentage-Point Yield Units
#'
#' Guardrail for the bond-pricing entry points: the term-structure
#' formulas assume yields in \strong{annualized percentage points} (the
#' \code{/ PERCENT_TO_DECIMAL = 100} divisor), so decimal yields would
#' silently distort the exponentiated quantities (\eqn{e^{\hat\mu}},
#' \eqn{\hat C_i}) instead of erroring. Nominal yields in percentage
#' points are not all below one across a full sample, so an all-small
#' magnitude flags likely decimal input. Warns (not errors) because a
#' single short maturity at the zero lower bound can be sub-unity.
#'
#' @param yields Yields data (matrix or data frame), percentage points
#'
#' @return Invisible TRUE; warns when the inputs look like decimals
#' @keywords internal
validate_percent_units <- function(yields) {
  assert_tabular(yields, "yields")
  y_max <- suppressWarnings(max(abs(as.matrix(yields)), na.rm = TRUE))
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
