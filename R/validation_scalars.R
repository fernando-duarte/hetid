#' Assert Scalar Finite Value
#'
#' Internal guard for parameters that must be a single
#' finite numeric value.
#'
#' @param x Object to check. Valid input is a single finite numeric value;
#'   missing values, \code{NaN}, and infinite values are rejected.
#' @param name Single character string used in the error message and the
#'   condition's \code{arg} field.
#'
#' @return Invisible \code{TRUE} if valid. Otherwise, signals a
#'   \code{hetid_error_bad_argument} condition.
#' @keywords internal
assert_scalar_finite <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
    stop_bad_argument(
      paste0(name, " must be a single finite numeric value"),
      arg = name
    )
  }
  invisible(TRUE)
}

#' Assert a Scalar Is an Integer Within a Closed Range
#'
#' Shared core for scalar integer-index validators. Checks that a single
#' finite numeric value is integer-valued and lies within inclusive bounds.
#'
#' @param x Object to check. Valid input is a single finite, integer-valued
#'   numeric value; missing values, \code{NaN}, and infinite values are rejected.
#' @param name Single character string used in the error message and the
#'   condition's \code{arg} field when the finite numeric check fails.
#' @param min_value,max_value Single numeric lower and upper bounds of the
#'   inclusive range. Bounds are supplied by the caller and are not validated.
#' @param arg Single character string used in the condition's \code{arg} field
#'   when the integer or range check fails. Defaults to \code{name}.
#'
#' @return Invisible \code{TRUE} if valid. Otherwise, signals a
#'   \code{hetid_error_bad_argument} condition if \code{x} fails validation.
#' @keywords internal
assert_scalar_integer_in_range <- function(x, name, min_value, max_value,
                                           arg = name) {
  assert_scalar_finite(x, name)
  assert_bad_argument_ok(
    x %% 1 == 0,
    paste0(name, " must be an integer"),
    arg = arg
  )
  assert_bad_argument_ok(
    x >= min_value && x <= max_value,
    paste0(name, " must be between ", min_value, " and ", max_value),
    arg = arg
  )
  invisible(TRUE)
}
