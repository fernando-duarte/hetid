#' Assert Scalar Finite Value
#'
#' Internal guard for parameters that must be a single
#' finite numeric value.
#'
#' @param x Value to check
#' @param name Parameter name for the error message
#'
#' @return Invisible TRUE if valid, stops with informative error otherwise.
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
#' Shared core for scalar integer-index validators. The \code{arg} defaults to
#' \code{name} so callers can keep a distinct condition \code{arg} field.
#'
#' @param x Value to check
#' @param name Parameter name used in the error message
#' @param min_value,max_value Inclusive integer bounds
#' @param arg Condition argument name (defaults to \code{name})
#'
#' @return Invisible TRUE if valid, stops with informative error otherwise.
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
