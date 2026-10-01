#' Validate a Date Vector for a Time-Series Output
#'
#' Check that a time-series date index is a dimensionless \code{Date} vector,
#' has finite, nonmissing values, and has the expected length.
#'
#' @details
#' Character, POSIXct, and bare numeric inputs are rejected without coercion.
#' Converting a bare numeric row index with \code{as.Date()} would interpret
#' it as days since the epoch. The sibling \code{coerce_optional_date} in
#' acm_date_utils.R instead parses character date bounds.
#'
#' Callers must supply calendar period-end dates normalized at ingestion.
#' This helper does not check period ends, ordering, or uniqueness and does
#' not modify the input. An empty \code{Date} vector passes when
#' \code{expected_len} is zero.
#'
#' @param dates A dimensionless \code{Date} vector with finite, nonmissing values.
#' @param expected_len A nonnegative integer scalar giving the required number
#'   of dates (e.g. \code{nrow(yields)}).
#' @param arg A character scalar naming the argument in bad-argument errors.
#'   Defaults to \code{"dates"}.
#'
#' @return Invisible \code{TRUE} when the checks pass. Invalid date types, shapes,
#'   or nonfinite dates signal a \code{hetid_error_bad_argument}; a length mismatch
#'   signals a \code{hetid_error_dimension_mismatch}. Both inherit from
#'   \code{hetid_error}.
#' @noRd
validate_dates_vector <- function(dates, expected_len, arg = "dates") {
  assert_bad_argument_ok(
    !is.null(dates) && inherits(dates, "Date") && is.null(dim(dates)),
    paste0(
      arg, " must be a Date vector (period-end calendar dates); a time ",
      "series cannot be returned without its date column"
    ),
    arg = arg
  )
  assert_bad_argument_ok(
    !anyNA(dates),
    paste0(arg, " must not contain NA"),
    arg = arg
  )
  assert_bad_argument_ok(
    all(is.finite(dates)),
    paste0(arg, " must contain only finite dates"),
    arg = arg
  )
  assert_dimension_ok(
    length(dates) == expected_len,
    sprintf(
      "length(%s) is %d but must be %d", arg, length(dates), expected_len
    )
  )
  invisible(TRUE)
}
