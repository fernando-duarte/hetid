#' Validate a Date Vector for a Time-Series Output
#'
#' Single source of truth for the package date convention: a function that
#' returns a time series cannot do so without a real \code{Date} index. The
#' dates must be a non-missing \code{Date} vector (never a character string or
#' a fabricated integer row index) of the expected length. Character/POSIXct
#' inputs are rejected rather than coerced, because \code{as.Date()} of a bare
#' numeric silently reads days-since-epoch -- exactly the fake-index failure
#' this guard exists to prevent.
#'
#' @param dates The date vector supplied by the caller.
#' @param expected_len Integer; the required length (e.g. \code{nrow(yields)}).
#' @param arg Argument name used in the structured error.
#'
#' @return Invisible \code{TRUE} when valid; otherwise a \code{hetid_error}.
#' @keywords internal
#' @noRd
validate_dates_vector <- function(dates, expected_len, arg = "dates") {
  assert_bad_argument_ok(
    !is.null(dates) && inherits(dates, "Date"),
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
  assert_dimension_ok(
    length(dates) == expected_len,
    sprintf(
      "length(%s) is %d but must be %d", arg, length(dates), expected_len
    )
  )
  invisible(TRUE)
}
