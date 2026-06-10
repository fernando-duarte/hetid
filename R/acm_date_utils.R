#' ACM Date Handling Utilities
#'
#' Locale-safe date parsing and date-based filtering helpers for the
#' ACM term-structure data access layer.
#'
#' @name acm_date_utils
#' @keywords internal
NULL

#' Parse Dates Under the C Locale
#'
#' The \code{\%b} month abbreviation in the ACM date format is
#' \code{LC_TIME}-dependent: "30-Jun-1961" parses to NA under, e.g.,
#' French locales. Parsing under the "C" locale makes the result
#' locale-independent.
#'
#' @param x Character vector of date strings
#' @param format Date format string passed to \code{as.Date}
#'
#' @return Date vector with NA for unparseable elements
#' @keywords internal
#' @noRd
parse_dates_c_locale <- function(x, format) {
  old_locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_locale), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  as.Date(x, format = format)
}

#' Coerce optional date input
#'
#' NULL and Date inputs pass through unchanged; character input is
#' parsed with \code{as.Date}. Anything else (e.g. a bare number, which
#' \code{Date >= numeric} would silently compare as days since the
#' epoch) raises a structured error.
#'
#' @param x Date bound supplied by the caller
#' @param arg Argument name for the structured error
#' @keywords internal
#' @noRd
coerce_optional_date <- function(x, arg) {
  if (is.null(x) || inherits(x, "Date")) {
    return(x)
  }
  if (is.character(x)) {
    return(as.Date(x))
  }
  stop_bad_argument(
    paste0(
      arg, " must be a Date or a character string in ",
      "\"YYYY-MM-DD\" format; got an object of class ",
      class(x)[1]
    ),
    arg = arg
  )
}

#' Normalize ACM date column
#'
#' Converts a character date column to Date using the ACM format under
#' the C locale. Errors when a non-empty column fails to parse entirely;
#' warns when only some values become NA.
#'
#' @keywords internal
#' @noRd
normalize_acm_date_column <- function(acm_data) {
  if (!("date" %in% names(acm_data)) || inherits(acm_data$date, "Date")) {
    return(acm_data)
  }

  raw_dates <- acm_data$date
  parsed <- parse_dates_c_locale(
    raw_dates,
    HETID_CONSTANTS$ACM_DATE_FORMAT
  )

  if (any(!is.na(raw_dates)) && all(is.na(parsed))) {
    stop_hetid(paste0(
      "Date column could not be parsed with format '",
      HETID_CONSTANTS$ACM_DATE_FORMAT,
      "'. Check the data file for corruption."
    ))
  }

  newly_na <- is.na(parsed) & !is.na(raw_dates)
  if (any(newly_na)) {
    warning(
      sum(newly_na),
      " date value(s) could not be parsed and became NA",
      call. = FALSE
    )
  }

  acm_data$date <- parsed
  acm_data
}

#' Filter ACM data by optional date bounds
#'
#' Rows with NA dates are dropped explicitly; NA subscripts would
#' otherwise fabricate all-NA rows.
#'
#' @keywords internal
#' @noRd
filter_acm_date_range <- function(acm_data, start_date, end_date) {
  if (!is.null(start_date)) {
    acm_data <- acm_data[which(acm_data$date >= start_date), , drop = FALSE]
  }
  if (!is.null(end_date)) {
    acm_data <- acm_data[which(acm_data$date <= end_date), , drop = FALSE]
  }

  acm_data
}
