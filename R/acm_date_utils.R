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

#' Parse ACM dates with the shared format fallback chain
#'
#' Tries the locale-safe legacy ACM format, then R's default parser,
#' then explicit ISO, advancing to the next format whenever the current
#' one yields all-NA. \code{optional = TRUE} stops the default parser
#' from erroring on a malformed leading element, so the chain falls
#' through on a parse miss while genuinely unexpected errors (e.g. a
#' non-character input) still propagate instead of being swallowed.
#'
#' @param raw_dates Character vector of date strings
#' @return Date vector, or NULL when no format parses any element
#' @keywords internal
#' @noRd
parse_acm_dates <- function(raw_dates) {
  date_formats <- list(
    HETID_CONSTANTS$ACM_DATE_FORMAT,
    NULL,
    HETID_CONSTANTS$ISO_DATE_FORMAT
  )
  for (fmt in date_formats) {
    parsed <- if (is.null(fmt)) {
      as.Date(raw_dates, optional = TRUE)
    } else {
      parse_dates_c_locale(raw_dates, fmt)
    }
    if (!all(is.na(parsed))) {
      return(parsed)
    }
  }
  NULL
}

#' Parse a Date Column and Warn on Partial Failures
#'
#' Shared parse-and-warn step behind \code{normalize_acm_date_column}
#' and \code{load_term_premia}: parses \code{raw_dates} through the
#' shared format chain, errors when a non-empty column fails to parse
#' entirely, and raises a classed \code{warn_unparsed_dates} warning
#' when only some values become NA.
#'
#' @param raw_dates Character vector of date strings
#' @param label Column label used in messages (e.g. "date", "DATE")
#' @return Date vector (all-NA when \code{raw_dates} is entirely NA)
#' @keywords internal
#' @noRd
parse_and_warn_dates <- function(raw_dates, label = "date") {
  parsed <- parse_acm_dates(raw_dates)
  if (is.null(parsed)) {
    if (any(!is.na(raw_dates))) {
      stop_hetid(paste0(
        "The ", label, " column could not be parsed with any supported ",
        "format. The file may be stale or corrupt."
      ))
    }
    return(as.Date(rep(NA_character_, length(raw_dates))))
  }
  newly_na <- is.na(parsed) & !is.na(raw_dates)
  if (any(newly_na)) {
    warn_unparsed_dates(paste0(
      sum(newly_na), " ", label,
      " value(s) could not be parsed and became NA"
    ))
  }
  parsed
}

#' Normalize ACM date column
#'
#' Converts a character date column to Date via the shared
#' \code{parse_and_warn_dates} helper (legacy ACM format, default
#' parser, ISO).
#'
#' @keywords internal
#' @noRd
normalize_acm_date_column <- function(acm_data) {
  if (!("date" %in% names(acm_data)) || inherits(acm_data$date, "Date")) {
    return(acm_data)
  }
  acm_data$date <- parse_and_warn_dates(acm_data$date, "date")
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
