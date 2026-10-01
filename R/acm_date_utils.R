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
#' locale-independent. The previous \code{LC_TIME} setting is restored
#' on exit, including after a parsing error.
#'
#' @param x Character vector of date strings; missing values remain missing.
#' @param format Character date format string passed to \code{as.Date}.
#'
#' @return A \code{Date} vector of the same length, with \code{NA} for
#'   unparseable elements.
#' @noRd
parse_dates_c_locale <- function(x, format) {
  old_locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_locale), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  as.Date(x, format = format)
}

#' Coerce Optional Date Input
#'
#' \code{NULL} and scalar \code{Date} inputs pass through unchanged; character input is
#' parsed with \code{as.Date}. Anything else (e.g. a bare number, which
#' \code{Date >= numeric} would silently compare as days since the
#' epoch) raises a structured error. Non-NULL inputs must have length one.
#' Missing dates are retained; errors from \code{as.Date} propagate.
#'
#' @param x A scalar \code{Date}, a character date string, or \code{NULL}.
#' @param arg Character argument name for the structured error.
#' @return A scalar \code{Date}, possibly missing, or \code{NULL} for an
#'   unbounded date input.
#' @noRd
coerce_optional_date <- function(x, arg) {
  if (is.null(x)) {
    return(x)
  }
  assert_bad_argument_ok(
    length(x) == 1L,
    paste0(arg, " must be a single date; got length ", length(x)),
    arg = arg
  )
  if (inherits(x, "Date")) {
    return(x)
  }
  if (is.character(x)) {
    return(as.Date(x))
  }
  stop_bad_argument(
    paste0(
      arg, " must be a Date or a character string in ",
      "\"YYYY-MM-DD\" format; got an object of class ",
      paste(class(x), collapse = "/")
    ),
    arg = arg
  )
}

#' Parse ACM Dates With the Shared Format Fallback Chain
#'
#' Tries the locale-safe ACM month-abbreviation format, then R's default parser,
#' then explicit ISO, advancing to the next format whenever the current
#' one yields all-NA. The first result with a parsed date is returned;
#' formats are not combined element by element. \code{optional = TRUE} stops the default parser
#' from erroring on a malformed leading element, so the chain falls
#' through on a parse miss while unexpected parsing errors still propagate.
#'
#' @param raw_dates Character vector of date strings; missing values remain missing.
#' @return A \code{Date} vector of the same length, with \code{NA} for
#'   unparsed elements, or \code{NULL} when no format parses any element
#'   (including empty or entirely missing input).
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
#' shared format chain, errors if a column containing non-missing values
#' cannot be parsed at all, and raises a classed \code{hetid_warning_unparsed_dates} warning
#' when only some values become NA.
#'
#' @param raw_dates Character vector of date strings; missing values remain missing.
#' @param label Character column label used in messages; defaults to \code{"date"}.
#' @return A \code{Date} vector of the same length, all missing when
#'   \code{raw_dates} is entirely missing and empty when the input is empty.
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

#' Normalize ACM Date Column
#'
#' Converts a character date column to Date via the shared
#' \code{parse_and_warn_dates} helper (ACM month-abbreviation format, default
#' parser, ISO). Frames without a \code{date} column and columns already
#' inheriting from \code{Date} pass through unchanged.
#'
#' @param acm_data An ACM data frame, optionally with a \code{date} column.
#' @return The input data frame with a parsed \code{Date} column when a
#'   non-Date \code{date} column is present; otherwise the input unchanged.
#' @noRd
normalize_acm_date_column <- function(acm_data) {
  if (!("date" %in% names(acm_data)) || inherits(acm_data$date, "Date")) {
    return(acm_data)
  }
  acm_data$date <- parse_and_warn_dates(acm_data$date, "date")
  acm_data
}

#' Filter ACM Data by Optional Date Bounds
#'
#' Rows with NA dates are dropped explicitly; NA subscripts would
#' otherwise fabricate all-NA rows.
#'
#' @param acm_data An ACM data frame with a \code{Date} column named \code{date}.
#' @param start_date,end_date Optional inclusive scalar \code{Date} bounds;
#'   \code{NULL} means unbounded and a missing bound selects no rows.
#'   The lower bound is compared with period-end labels, the upper bound
#'   with raw dates.
#' @param frequency Character frequency used for lower-bound period labels:
#'   \code{"monthly"} (default), \code{"quarterly"}, \code{"annual"}, or \code{"daily"}.
#' @return A data frame containing the selected rows in input order, with
#'   missing-date rows dropped and columns, including raw dates, unchanged.
#' @noRd
filter_acm_date_range <- function(acm_data, start_date, end_date,
                                  frequency = "monthly") {
  # Dropping missing dates lets unbounded extracts reach to_period_end without an error
  acm_data <- acm_data[!is.na(acm_data$date), , drop = FALSE]
  if (!is.null(start_date)) {
    # Keep the boundary period when start_date is its calendar end but raw dates are earlier
    period_labels <- to_period_end(acm_data$date, frequency)
    acm_data <- acm_data[which(period_labels >= start_date), , drop = FALSE]
  }
  if (!is.null(end_date)) {
    acm_data <- acm_data[which(acm_data$date <= end_date), , drop = FALSE]
  }

  acm_data
}
