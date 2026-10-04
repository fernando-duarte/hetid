#' Normalize Dates to the Calendar Period-End Convention
#'
#' Maps each date to the **last calendar day** of its period at the given
#' frequency: month-end for \code{"monthly"}, quarter-end
#' (Mar 31 / Jun 30 / Sep 30 / Dec 31) for \code{"quarterly"}, and Dec 31 for
#' \code{"annual"}. For \code{"daily"} the dates are returned unchanged (after
#' Date coercion and the missing-value check): each observation is its own
#' period. Period-end labels give series a common calendar convention for
#' joins by date. Inputs must still represent compatible observation periods.
#'
#' Normalization is a relabel, not a reshape: each input date maps to exactly
#' one period-end, so applying it to a regular series preserves the row count
#' and only canonicalizes the date labels (e.g. a business-day end
#' \code{1962-03-30} and a period-start \code{1962-01-01} both become the
#' calendar quarter-end \code{1962-03-31}).
#' Input order is preserved, and dates in the same period remain duplicated;
#' observations are not aggregated.
#'
#' Missing dates, including missing values produced by coercion, cause a
#' \code{hetid_error_bad_argument} error. Other coercion errors are propagated
#' from \code{\link[base:as.Date]{as.Date}}, and invalid frequencies are rejected
#' by \code{\link[base:match.arg]{match.arg}}.
#'
#' The bundled \code{\link{variables}} dataset ships exactly as imported from
#' its source repository with quarter-start date labels; apply this function
#' to its \code{date} column before merging it with package ACM extracts.
#'
#' @param dates A non-missing \code{Date} vector, or a character vector coercible
#'   by \code{\link[base:as.Date]{as.Date}} without producing missing dates.
#' @param frequency A single character string: \code{"monthly"} (the default),
#'   \code{"quarterly"}, \code{"annual"}, or \code{"daily"}. Unambiguous
#'   abbreviations are accepted.
#' @return A \code{Date} vector of the same length and order as \code{dates},
#'   each the calendar period-end. Empty input returns an empty \code{Date} vector.
#' @examples
#' to_period_end(as.Date(c("1962-01-01", "1962-03-30")), "quarterly")
#'
#' # Month-end is the default and accounts for leap years
#' to_period_end(c("2024-02-01", "2024-02-29"))
#' to_period_end("2024-02-01", "annual")
#' to_period_end(c("2024-02-01", "2024-02-29"), "daily")
#' @export
to_period_end <- function(dates,
                          frequency = c("monthly", "quarterly", "annual", "daily")) {
  frequency <- match.arg(frequency)
  dates <- as.Date(dates)
  if (anyNA(dates)) {
    stop_bad_argument("dates must be non-missing and coercible to Date", arg = "dates")
  }
  if (frequency == "daily") {
    return(dates)
  }

  year <- as.integer(format(dates, HETID_CONSTANTS$YEAR_FORMAT))
  month <- as.integer(format(dates, HETID_CONSTANTS$MONTH_FORMAT))

  terminal_month <- switch(frequency,
    monthly = month,
    quarterly = ceiling(month / HETID_CONSTANTS$MONTHS_PER_QUARTER) *
      HETID_CONSTANTS$MONTHS_PER_QUARTER,
    annual = HETID_CONSTANTS$MONTHS_PER_YEAR
  )

  # Last calendar day of terminal_month = first day of the next month minus 1
  first_of_next_month <- as.Date(sprintf(
    "%04d-%02d-01",
    as.integer(year + terminal_month %/% HETID_CONSTANTS$MONTHS_PER_YEAR),
    as.integer(terminal_month %% HETID_CONSTANTS$MONTHS_PER_YEAR + 1L)
  ))
  first_of_next_month - 1L
}
