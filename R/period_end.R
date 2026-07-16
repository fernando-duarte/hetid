#' Normalize Dates to the Calendar Period-End Convention
#'
#' Maps each date to the **last calendar day** of its period at the given
#' frequency: month-end for \code{"monthly"}, quarter-end
#' (Mar 31 / Jun 30 / Sep 30 / Dec 31) for \code{"quarterly"}, and Dec 31 for
#' \code{"annual"}. For \code{"daily"} the dates are returned unchanged (after
#' Date coercion and the missing-value check): each observation is its own
#' period. This is the single date convention used throughout the
#' package: every time series is normalized to period-end at ingestion so that
#' series of any frequency align on identical calendar dates and merge by date.
#'
#' Normalization is a relabel, not a reshape: each input date maps to exactly
#' one period-end, so applying it to a regular series preserves the row count
#' and only canonicalizes the date labels (e.g. a business-day end
#' \code{1962-03-30} and a period-start \code{1962-01-01} both become the
#' calendar quarter-end \code{1962-03-31}).
#'
#' The bundled \code{\link{variables}} dataset ships exactly as imported from
#' its source repository with quarter-start date labels; apply this function
#' to its \code{date} column before merging it with package ACM extracts.
#'
#' @param dates A \code{Date} (or character coercible by \code{as.Date}) vector.
#' @param frequency One of \code{"monthly"}, \code{"quarterly"}, \code{"annual"},
#'   \code{"daily"}.
#' @return A \code{Date} vector of the same length, each the calendar period-end.
#' @examples
#' to_period_end(as.Date(c("1962-01-01", "1962-03-30")), "quarterly")
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
    annual = 12L
  )

  # Last calendar day of terminal_month = first day of the next month minus 1
  first_of_next_month <- as.Date(sprintf(
    "%04d-%02d-01",
    as.integer(year + terminal_month %/% 12L),
    as.integer(terminal_month %% 12L + 1L)
  ))
  first_of_next_month - 1L
}
