#' Convert Monthly Data to Quarterly
#'
#' Internal function to convert monthly data to quarterly by keeping
#' the last observation of each quarter. Quarters whose last available
#' observation is not in the terminal month (March, June, September,
#' December) are either kept with their date re-coded to the last day
#' of the terminal month, so the quarterly series is uniformly dated --
#' raising a classed warning
#' (\code{hetid_warning_incomplete_quarter}) because incomplete data
#' enters the output -- or dropped, announced by an informational
#' message naming the removed quarters.
#'
#' @param data Data frame with a date column
#' @param use_incomplete_quarters Logical. If TRUE (the default, from
#'   \code{HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS}), incomplete
#'   quarters keep their latest available observation, re-dated to the
#'   end of the terminal quarter month. If FALSE, incomplete quarters
#'   are dropped.
#'
#' @return Data frame with quarterly observations
#' @keywords internal
convert_to_quarterly <- function(
  data,
  use_incomplete_quarters = HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS
) {
  if (nrow(data) == 0) {
    return(data)
  }

  # Remove NA-dated rows explicitly before the duplicate check (repeated
  # NAs would otherwise be read as duplicates).
  na_date <- is.na(data$date)
  if (any(na_date)) {
    n_na <- sum(na_date)
    warn_dropped_na_dates(sprintf(
      paste0(
        "Dropped %d row%s with a missing (NA) date before quarterly ",
        "conversion; the monthly path keeps such rows."
      ),
      n_na, if (n_na == 1L) "" else "s"
    ))
    data <- data[!na_date, , drop = FALSE]
    if (nrow(data) == 0) {
      return(data)
    }
  }

  assert_bad_argument_ok(
    anyDuplicated(data$date) == 0,
    paste0(
      "data contains duplicated dates; each date must appear at most ",
      "once for quarterly conversion"
    ),
    arg = "data"
  )

  data <- data[order(data$date), , drop = FALSE]

  # Scratch frame avoids clobbering any input columns named year/month/quarter
  scratch <- data.frame(
    date = data$date,
    year = as.numeric(format(data$date, HETID_CONSTANTS$YEAR_FORMAT)),
    month = as.numeric(format(data$date, HETID_CONSTANTS$MONTH_FORMAT))
  )
  scratch$quarter <- ceiling(
    scratch$month / HETID_CONSTANTS$MONTHS_PER_QUARTER
  )

  last_in_quarter <- aggregate(
    date ~ year + quarter,
    data = scratch,
    FUN = max
  )

  last_months <- as.numeric(
    format(last_in_quarter$date, HETID_CONSTANTS$MONTH_FORMAT)
  )
  expected_months <- last_in_quarter$quarter *
    HETID_CONSTANTS$MONTHS_PER_QUARTER
  incomplete <- last_months != expected_months

  if (any(incomplete)) {
    details <- paste0(
      last_in_quarter$year[incomplete],
      " Q", last_in_quarter$quarter[incomplete],
      " (last observation in ", month.name[last_months[incomplete]],
      ", quarter ends in ", month.name[expected_months[incomplete]], ")"
    )
    notice <- paste0(
      "Incomplete quarter(s) detected: ",
      paste(details, collapse = "; "), ". "
    )
    if (use_incomplete_quarters) {
      warn_incomplete_quarter(paste0(
        notice,
        "These quarters are kept in the quarterly output using their ",
        "latest available observation, re-dated to the last day of the ",
        "quarter so that every quarterly date falls in March, June, ",
        "September, or December. To drop incomplete quarters instead, ",
        "set use_incomplete_quarters = FALSE (the TRUE default comes ",
        "from HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS)."
      ))
    } else {
      dropped <- if (sum(incomplete) == 1) {
        "This quarter was dropped from the quarterly output. To keep it"
      } else {
        "These quarters were dropped from the quarterly output. To keep them"
      }
      message(paste0(
        notice, dropped,
        " instead, set use_incomplete_quarters = TRUE."
      ))
      last_in_quarter <- last_in_quarter[!incomplete, , drop = FALSE]
    }
  }

  result <- merge(
    last_in_quarter[, "date", drop = FALSE],
    data,
    by = "date",
    all.x = TRUE
  )

  # Period-end: shift to the last calendar day of the quarter (Mar 31 / Jun 30 /
  # Sep 30 / Dec 31) regardless of which business day the last observation fell on.
  result$date <- to_period_end(result$date, "quarterly")

  result
}
