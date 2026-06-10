#' Convert Monthly Data to Quarterly
#'
#' Internal function to convert monthly data to quarterly by keeping
#' the last observation of each quarter. Quarters whose last available
#' observation is not in the terminal month (March, June, September,
#' December) raise a classed warning
#' (\code{hetid_warning_incomplete_quarter}); they are then either kept
#' with their date re-coded to the last day of the terminal month, so
#' the quarterly series is uniformly dated, or dropped.
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
  # An empty input has no quarters; return it unchanged to mirror the
  # monthly path's documented zero-row result
  if (nrow(data) == 0) {
    return(data)
  }

  data <- data[order(data$date), , drop = FALSE]

  # Quarter bookkeeping lives in a scratch frame so input columns named
  # year/month/quarter are never clobbered
  scratch <- data.frame(
    date = data$date,
    year = as.numeric(format(data$date, HETID_CONSTANTS$YEAR_FORMAT)),
    month = as.numeric(format(data$date, HETID_CONSTANTS$MONTH_FORMAT))
  )
  scratch$quarter <- ceiling(
    scratch$month / HETID_CONSTANTS$MONTHS_PER_QUARTER
  )

  # Group by year-quarter and take last observation
  last_in_quarter <- aggregate(
    date ~ year + quarter,
    data = scratch,
    FUN = max
  )

  # Quarters whose last observation is not in the terminal month
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
      " (using month ", last_months[incomplete],
      " instead of ", expected_months[incomplete], ")"
    )
    action <- if (use_incomplete_quarters) {
      "Keeping the latest available month for each, re-dated to quarter end."
    } else {
      "Dropping the incomplete quarter(s)."
    }
    warn_incomplete_quarter(paste0(
      "Incomplete quarter(s) detected: ",
      paste(details, collapse = "; "), ". ", action
    ))
    if (!use_incomplete_quarters) {
      last_in_quarter <- last_in_quarter[!incomplete, , drop = FALSE]
    }
  }

  # Merge to get full data for last observation in each quarter
  result <- merge(
    last_in_quarter[, "date", drop = FALSE],
    data,
    by = "date",
    all.x = TRUE
  )

  if (use_incomplete_quarters && any(incomplete)) {
    # Re-date kept incomplete quarters to the end of the terminal month
    # so the quarterly series is uniformly dated; chronological order is
    # unchanged because the new date stays within the same quarter
    idx <- match(last_in_quarter$date[incomplete], result$date)
    result$date[idx] <- quarter_end_date(
      last_in_quarter$year[incomplete],
      expected_months[incomplete]
    )
  }

  result
}

#' Last Calendar Day of a Terminal Quarter Month
#'
#' @param year Numeric vector of years
#' @param terminal_month Numeric vector of quarter-end months (3, 6, 9, 12)
#'
#' @return Date vector
#' @keywords internal
#' @noRd
quarter_end_date <- function(year, terminal_month) {
  first_of_next_month <- as.Date(sprintf(
    "%04d-%02d-01",
    as.integer(year + terminal_month %/% 12),
    as.integer(terminal_month %% 12 + 1)
  ))
  first_of_next_month - 1
}
