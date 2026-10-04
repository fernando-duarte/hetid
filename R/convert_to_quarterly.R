#' Convert Monthly Data to Quarterly
#'
#' Internal function to convert monthly data to quarterly by keeping
#' the last observation of each quarter and relabeling its date to the
#' last calendar day of the quarter using \code{\link{to_period_end}}.
#'
#' @details
#' Quarters whose last available observation is not in the terminal month
#' (March, June, September,
#' December) are either kept with their date re-coded to the last day
#' of the terminal month, so the quarterly series is uniformly dated --
#' raising a classed warning
#' (\code{hetid_warning_incomplete_quarter}) because incomplete data
#' enters the output -- or dropped, announced by an informational
#' message naming the removed quarters.
#' A quarter is complete when its last observation falls in its terminal
#' month; observations in every month of the quarter are not required.
#' Rows with a missing (NA) date are dropped first, with a classed warning
#' (\code{hetid_warning_dropped_na_dates}); the monthly path keeps them.
#' Duplicated dates are rejected up front with a structured error, since each
#' date must appear at most once for the conversion to be well defined.
#'
#' @param data Data frame with a \code{Date} column named \code{date}.
#'   Non-missing dates must be unique. Missing values in other columns
#'   are retained in the selected observations.
#' @param use_incomplete_quarters Logical scalar. If \code{TRUE} (the default, from
#'   \code{HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS}), incomplete
#'   quarters keep their latest available observation, re-dated to the
#'   end of the terminal quarter month. If \code{FALSE}, incomplete quarters
#'   are dropped.
#'
#' @return Data frame with one observation per retained quarter, ordered by
#'   date, with dates at calendar quarter-end and other column values taken
#'   from that quarter's last observation. A zero-row data frame is returned
#'   when the input is empty, when every date is \code{NA}, or when every
#'   quarter is dropped as incomplete.
#' @keywords internal
convert_to_quarterly <- function(
  data,
  use_incomplete_quarters = HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS
) {
  assert_tabular(data, "data")
  assert_columns_exist(data, "date", arg = "data")

  if (nrow(data) == 0) {
    return(data)
  }

  # Remove NA-dated rows explicitly before the duplicate check (repeated
  # NAs would otherwise be read as duplicates)
  na_date <- is.na(data[["date"]])
  if (any(na_date)) {
    n_na <- sum(na_date)
    warn_hetid(sprintf(
      paste0(
        "Dropped %d row%s with a missing (NA) date before quarterly ",
        "conversion; the monthly path keeps such rows."
      ),
      n_na, if (n_na == 1L) "" else "s"
    ), "hetid_warning_dropped_na_dates")
    data <- data[!na_date, , drop = FALSE]
    if (nrow(data) == 0) {
      return(data)
    }
  }

  assert_bad_argument_ok(
    anyDuplicated(data[["date"]]) == 0,
    paste0(
      "data contains duplicated dates; each date must appear at most ",
      "once for quarterly conversion"
    ),
    arg = "data"
  )

  data <- data[order(data[["date"]]), , drop = FALSE]

  # Separate frame preserves input columns named year, month, or quarter
  scratch <- data.frame(
    date = data[["date"]],
    year = as.numeric(format(data[["date"]], HETID_CONSTANTS$YEAR_FORMAT)),
    month = as.numeric(format(data[["date"]], HETID_CONSTANTS$MONTH_FORMAT))
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
      warn_hetid(paste0(
        notice,
        "These quarters are kept in the quarterly output using their ",
        "latest available observation, re-dated to the last day of the ",
        "quarter so that every quarterly date falls in March, June, ",
        "September, or December. To drop incomplete quarters instead, ",
        "set use_incomplete_quarters = FALSE (the TRUE default comes ",
        "from HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS)."
      ), "hetid_warning_incomplete_quarter")
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

  result$date <- to_period_end(result$date, "quarterly")

  result
}
