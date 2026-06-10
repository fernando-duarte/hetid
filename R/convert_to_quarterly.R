#' Convert Monthly Data to Quarterly
#'
#' Internal function to convert monthly data to quarterly by keeping
#' the last observation of each quarter.
#'
#' @param data Data frame with a date column
#'
#' @return Data frame with quarterly observations
#' @keywords internal
convert_to_quarterly <- function(data) {
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

  # Warn if any quarter uses a non-terminal month
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
    warning(
      "Incomplete quarter(s) detected: ",
      paste(details, collapse = "; "),
      ". Using the latest available month for each.",
      call. = FALSE
    )
  }

  # Merge to get full data for last observation in each quarter
  merge(
    last_in_quarter[, "date", drop = FALSE],
    data,
    by = "date",
    all.x = TRUE
  )
}
