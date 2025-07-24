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
  # Add year and month to identify quarter ends
  data$year <- as.numeric(format(data$date, HETID_CONSTANTS$YEAR_FORMAT))
  data$month <- as.numeric(format(data$date, HETID_CONSTANTS$MONTH_FORMAT))
  data$quarter <- ceiling(data$month / 3)

  # For each quarter, keep only the last observation
  data <- data[order(data$date), ]

  # Group by year-quarter and take last observation
  last_in_quarter <- aggregate(
    date ~ year + quarter,
    data = data,
    FUN = max
  )

  # Merge to get full data for last observation in each quarter
  result <- merge(
    last_in_quarter[, "date", drop = FALSE],
    data[, setdiff(names(data), c("year", "month", "quarter"))],
    by = "date",
    all.x = TRUE
  )

  result
}
