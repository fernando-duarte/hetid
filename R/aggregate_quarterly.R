#' Aggregate Monthly Values to Calendar Quarters
#'
#' Applies an explicit mean or last-observation rule to a dated numeric frame,
#' retaining the package's terminal-month policy for incomplete quarters.
#'
#' @param data A data frame with a nonmissing finite \code{Date} column named
#'   \code{date} and numeric value columns. Column names must be unique and
#'   nonempty. At most one observation per calendar month is allowed.
#' @param method A required character scalar: \code{"mean"} or \code{"last"}.
#' @param use_incomplete_quarters A nonmissing logical scalar. The default
#'   comes from \code{HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS}. If \code{TRUE},
#'   quarters lacking their terminal month are retained with the existing classed
#'   warning; if \code{FALSE}, they are dropped with an informational message.
#' @param na_rm A nonmissing logical scalar, default \code{FALSE}. Passed to
#'   \code{mean()} for the mean method. It does not affect last-observation
#'   selection and never fills a missing last value.
#'
#' @return A list with \code{data}, \code{periods}, \code{method}, \code{na_rm},
#'   and \code{use_incomplete_quarters}. Data retains the value columns and
#'   uses ordered calendar quarter-end dates. Periods contains matching
#'   \code{date}, \code{n_observations}, and \code{terminal_month_present}
#'   columns. Empty input or dropping every quarter gives typed empty frames.
#'
#' @details
#' Monthly labels are normalized without changing values, then observations are
#' sorted by date. Duplicate months, invalid dates and nonnumeric value columns
#' raise structured \code{hetid_error_bad_argument} conditions.
#' A complete quarter has an observation in its terminal month; missing interior
#' months do not make it incomplete. Counts expose the actual coverage.
#'
#' Means use actual observed monthly values without imputation. Base mean's
#' NA/NaN behavior is preserved: with \code{na_rm = FALSE}, all-NA values
#' return NA and all-NaN values return NaN; with \code{TRUE}, either all-missing
#' input returns NaN. All-missing columns are retained. Last selection preserves
#' the selected value, including NA, as in the existing quarterly ACM extraction.
#'
#' Descriptive quarterly yield means and last-observation SDF inputs are distinct
#' policies. This function does not choose one from an intended downstream use.
#'
#' @seealso \code{\link{to_period_end}}, \code{\link{compute_sdf_panel}}
#' @export
#' @examples
#' monthly <- data.frame(
#'   date = as.Date(c("2024-01-31", "2024-02-29", "2024-03-31")),
#'   value = c(1, 2, 6)
#' )
#' aggregate_quarterly(monthly, method = "mean")
#' aggregate_quarterly(monthly, method = "last")
aggregate_quarterly <- function(
  data, method,
  use_incomplete_quarters = HETID_CONSTANTS$USE_INCOMPLETE_QUARTERS,
  na_rm = FALSE
) {
  if (missing(method)) {
    stop_bad_argument("method must be supplied as mean or last", "method")
  }
  assert_bad_argument_ok(
    is.character(method) && length(method) == 1L && is.null(dim(method)) &&
      !is.na(method) && method %in% c("mean", "last"),
    "method must be mean or last", "method"
  )
  assert_flag(use_incomplete_quarters, "use_incomplete_quarters")
  assert_flag(na_rm, "na_rm")
  validate_sdf_dated_frame(data, "data")
  data$date <- to_period_end(data$date, "monthly")
  assert_bad_argument_ok(
    !anyDuplicated(data$date), "data must have at most one observation per month", "data"
  )
  data <- data[order(data$date), , drop = FALSE]
  result <- convert_to_quarterly(data, use_incomplete_quarters)
  periods <- data.frame(
    date = result$date, n_observations = integer(nrow(result)),
    terminal_month_present = logical(nrow(result))
  )
  if (nrow(result) > 0L) {
    quarter_dates <- to_period_end(data$date, "quarterly")
    groups <- split(seq_len(nrow(data)), as.character(quarter_dates))
    rows <- groups[as.character(result$date)]
    periods$n_observations <- unname(lengths(rows))
    month <- as.integer(format(data$date, HETID_CONSTANTS$MONTH_FORMAT))
    periods$terminal_month_present <- unname(vapply(rows, function(index) {
      any(month[index] %% HETID_CONSTANTS$MONTHS_PER_QUARTER == 0L)
    }, logical(1)))
    if (method == "mean") {
      for (column in setdiff(names(data), "date")) {
        result[[column]] <- unname(vapply(rows, function(index) {
          mean(data[[column]][index], na.rm = na_rm)
        }, numeric(1)))
      }
    }
  }
  list(
    data = result, periods = periods, method = method, na_rm = na_rm,
    use_incomplete_quarters = use_incomplete_quarters
  )
}
