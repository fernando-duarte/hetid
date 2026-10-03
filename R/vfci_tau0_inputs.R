#' Prepare the Dated Tau-Zero VFCI Samples
#'
#' Validates column selectors and the quarterly date axis, then marks the
#' mean and volatility observations in the original input row order.
#'
#' @param data A data frame with a finite \code{Date} column named \code{date}.
#' @param y,z Single column names for the outcome and instrument.
#' @param x,y2,het Nonempty character vectors naming the conditioning,
#'   news, and volatility columns, respectively.
#' @param date_begin,date_end Finite scalar \code{Date} window bounds.
#' @return A list containing the quarter-end dates and original-row masks.
#'   Invalid inputs signal structured \code{hetid_error} conditions.
#' @noRd
vfci_tau0_inputs <- function(data, y, x, y2, z, het, date_begin, date_end) {
  assert_bad_argument_ok(is.data.frame(data), "data must be a data frame", arg = "data")
  assert_instrument_names(names(data), "data")
  selectors <- list(y = y, x = x, y2 = y2, z = z, het = het)
  for (arg in names(selectors)) {
    cols <- selectors[[arg]]
    assert_bad_argument_ok(
      is.character(cols) && is.null(dim(cols)) && length(cols) >= 1L,
      paste0(arg, " must be a nonempty character vector of column names"),
      arg = arg
    )
    assert_instrument_names(cols, arg)
    assert_bad_argument_ok(
      all(nzchar(trimws(cols))), paste0(arg, " must not contain blank names"),
      arg = arg
    )
  }
  assert_bad_argument_ok(length(y) == 1L, "y must select one column", arg = "y")
  assert_bad_argument_ok(length(z) == 1L, "z must select one column", arg = "z")
  mean_cols <- unique(c(y, x, y2, z))
  required <- unique(c(mean_cols, het))
  assert_columns_exist(data, c("date", required), arg = "data")
  for (col in required) {
    value <- data[[col]]
    assert_bad_argument_ok(
      is.numeric(value) && is.null(dim(value)),
      paste0(col, " must be a numeric vector"),
      arg = col
    )
  }

  validate_dates_vector(data$date, nrow(data), "data$date")
  dates <- to_period_end(data$date, "quarterly")
  assert_bad_argument_ok(
    all(diff(dates) > 0), "data$date must contain sorted, unique quarters",
    arg = "data$date"
  )
  validate_dates_vector(date_begin, 1L, "date_begin")
  validate_dates_vector(date_end, 1L, "date_end")
  first_date <- to_period_end(date_begin, "quarterly")
  last_date <- to_period_end(date_end, "quarterly")
  assert_bad_argument_ok(
    first_date <= last_date, "date_begin must not follow date_end",
    arg = "date_begin"
  )

  window_mask <- dates >= first_date & dates <= last_date
  mean_mask <- window_mask & stats::complete.cases(data[mean_cols])
  variance_mask <- mean_mask & stats::complete.cases(data[het])
  assert_insufficient_data_ok(
    sum(mean_mask) >= min_obs_for_pc_regression(length(x)),
    "Insufficient mean-equation observations for the tau-zero VFCI"
  )
  assert_insufficient_data_ok(
    sum(variance_mask) >= min_obs_for_pc_regression(length(het)),
    "Insufficient volatility-equation observations for the tau-zero VFCI"
  )
  assert_numeric_finite_values(as.matrix(data[variance_mask, het, drop = FALSE]), "het")
  list(dates = dates, masks = list(
    window = window_mask, mean = mean_mask, variance = variance_mask
  ))
}
