#' Time Series Transformation Utilities
#'
#' Vectorized transformation functions for time series data
#'
#' @name transform_utils
#' @keywords internal
NULL

#' Validate Transform Output Length
#'
#' @param out Output from transform_fn
#' @param n_valid Number of valid (non-NA) elements
#' @keywords internal
validate_transform_length <- function(out, n_valid) {
  assert_dimension_ok(
    length(out) == n_valid,
    "transform_fn must return one value per valid element"
  )
}

#' Apply Transformation to Time Series
#'
#' Apply a transformation function to aligned time series
#'
#' @param series1 First series
#' @param series2 Second series (optional)
#' @param transform_fn Transformation function
#' @param ... Additional arguments to transform_fn
#' @return Transformed series
#' @keywords internal
apply_time_series_transform <- function(series1, series2 = NULL,
                                        transform_fn, ...) {
  if (is.null(series2)) {
    result <- rep(NA_real_, length(series1))
    valid <- !is.na(series1)
    if (any(valid)) {
      out <- transform_fn(series1[valid], ...)
      validate_transform_length(out, sum(valid))
      result[valid] <- out
    }
  } else {
    assert_dimension_ok(
      length(series1) == length(series2),
      "series1 and series2 must have equal length"
    )
    result <- rep(NA_real_, length(series1))
    valid <- !is.na(series1) & !is.na(series2)
    if (any(valid)) {
      out <- transform_fn(
        series1[valid], series2[valid], ...
      )
      validate_transform_length(out, sum(valid))
      result[valid] <- out
    }
  }
  result
}
