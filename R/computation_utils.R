#' Computation Utilities for Term Structure Analysis
#'
#' Common utility functions for price news and SDF innovations computations
#'
#' @name computation_utils
#' @keywords internal
NULL

#' Validate Maturity Parameter
#'
#' @param i Maturity parameter
#' @keywords internal
validate_maturity_param <- function(i) {
  if (i < HETID_CONSTANTS$MIN_MATURITY) {
    stop("i must be >= ", HETID_CONSTANTS$MIN_MATURITY)
  }
  invisible(TRUE)
}

#' Compute N-Hat Series with Validation
#'
#' @param yields Yields data
#' @param term_premia Term premia data
#' @param i Maturity
#' @param return_df Whether to return data frame
#' @param dates Optional dates
#' @keywords internal
compute_n_hat_validated <- function(yields, term_premia, i, return_df = FALSE, dates = NULL) {
  validate_maturity_param(i)

  compute_n_hat(yields, term_premia, i,
    return_df = return_df, dates = dates
  )
}

#' Compute Previous Period N-Hat
#'
#' Handles the special case for i=1 where n_hat(0,t) = -y1
#'
#' @param yields Yields data
#' @param term_premia Term premia data
#' @param i Maturity
#' @param dates Optional dates
#' @return Previous period n_hat series
#' @keywords internal
compute_n_hat_previous <- function(yields, term_premia, i, dates = NULL) {
  if (i == 1) {
    # For i=1, n_hat(0,t) = E_t[p_(t+0)^(1)] = p_t^(1) = -y_t^(1)
    y1 <- yields[["y1"]]
    if (is.null(y1)) {
      stop("y1 column not found in yields")
    }
    return(-y1 / HETID_CONSTANTS$PERCENT_TO_DECIMAL) # Convert to decimal
  } else {
    return(compute_n_hat(yields, term_premia, i - 1,
      return_df = FALSE, dates = dates
    ))
  }
}

#' Compute Time Series News
#'
#' Generic function to compute news as difference between future and current values
#'
#' @param current_series Current period series
#' @param future_series Future period series
#' @param negate Whether to negate the result (for yield news)
#' @return News series
#' @keywords internal
compute_time_series_news <- function(current_series, future_series, negate = FALSE) {
  n_obs <- length(current_series)
  news <- rep(NA, n_obs - 1)

  # Compute news: future[t+1] - current[t]
  for (t in 1:(n_obs - 1)) {
    if (!is.na(future_series[t + 1]) && !is.na(current_series[t])) {
      news[t] <- future_series[t + 1] - current_series[t]
    }
  }

  if (negate) {
    news <- -news
  }

  return(news)
}

#' Prepare Return Data Frame
#'
#' Common logic for preparing return data frames with dates
#'
#' @param result_series The computed series
#' @param return_df Whether to return data frame
#' @param dates Optional dates vector
#' @param yields Original yields data for fallback dates
#' @param series_name Name for the series column
#' @return Either the series or a data frame
#' @keywords internal
prepare_return_data <- function(result_series, return_df, dates, yields, series_name) {
  if (!return_df) {
    return(result_series)
  }

  # Use provided dates, or create generic time index
  if (is.null(dates)) {
    dates <- seq_len(nrow(yields))
  }

  # Ensure dates is the same length as the data
  if (length(dates) != nrow(yields)) {
    stop("Length of dates must match number of rows in yields")
  }

  # For news series (T-1 elements), align with dates by adding NA at beginning
  # This represents that news[t] corresponds to change from t to t+1
  if (length(result_series) == length(dates) - 1) {
    result_aligned <- c(NA, result_series)
  } else {
    result_aligned <- result_series
  }

  result_df <- data.frame(
    date = dates,
    stringsAsFactors = FALSE
  )
  result_df[[series_name]] <- result_aligned

  result_df
}

#' Compute Expected Squared Values
#'
#' Compute expectation of squared values, handling NA values
#'
#' @param series Input series
#' @param error_msg Error message if no valid values
#' @return Expected squared value
#' @keywords internal
compute_expected_squared <- function(series, error_msg = "No valid values to compute expectation") {
  clean_series <- series[!is.na(series)]
  if (length(clean_series) == 0) {
    stop(error_msg)
  }
  return(mean(clean_series^2))
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
apply_time_series_transform <- function(series1, series2 = NULL, transform_fn, ...) {
  if (is.null(series2)) {
    n_obs <- length(series1)
    result <- rep(NA, n_obs)

    for (t in 1:n_obs) {
      if (!is.na(series1[t])) {
        result[t] <- transform_fn(series1[t], ...)
      }
    }
  } else {
    n_obs <- min(length(series1), length(series2))
    result <- rep(NA, n_obs)

    for (t in 1:n_obs) {
      if (!is.na(series1[t]) && !is.na(series2[t])) {
        result[t] <- transform_fn(series1[t], series2[t], ...)
      }
    }
  }

  result
}
