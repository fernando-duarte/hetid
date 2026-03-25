#' Computation Utilities for Term Structure Analysis
#'
#' Common utility functions for price news and SDF innovations computations
#'
#' @name computation_utils
#' @keywords internal
NULL

#' Build PC Column Names
#'
#' @param n_pcs Number of principal components
#' @return Character vector, e.g. c("pc1", "pc2", ...)
#' @keywords internal
get_pc_column_names <- function(n_pcs) {
  paste0(HETID_CONSTANTS$PC_PREFIX, seq_len(n_pcs))
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
    y1 <- require_column(yields, "y1", "yields")
    -y1 / HETID_CONSTANTS$PERCENT_TO_DECIMAL # Convert to decimal
  } else {
    compute_n_hat(yields, term_premia, i - 1,
      return_df = FALSE, dates = dates
    )
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

  # Compute news: future[t+1] - current[t]
  # NAs propagate naturally via R's NA arithmetic
  ts_news <- future_series[2:n_obs] - current_series[1:(n_obs - 1)]

  if (negate) {
    ts_news <- -ts_news
  }

  ts_news
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
    stop(
      "Length of dates must match number of rows in yields",
      call. = FALSE
    )
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
    stop(error_msg, call. = FALSE)
  }
  mean(clean_series^2)
}

#' Run PC Regression
#'
#' Shared regression core for W1 and W2 residual computation.
#' Expects pre-aligned, pre-lagged inputs.
#'
#' @param y Numeric response vector
#' @param pcs Matrix of principal components
#' @param n_pcs Number of PCs to label
#'
#' @return List with residuals, fitted, coefficients,
#'   r_squared, model, and complete_idx
#' @keywords internal
run_pc_regression <- function(y, pcs, n_pcs) {
  # Subset to first n_pcs columns to avoid name
  # recycling when user supplies extra columns
  pcs <- pcs[, seq_len(n_pcs), drop = FALSE]

  complete_idx <- complete.cases(y, pcs)
  y_clean <- y[complete_idx]
  pcs_clean <- pcs[complete_idx, , drop = FALSE]

  pc_names <- get_pc_column_names(n_pcs)
  colnames(pcs_clean) <- pc_names
  formula_str <- paste(
    "y ~", paste(pc_names, collapse = " + ")
  )
  reg_data <- data.frame(y = y_clean, pcs_clean)
  model <- lm(
    as.formula(formula_str),
    data = reg_data
  )

  list(
    residuals = residuals(model),
    fitted = fitted(model),
    coefficients = coef(model),
    r_squared = summary(model)$r.squared,
    model = model,
    complete_idx = complete_idx
  )
}
