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
#' @return Character vector, e.g. c("pc1", "pc2", ...); empty for zero PCs
#' @keywords internal
get_pc_column_names <- function(n_pcs) {
  # Explicit zero branch: paste0 would recycle the prefix against
  # integer(0) and return a bare "pc"
  if (n_pcs == 0) {
    return(character(0))
  }
  paste0(HETID_CONSTANTS$PC_PREFIX, seq_len(n_pcs))
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
  assert_dimension_ok(
    length(dates) == nrow(yields),
    "Length of dates must match number of rows in yields"
  )

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
  assert_insufficient_data_ok(
    length(clean_series) > 0,
    error_msg
  )
  mean(clean_series^2)
}

#' Run PC Regression
#'
#' Shared regression core for W1 and W2 residual computation.
#' Expects pre-aligned, pre-lagged inputs. Regressor labels come from
#' the matrix's own column names (sanitized for formula use); unnamed
#' or partially named input falls back to the bundled pc1..pcN names.
#'
#' @param y Numeric response vector
#' @param pcs Matrix of regressors (principal components in the
#'   bundled workflow)
#' @param n_pcs Number of leading columns to use
#'
#' @return List with residuals, fitted, coefficients,
#'   r_squared, model, and complete_idx
#' @keywords internal
run_pc_regression <- function(y, pcs, n_pcs) {
  # Subset to first n_pcs columns to avoid name
  # recycling when user supplies extra columns
  pcs <- pcs[, seq_len(n_pcs), drop = FALSE]

  complete_idx <- complete.cases(y, pcs)
  n_complete <- sum(complete_idx)
  min_obs_for_regression <- n_pcs + 2L
  if (n_complete < min_obs_for_regression) {
    stop_insufficient_data(paste0(
      "Insufficient complete observations for PC regression: got ",
      n_complete, ", need at least ", min_obs_for_regression,
      " (n_pcs + 2)"
    ))
  }
  y_clean <- y[complete_idx]
  pcs_clean <- pcs[complete_idx, , drop = FALSE]

  nms <- colnames(pcs)
  pc_names <- if (is.null(nms) || anyNA(nms) || !all(nzchar(nms))) {
    get_pc_column_names(n_pcs)
  } else {
    # Sanitize so the formula string and data.frame name mangling
    # agree for non-syntactic user names ("10y rate" -> "X10y.rate")
    make.names(nms, unique = TRUE)
  }
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
