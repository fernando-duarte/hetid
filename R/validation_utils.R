#' Validation Utilities
#'
#' Validation helpers that consolidate common input checking patterns.
#'
#' @name validation_utils
#' @keywords internal
NULL

#' Assert Scalar Finite Value
#'
#' Internal guard for parameters that must be a single
#' finite numeric value.
#'
#' @param x Value to check
#' @param name Parameter name for the error message
#'
#' @return Invisible NULL; stops with error if invalid
#' @keywords internal
assert_scalar_finite <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
    stop(name, " must be a single finite numeric value",
      call. = FALSE
    )
  }
}

#' Validate Maturity Index
#'
#' Validates maturity index against dataset constraints.
#'
#' @param i Integer maturity index to validate
#' @param max_maturity Maximum allowed maturity (default from ACM dataset limit)
#'
#' @return Invisible TRUE if valid, stops with informative error if invalid
#' @keywords internal
validate_maturity_index <- function(i, max_maturity = HETID_CONSTANTS$MAX_MATURITY) {
  assert_scalar_finite(i, "Maturity index i")

  if (i %% 1 != 0) {
    stop("Maturity index i must be an integer", call. = FALSE)
  }

  if (i < HETID_CONSTANTS$MIN_MATURITY || i > max_maturity) {
    stop(
      "Maturity index i must be between ", HETID_CONSTANTS$MIN_MATURITY,
      " and ", max_maturity,
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate Data Dimensions
#'
#' Validates that yields and term premia have consistent dimensions.
#'
#' @param yields Yields data (matrix or data frame)
#' @param term_premia Term premia data (matrix or data frame)
#'
#' @return Invisible TRUE if valid, stops with informative error if invalid
#' @keywords internal
validate_data_dimensions <- function(yields, term_premia) {
  if (nrow(yields) != nrow(term_premia)) {
    stop(
      "yields and term_premia must have same number of observations. ",
      "Got ", nrow(yields), " vs ", nrow(term_premia), " rows.",
      call. = FALSE
    )
  }

  if (ncol(yields) != ncol(term_premia)) {
    stop(
      "yields and term_premia must have same number of maturities. ",
      "Got ", ncol(yields), " vs ", ncol(term_premia), " columns.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate Number of Principal Components
#'
#' Validates n_pcs parameter for principal components.
#'
#' @param n_pcs Number of principal components to validate
#'
#' @return Invisible TRUE if valid, stops with informative error if invalid
#' @keywords internal
validate_n_pcs <- function(n_pcs) {
  assert_scalar_finite(n_pcs, "n_pcs")

  if (n_pcs %% 1 != 0) {
    stop("n_pcs must be an integer", call. = FALSE)
  }

  if (n_pcs < 1 || n_pcs > HETID_CONSTANTS$MAX_N_PCS) {
    stop(
      "n_pcs must be between 1 and ", HETID_CONSTANTS$MAX_N_PCS,
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate Minimum Observations
#'
#' Validates that sufficient observations are available for statistical estimation.
#'
#' @param n Number of observations to validate
#' @param min_obs Minimum required observations
#'
#' @return Invisible TRUE if valid, stops with informative error if invalid
#' @keywords internal
validate_min_observations <- function(n, min_obs = HETID_CONSTANTS$MIN_OBSERVATIONS) {
  assert_scalar_finite(n, "Number of observations")

  if (n < min_obs) {
    stop(
      "Not enough complete observations (need at least ", min_obs,
      ")",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate Time Series Lengths
#'
#' Validates that multiple time series have consistent lengths.
#'
#' @param ... Time series vectors to validate
#'
#' @return Invisible TRUE if valid, stops with informative error if invalid
#' @keywords internal
validate_time_series_lengths <- function(...) {
  series_list <- list(...)

  if (length(series_list) < 2) {
    stop("At least two time series required for length validation",
      call. = FALSE
    )
  }

  series_lengths <- lengths(series_list)

  if (length(unique(series_lengths)) > 1) {
    stop(
      "All input time series must have the same length. ",
      "Got lengths: ",
      paste(series_lengths, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate Numeric Inputs
#'
#' Validates that inputs are numeric vectors for mathematical computation.
#'
#' @param ... Named numeric vectors to validate
#'
#' @return Invisible TRUE if valid, stops with informative error if invalid
#' @keywords internal
validate_numeric_inputs <- function(...) {
  inputs <- list(...)
  input_names <- names(inputs)

  if (is.null(input_names)) {
    input_names <- paste0("input_", seq_along(inputs))
  }

  for (i in seq_along(inputs)) {
    if (!is.numeric(inputs[[i]])) {
      stop(input_names[i], " must be a numeric vector",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}
