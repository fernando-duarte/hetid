#' Validation Utilities
#'
#' Validation helpers that consolidate common input checking patterns.
#'
#' @name validation_utils
#' @keywords internal
NULL

#' Assert Bad Argument Invariant
#'
#' Internal guard that raises a bad-argument condition
#' when `ok` is not `TRUE`.
#'
#' @param ok Logical scalar indicating whether validation passed
#' @param message Error message for the failing condition
#' @param arg Optional argument name for the condition payload
#'
#' @return Invisible TRUE when validation passes
#' @keywords internal
#' @noRd
assert_bad_argument_ok <- function(ok, message, arg = NULL) {
  if (!isTRUE(ok)) {
    stop_bad_argument(message, arg = arg)
  }

  invisible(TRUE)
}

#' Assert Dimension Invariant
#'
#' Internal guard that raises a dimension-mismatch condition
#' when `ok` is not `TRUE`.
#'
#' @param ok Logical scalar indicating whether validation passed
#' @param message Error message for the failing condition
#'
#' @return Invisible TRUE when validation passes
#' @keywords internal
#' @noRd
assert_dimension_ok <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop_dimension_mismatch(message)
  }

  invisible(TRUE)
}

#' Assert Data Availability Invariant
#'
#' Internal guard that raises an insufficient-data condition
#' when `ok` is not `TRUE`.
#'
#' @param ok Logical scalar indicating whether validation passed
#' @param message Error message for the failing condition
#'
#' @return Invisible TRUE when validation passes
#' @keywords internal
#' @noRd
assert_insufficient_data_ok <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop_insufficient_data(message)
  }

  invisible(TRUE)
}

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
    stop_bad_argument(
      paste0(name, " must be a single finite numeric value"),
      arg = name
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

  assert_bad_argument_ok(
    i %% 1 == 0,
    "Maturity index i must be an integer",
    arg = "i"
  )
  assert_bad_argument_ok(
    i >= HETID_CONSTANTS$MIN_MATURITY &&
      i <= max_maturity,
    paste0(
      "Maturity index i must be between ",
      HETID_CONSTANTS$MIN_MATURITY,
      " and ", max_maturity
    ),
    arg = "i"
  )

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
  assert_dimension_ok(
    nrow(yields) == nrow(term_premia),
    paste0(
      "yields and term_premia must have same number of ",
      "observations. Got ", nrow(yields),
      " vs ", nrow(term_premia), " rows."
    )
  )
  assert_dimension_ok(
    ncol(yields) == ncol(term_premia),
    paste0(
      "yields and term_premia must have same number of ",
      "maturities. Got ", ncol(yields),
      " vs ", ncol(term_premia), " columns."
    )
  )

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

  assert_bad_argument_ok(
    n_pcs %% 1 == 0,
    "n_pcs must be an integer",
    arg = "n_pcs"
  )
  assert_bad_argument_ok(
    n_pcs >= 1 && n_pcs <= HETID_CONSTANTS$MAX_N_PCS,
    paste0(
      "n_pcs must be between 1 and ",
      HETID_CONSTANTS$MAX_N_PCS
    ),
    arg = "n_pcs"
  )

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

  assert_insufficient_data_ok(
    n >= min_obs,
    paste0(
      "Not enough complete observations (need at least ",
      min_obs, ")"
    )
  )

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

  assert_bad_argument_ok(
    length(series_list) >= 2,
    "At least two time series required for length validation"
  )

  series_lengths <- lengths(series_list)

  assert_dimension_ok(
    length(unique(series_lengths)) == 1,
    paste0(
      "All input time series must have the same length. ",
      "Got lengths: ",
      paste(series_lengths, collapse = ", ")
    )
  )

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
    assert_bad_argument_ok(
      is.numeric(inputs[[i]]),
      paste0(input_names[i], " must be a numeric vector"),
      arg = input_names[i]
    )
  }

  invisible(TRUE)
}
