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
    stop_bad_argument(
      paste0(name, " must be a single finite numeric value"),
      arg = name
    )
  }
}

#' Assert a Scalar is an Integer Within a Closed Range
#'
#' Shared core for scalar integer-index validators. The \code{arg} defaults to
#' \code{name} so callers can keep a distinct condition \code{arg} field.
#'
#' @param x Value to check
#' @param name Parameter name used in the error message
#' @param min_value,max_value Inclusive integer bounds
#' @param arg Condition argument name (defaults to \code{name})
#'
#' @return Invisible TRUE if valid, stops with informative error otherwise.
#' @keywords internal
assert_scalar_integer_in_range <- function(x, name, min_value, max_value,
                                           arg = name) {
  assert_scalar_finite(x, name)
  assert_bad_argument_ok(
    x %% 1 == 0,
    paste0(name, " must be an integer"),
    arg = arg
  )
  assert_bad_argument_ok(
    x >= min_value && x <= max_value,
    paste0(name, " must be between ", min_value, " and ", max_value),
    arg = arg
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
  assert_scalar_integer_in_range(
    n_pcs, "n_pcs", 1, HETID_CONSTANTS$MAX_N_PCS
  )
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

#' Validate Equal Lengths Across Inputs
#'
#' Validates that multiple inputs (vectors or lists) have consistent lengths.
#' With \code{expected_length}, every input must equal that length; without it,
#' the inputs must merely share a common length.
#'
#' @param ... Vectors or lists whose lengths must agree
#' @param expected_length Optional single nonnegative integer; if supplied, every
#'   input must have exactly this length
#'
#' @return Invisible TRUE if valid, stops with informative error if invalid
#' @keywords internal
validate_time_series_lengths <- function(..., expected_length = NULL) {
  series_list <- list(...)
  series_lengths <- lengths(series_list)

  if (is.null(expected_length)) {
    assert_bad_argument_ok(
      length(series_list) >= 2,
      "At least two inputs required for length comparison"
    )
    lengths_ok <- length(unique(series_lengths)) == 1
    expectation <- "All inputs must have the same length"
  } else {
    assert_bad_argument_ok(
      is.numeric(expected_length) &&
        length(expected_length) == 1 &&
        is.finite(expected_length) &&
        expected_length >= 0 &&
        expected_length %% 1 == 0,
      "expected_length must be a single finite nonnegative integer",
      arg = "expected_length"
    )
    assert_bad_argument_ok(
      length(series_list) >= 1,
      "At least one input required for length validation"
    )
    lengths_ok <- all(series_lengths == expected_length)
    expectation <- paste0("All inputs must have length ", expected_length)
  }

  assert_dimension_ok(
    lengths_ok,
    paste0(
      expectation, ". Got lengths: ",
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
      is.numeric(inputs[[i]]) && is.null(dim(inputs[[i]])),
      paste0(input_names[i], " must be a numeric vector"),
      arg = input_names[i]
    )
  }

  invisible(TRUE)
}
