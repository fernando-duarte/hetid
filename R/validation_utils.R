#' Validation Utilities
#'
#' Validation helpers that consolidate common input checking patterns.
#'
#' @name validation_utils
#' @keywords internal
NULL

#' Validate Row Alignment of Yields and Term Premia
#'
#' Rows-only counterpart to \code{\link{validate_data_dimensions}} for the
#' bond-pricing entry points: their frames may carry different column
#' sets, but mismatched row counts must error, never recycle.
#'
#' @details Checks row counts only; callers must align observations by date.
#'   Column counts, names, and values are not checked, and empty frames are allowed.
#'
#' @param yields A matrix or data frame of yields, with observations in rows.
#' @param term_premia A matrix or data frame of term premia, with observations in rows.
#'
#' @return Invisible \code{TRUE} when row counts agree. Otherwise, signals a
#'   \code{hetid_error_dimension_mismatch} condition.
#' @keywords internal
validate_row_alignment <- function(yields, term_premia) {
  assert_dimension_ok(
    nrow(yields) == nrow(term_premia),
    paste0(
      "yields and term_premia must have same number of ",
      "observations. Got ", nrow(yields),
      " vs ", nrow(term_premia), " rows."
    )
  )
  invisible(TRUE)
}

#' Validate Data Dimensions
#'
#' Validates that yields and term premia have equal row and column counts.
#'
#' @details Checks dimensions only; dates, column names, and values are not checked.
#'   Empty matrices and data frames are allowed when their dimensions agree.
#'
#' @param yields A matrix or data frame of yields, with observations in rows.
#' @param term_premia A matrix or data frame of term premia, with observations in rows.
#'
#' @return Invisible \code{TRUE} when dimensions agree. Otherwise, signals a
#'   \code{hetid_error_dimension_mismatch} condition.
#' @keywords internal
validate_data_dimensions <- function(yields, term_premia) {
  validate_row_alignment(yields, term_premia)
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
#' Validates the number of principal components of nominal financial asset returns.
#'
#' @param n_pcs A single finite numeric value representing an integer from one to
#'   \code{HETID_CONSTANTS$MAX_N_PCS}, inclusive.
#'
#' @return Invisible \code{TRUE} when valid. Otherwise, signals a
#'   \code{hetid_error_bad_argument} condition with \code{arg = "n_pcs"}.
#' @keywords internal
validate_n_pcs <- function(n_pcs) {
  assert_scalar_integer_in_range(
    n_pcs, "n_pcs", 1, HETID_CONSTANTS$MAX_N_PCS
  )
}

#' Validate Equal Lengths Across Inputs
#'
#' Validates that multiple inputs (vectors or lists) have consistent lengths.
#' With \code{expected_length}, every input must equal that length; without it,
#' the inputs must merely share a common length.
#'
#' @details Checks lengths only; values, types, and dates are not checked.
#'   Empty inputs are allowed when their lengths satisfy the requested comparison.
#'
#' @param ... Vectors or lists whose lengths must agree. Supply at least two inputs
#'   when \code{expected_length = NULL}, or at least one when a length is supplied.
#' @param expected_length A single finite nonnegative numeric integer, or
#'   \code{NULL} (the default) to compare input lengths with each other.
#'
#' @return Invisible \code{TRUE} when lengths agree. Signals a
#'   \code{hetid_error_bad_argument} condition for an invalid expected length or too
#'   few inputs, or \code{hetid_error_dimension_mismatch} for unequal lengths.
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
#' @details Integer and double vectors are accepted, including empty vectors and
#'   missing or nonfinite values. Matrices, arrays, and nonnumeric inputs are rejected.
#'   Supplying no inputs succeeds.
#'
#' @param ... Numeric vectors, optionally named. Names identify invalid inputs in
#'   errors; unnamed inputs are identified as \code{input_1}, \code{input_2}, and so on.
#'
#' @return Invisible \code{TRUE} when all inputs are numeric vectors. Otherwise,
#'   signals a \code{hetid_error_bad_argument} condition whose \code{arg} field
#'   identifies the first invalid input.
#' @keywords internal
validate_numeric_inputs <- function(...) {
  inputs <- list(...)
  input_names <- names(inputs)

  if (is.null(input_names)) {
    input_names <- paste0("input_", seq_along(inputs))
  } else {
    # Partially named calls yield "" for unnamed entries; fill only those
    unnamed <- !nzchar(input_names)
    input_names[unnamed] <- paste0(
      "input_", seq_along(inputs)[unnamed]
    )
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
