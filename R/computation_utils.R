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

#' Assemble the W2 Coefficient Matrix
#'
#' Builds the per-maturity coefficient matrix from the list of
#' regression coefficient vectors, taking column names from the
#' regression output itself (the single source). A skipped maturity
#' (NULL entry) contributes an all-NA row; when every maturity was
#' skipped, \code{fallback_names} supplies the columns.
#'
#' @param coef_list List of coefficient vectors (NULL for skipped),
#'   one per maturity
#' @param row_names Maturity row names
#' @param fallback_names Column names used only when all entries are NULL
#' @return Numeric matrix with one row per maturity
#' @keywords internal
assemble_w2_coef_matrix <- function(coef_list, row_names, fallback_names) {
  fitted_coef <- Filter(Negate(is.null), coef_list)
  coef_names <- if (length(fitted_coef) > 0) {
    names(fitted_coef[[1]])
  } else {
    fallback_names
  }
  coef_matrix <- matrix(
    NA_real_,
    nrow = length(coef_list), ncol = length(coef_names),
    dimnames = list(row_names, coef_names)
  )
  for (idx in seq_along(coef_list)) {
    if (!is.null(coef_list[[idx]])) {
      coef_matrix[idx, ] <- coef_list[[idx]]
    }
  }
  coef_matrix
}

#' Minimum Complete Observations for PC Regression
#'
#' Single source of truth for the "need at least n_pcs + 2 complete
#' observations" rule, shared by \code{\link{run_pc_regression}} (which
#' errors) and the \code{process_w2_maturity} pre-check (which skips).
#'
#' @param n_pcs Number of principal components
#' @return Integer minimum complete-observation count
#' @keywords internal
min_obs_for_pc_regression <- function(n_pcs) {
  n_pcs + 2L
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
#' @param is_news Logical. If TRUE, \code{result_series} is a news series
#'   with one element per period change (T - 1 elements), aligned to the
#'   dates by prepending NA. If FALSE (the default), it is a level series
#'   with one element per date. The length is checked against this
#'   expectation rather than inferred.
#' @return Either the series or a data frame
#' @keywords internal
prepare_return_data <- function(result_series, return_df, dates, yields,
                                series_name, is_news = FALSE) {
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

  # A news series carries one element per period change (T - 1); a level
  # series carries one per date. Check the length against the declared
  # kind instead of guessing, so an unexpected mismatch fails loudly.
  expected_length <- if (is_news) length(dates) - 1L else length(dates)
  assert_dimension_ok(
    length(result_series) == expected_length,
    sprintf(
      "Length of result_series (%d) must be %d for a %s series of %d dates",
      length(result_series), expected_length,
      if (is_news) "news" else "level", length(dates)
    )
  )

  # A news series aligns to the dates by prepending NA: news[t] is the
  # change from t to t + 1.
  result_aligned <- if (is_news) c(NA, result_series) else result_series

  result_df <- data.frame(
    date = dates,
    stringsAsFactors = FALSE
  )
  result_df[[series_name]] <- result_aligned

  result_df
}
