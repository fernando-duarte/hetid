#' Computation Utilities for Term Structure Analysis
#'
#' Internal helpers for regression coefficients, dated series, and variance-bound trimming.
#'
#' @name computation_utils
#' @keywords internal
NULL

#' Build PC Column Names
#'
#' Builds sequential names using \code{HETID_CONSTANTS$PC_PREFIX}.
#'
#' @param n_pcs Non-negative integer count of principal components.
#' @return Character vector of length \code{n_pcs}; empty for zero PCs.
#' @keywords internal
get_pc_column_names <- function(n_pcs) {
  # paste0 against integer(0) returns "pc", not character(0)
  if (n_pcs == 0) {
    return(character(0))
  }
  paste0(HETID_CONSTANTS$PC_PREFIX, seq_len(n_pcs))
}

#' Assemble the \eqn{\omega_2} Coefficient Matrix
#'
#' Builds the per-maturity coefficient matrix from the list of
#' regression coefficient vectors, taking column names from the
#' regression output itself (the single source). A skipped maturity
#' (NULL entry) contributes an all-NA row; when every maturity was
#' skipped, \code{fallback_names} supplies the columns.
#'
#' @details Non-\code{NULL} vectors must have the same length and coefficient order.
#'   Values are assigned by position, without matching coefficient names.
#'
#' @param coef_list List of named numeric coefficient vectors, one per maturity;
#'   \code{NULL} entries denote skipped maturities.
#' @param row_names Character vector of row labels, one per entry of \code{coef_list}.
#' @param fallback_names Character vector of column names used only when all entries
#'   are \code{NULL}.
#' @return Numeric matrix with \code{length(coef_list)} rows, named by \code{row_names}.
#'   Columns use the names of the first non-\code{NULL} vector, or \code{fallback_names}
#'   when all entries are \code{NULL}. Skipped rows contain \code{NA_real_}.
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
#' errors), the \code{process_w2_maturity} pre-check (which skips), and the
#' log-variance and tau-zero input checks.
#'
#' @param n_pcs Non-negative integer count of regressors, including any own-lag columns.
#' @return Numeric scalar \code{n_pcs + 2L}, the minimum complete-observation count;
#'   an integer when \code{n_pcs} is an integer.
#' @keywords internal
min_obs_for_pc_regression <- function(n_pcs) {
  n_pcs + 2L
}

#' Attach Dates to a Time-Series Output
#'
#' Attaches the supplied dates to a computed level or news series. Dates are mandatory
#' and validated as a non-missing \code{Date} vector of the same length as the rows
#' of \code{yields}. Dates are preserved; callers supply period-end labels.
#'
#' @param result_series Numeric vector with \code{length(dates)} elements for a level
#'   series, or \code{length(dates) - 1} elements when \code{is_news} is \code{TRUE}.
#'   Missing values are retained.
#' @param dates Non-missing period-end \code{Date} vector, one per row of \code{yields}.
#' @param yields Original yields matrix or data frame, used only for the row-count check.
#' @param series_name Character scalar naming the value column, distinct from \code{"date"}.
#' @param is_news Logical scalar. If \code{TRUE}, \code{result_series} has one
#'   element per period change (T - 1 elements), aligned to the dates by
#'   prepending \code{NA_real_} so element \code{k} (the change from \code{k} to \code{k + 1})
#'   carries \code{dates[k + 1]}. If \code{FALSE} (the default), it is a level series
#'   carrying its own date.
#' @return Data frame with \code{length(dates)} rows, the unchanged \code{date} column,
#'   and a value column named by \code{series_name}.
#' @details Invalid or missing dates signal \code{hetid_error_bad_argument}.
#'   Date or series length mismatches signal \code{hetid_error_dimension_mismatch}.
#' @keywords internal
prepare_return_data <- function(result_series, dates, yields,
                                series_name, is_news = FALSE) {
  # Report invalid dates before checking the series length
  validate_dates_vector(dates, nrow(yields))

  expected_length <- if (is_news) length(dates) - 1L else length(dates)
  assert_dimension_ok(
    length(result_series) == expected_length,
    sprintf(
      "Length of result_series (%d) must be %d for a %s series of %d dates",
      length(result_series), expected_length,
      if (is_news) "news" else "level", length(dates)
    )
  )

  result_aligned <- if (is_news) c(NA_real_, result_series) else result_series

  result_df <- data.frame(date = dates, stringsAsFactors = FALSE)
  result_df[[series_name]] <- result_aligned

  result_df
}

#' Trim a Series to the Bound Index Set
#'
#' Shared trim for the variance-bound kernels: keeps indices
#' \eqn{T_i = \{1, \dots, T - i/step\}}, then removes missing values,
#' leaving each caller its own reduction. \code{len_offset}
#' adapts the length bookkeeping for a length-T level series
#' (\code{n_hat}, offset 0) versus a T-1 news series (\code{delta_p},
#' offset 1). Callers validate \code{i} as a positive multiple of
#' \code{step} so \code{i/step} is a whole number of periods.
#'
#' @param series Numeric series to trim.
#' @param i Integer maturity index in months, a positive multiple of \code{step}.
#' @param step Positive integer number of months per news period.
#' @param len_offset Integer length adjustment: \code{0L} (the default) for a length-T
#'   level series, or \code{1L} for a T-1 news series.
#' @return Numeric vector in input order with \code{NA} and \code{NaN} removed from
#'   the retained range; empty when all retained values are missing.
#' @details Signals \code{hetid_error_insufficient_data} when
#'   \code{length(series) + len_offset <= i/step}. Other input validation belongs
#'   to the caller.
#' @keywords internal
trim_to_bound_index_set <- function(series, i, step, len_offset = 0L) {
  horizon_periods <- i %/% step
  n <- length(series)
  assert_insufficient_data_ok(
    n + len_offset > horizon_periods,
    HETID_CONSTANTS$INSUFFICIENT_NEWS_MSG
  )
  trimmed <- series[seq_len(n - horizon_periods + len_offset)]
  trimmed[!is.na(trimmed)]
}
