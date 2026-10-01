#' N-Hat Series Utilities
#'
#' Helpers shared by the price-news and SDF-innovation chain: the
#' previous-period n_hat series and news between adjacent observations.
#'
#' @name n_hat_utils
#' @keywords internal
NULL

#' Validate the Shared News-Kernel Input Contract
#'
#' The common preamble for the variance-bound news kernels: a valid
#' \code{step}, a maturity index within the estimator's ceiling, an
#' optional whole-news-period (step-multiple) check, and row-aligned
#' yields and term premia.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#' @param step_multiple_reason Character reason string for requiring \code{i}
#'   to be a positive multiple of \code{step}, or \code{NULL} (the default)
#'   to skip that check.
#' @param max_index Logical flag. When \code{TRUE} (the default), cap \code{i}
#'   at \code{effective_max_maturity(step)}; otherwise cap it at
#'   \code{HETID_CONSTANTS$MAX_MATURITY} (the k_hat estimator reads no
#'   \code{i + step} column).
#' @details Both input frames must have the same number of rows. This helper
#'   does not check dates, column availability, units, or missing values.
#'   The news step must be a positive integer no larger than
#'   \code{HETID_CONSTANTS$MAX_MATURITY \%/\% 2L}.
#' @return Invisible \code{TRUE}; stops with a structured error otherwise.
#' @keywords internal
validate_news_kernel_inputs <- function(yields, term_premia, i, step,
                                        step_multiple_reason = NULL,
                                        max_index = TRUE) {
  validate_step(step)
  if (isTRUE(max_index)) {
    validate_maturity_index(i, max_maturity = effective_max_maturity(step))
  } else {
    validate_maturity_index(i)
  }
  if (!is.null(step_multiple_reason)) {
    validate_step_multiple(i, step, step_multiple_reason)
  }
  validate_row_alignment(yields, term_premia)
  invisible(TRUE)
}

#' Validate the Shared Expected-SDF Input Contract
#'
#' The common preamble for the expected-SDF kernels: a valid \code{step},
#' a maturity index in \code{[0, effective_max_maturity(step)]} (the lower
#' bound of 0 admits the horizon-0 boundary the callers handle), and
#' row-aligned yields and term premia.
#'
#' @template param-yields-term-premia
#' @param i Integer maturity index in months between zero and
#'   \code{effective_max_maturity(step)}, inclusive. The horizon-0 boundary
#'   (\code{i == 0}) is admitted here and handled by the callers.
#' @template param-step
#' @details Both input frames must have the same number of rows. This helper
#'   does not check dates, column availability, units, or missing values.
#'   The news step must be a positive integer no larger than
#'   \code{HETID_CONSTANTS$MAX_MATURITY \%/\% 2L}.
#' @return Invisible \code{TRUE}; stops with a structured error otherwise.
#' @keywords internal
validate_expected_sdf_inputs <- function(yields, term_premia, i, step) {
  validate_step(step)
  assert_scalar_integer_in_range(
    i, "Maturity index i", 0L, effective_max_maturity(step),
    arg = "i"
  )
  validate_row_alignment(yields, term_premia)
  invisible(TRUE)
}

#' Compute Previous Period N-Hat
#'
#' Handles the boundary case \code{i == step}, where the
#' previous-period index \code{i - step = 0} denotes the realized
#' one-period bond: n_hat(0,t) = E_t\[p_t^(step)\] = p_t^(step), the
#' log price of the step-maturity bond.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#' @details The maturity index must equal \code{step} or satisfy
#'   \code{i - step >= HETID_CONSTANTS$MIN_MATURITY}.
#'   For \code{i == step}, only the \code{yields} column at maturity
#'   \code{step} is read; \code{term_premia} is unused. For \code{i > step},
#'   both input frames need columns at maturities \code{i - step} and \code{i},
#'   and \code{i} must not exceed \code{HETID_CONSTANTS$MAX_MATURITY}.
#'   Yields and term premia are annualized percentage points. Missing values
#'   propagate through the calculation; rows are not removed. The output has
#'   no dates, and input rows must already refer to matching dates.
#'   The nonboundary branch checks row counts and warns with
#'   \code{hetid_warning_unit_scale} when yields appear to be decimal values.
#' @return Numeric vector of length \code{nrow(yields)}: the previous-period
#'   n_hat series, \eqn{n\_hat(i - step, t)}.
#' @keywords internal
compute_n_hat_previous <- function(yields, term_premia, i,
                                   step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  if (i == step) {
    y_step <- require_acm_col(yields, "yields", step)
    m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
    -m_step * y_step / HETID_CONSTANTS$PERCENT_TO_DECIMAL
  } else {
    n_hat_series(yields, term_premia, i - step, step = step)
  }
}

#' Compute Time Series News
#'
#' Computes news by subtracting each current-period value from the
#' future series in the following row.
#'
#' @param current_series Numeric vector of current-period values.
#' @param future_series Numeric vector of future-period values, the same length
#'   as \code{current_series}.
#' @return Numeric vector of length \code{length(current_series) - 1}, or
#'   \code{numeric(0)} when the inputs have fewer than two observations.
#' @details Element \code{t} is \code{future_series[t + 1] - current_series[t]}.
#'   Missing values in either paired observation propagate to that element;
#'   observations are not removed. Unequal input lengths raise a
#'   \code{hetid_error_dimension_mismatch}. The output has no dates; input
#'   positions must already refer to corresponding periods.
#' @keywords internal
compute_time_series_news <- function(current_series, future_series) {
  assert_dimension_ok(
    length(current_series) == length(future_series),
    "current_series and future_series must have equal length"
  )
  n_obs <- length(current_series)

  if (n_obs < 2) {
    return(numeric(0))
  }

  future_series[seq.int(2L, n_obs)] -
    current_series[seq_len(n_obs - 1L)]
}
