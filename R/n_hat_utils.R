#' N-Hat Series Utilities
#'
#' Helpers shared by the price-news and SDF-innovation chain: the
#' previous-period n_hat series and the generic news differencing.
#'
#' @name n_hat_utils
#' @keywords internal
NULL

#' Validate the Shared News-Kernel Input Contract
#'
#' The common preamble for the variance-bound news kernels: a valid
#' \code{step}, a maturity index within the estimator's ceiling, an
#' optional whole-news-period (step-multiple) check, and row-aligned
#' yields and term premia, in the historical call order.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#' @param step_multiple_reason Reason string for the step-multiple check,
#'   or \code{NULL} to skip it.
#' @param max_index When \code{TRUE}, cap \code{i} at
#'   \code{effective_max_maturity(step)}; when \code{FALSE} only require a
#'   positive index (the k_hat estimator reads no \code{i + step} column).
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
#' @param i Maturity index; the horizon-0 boundary (\code{i == 0}) is
#'   admitted here and handled by the callers.
#' @template param-step
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
#' @return Previous period n_hat series
#' @keywords internal
compute_n_hat_previous <- function(yields, term_premia, i,
                                   step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  if (i == step) {
    # Boundary: realized log price of the step-maturity bond, observable at t
    y_step <- require_acm_col(yields, "yields", step)
    m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
    -m_step * y_step / HETID_CONSTANTS$PERCENT_TO_DECIMAL
  } else {
    n_hat_series(yields, term_premia, i - step, step = step)
  }
}

#' Compute Time Series News
#'
#' Generic function to compute news as difference between future and current values
#'
#' @param current_series Current period series
#' @param future_series Future period series
#' @return News series (\code{length(current_series) - 1}), or
#'   \code{numeric(0)} when the inputs have fewer than two observations.
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
