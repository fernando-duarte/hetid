#' N-Hat Series Utilities
#'
#' Helpers shared by the price-news and SDF-innovation chain: the
#' previous-period n_hat series and the generic news differencing.
#'
#' @name n_hat_utils
#' @keywords internal
NULL

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
    y_step <- require_column(
      yields, acm_column_name("yields", step), "yields"
    )
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
