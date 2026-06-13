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
#' @param yields Yields data
#' @param term_premia Term premia data
#' @param i Maturity
#' @template param-step
#' @return Previous period n_hat series
#' @keywords internal
compute_n_hat_previous <- function(yields, term_premia, i,
                                   step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  if (i == step) {
    # Boundary: the realized log price of the step-maturity bond,
    # -m(step) * y_t^(step), already observable at date t
    y_step <- require_column(
      yields, acm_column_name("yields", step), "yields"
    )
    m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
    -m_step * y_step / HETID_CONSTANTS$PERCENT_TO_DECIMAL
  } else {
    compute_n_hat(yields, term_premia, i - step, return_df = FALSE, step = step)
  }
}

#' Compute Time Series News
#'
#' Generic function to compute news as difference between future and current values
#'
#' @param current_series Current period series
#' @param future_series Future period series
#' @return News series
#' @keywords internal
compute_time_series_news <- function(current_series, future_series) {
  assert_dimension_ok(
    length(current_series) == length(future_series),
    "current_series and future_series must have equal length"
  )
  n_obs <- length(current_series)

  # Guard: need at least 2 obs for differencing
  if (n_obs < 2) {
    return(numeric(0))
  }

  # Compute news: future[t+1] - current[t]
  # NAs propagate naturally via R's NA arithmetic
  future_series[seq.int(2L, n_obs)] -
    current_series[seq_len(n_obs - 1L)]
}
