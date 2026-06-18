#' Compute Expected Stochastic Discount Factor (expected_sdf)
#'
#' Computes the time series approximating the conditional expectation of
#' the one-period stochastic discount factor \code{s = i / step} news
#' periods ahead, \eqn{E_t[\mathrm{SDF}_{t+1+s}]}, using the expected
#' log one-period price \code{n_hat(i, t)} plus an empirical bias
#' correction. The unconditional expectation is replaced by the sample
#' average over the observations.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-return-df-dates
#' @template param-step
#'
#' @template return-numeric-or-dataframe
#'
#' @section Mathematical Formula:
#' With \code{s = i / step} news periods, \eqn{m(\mathrm{step})} the step
#' maturity in years, and \code{n_hat(i, t)} the expected log one-period
#' price from \code{\link{compute_n_hat}}, the estimator is
#' \deqn{\exp(n\_hat(i,t)) + \frac{1}{|T_i|} \sum_{t \in T_i}
#'   \left( e^{-m(\mathrm{step}) y^{(\mathrm{step})}_{t+s} / 100}
#'   - \exp(n\_hat(i,t)) \right)}
#' over the bound index set \eqn{T_i = \{1, \dots, T - s\}}. The leading
#' term \eqn{\exp(n\_hat(i,t))} is the conditional approximation to
#' \eqn{E_t[e^{-y^{(1)}_{t+s}}]}; the constant correction is the sample
#' analogue of the unconditional
#' \eqn{E[e^{-y^{(1)}_{t+s}} - \exp(n\_hat(i,t))]}. The realized
#' one-period price \eqn{e^{-y^{(1)}_{t+s}}} equals
#' \eqn{\exp(-m(\mathrm{step}) y^{(\mathrm{step})}_{t+s} / 100)}, the
#' step-maturity (one-period) bond at date \eqn{t + s}.
#'
#' @section Time units:
#' The realized one-period yield is led \code{i / step} rows. Rows are
#' whatever observation frequency the caller supplies; the shift counts
#' news periods, not calendar time, so row frequency must equal the
#' intended news period. \code{i} must be a positive multiple of
#' \code{step}.
#'
#' @details
#' The additive constant mean-matches the estimator to the realized
#' one-period price: with complete data, averaging the returned series
#' over \eqn{T_i} equals the sample mean of \eqn{e^{-y^{(1)}_{t+s}}}. The
#' two means in the correction share one index set - the dates where both
#' legs are finite - so missing or non-finite values never misalign them.
#'
#' Because the correction is an unconditional additive constant, it
#' preserves the mean but does not guarantee that every fitted value is
#' positive: at dates where \eqn{\exp(n\_hat(i,t))} is tiny and the
#' correction is negative, the estimator can dip below zero. This is a
#' property of the requested additive form, not a defect.
#'
#' At the one-period horizon \code{i == step} (\code{s == 1}), the
#' \code{n_hat} normalization \code{TP^(1) := 0} drops the one-period
#' term premium, so the result does not depend on \code{tp\{step\}} (only
#' on the \code{y\{step\}} yield used by the realized leg).
#'
#' The correction is estimated over the whole sample from realized future
#' one-period prices, so the returned \eqn{E_t}-labelled series is an
#' in-sample fitted object, not a pseudo-out-of-sample forecast; it must
#' not be fed into a real-time backtest. The \code{is.finite()} mask
#' guards only the scalar correction: any \code{NA}/\code{Inf} carried by
#' \code{n_hat(i, t)} itself (e.g. at the unpaired tail dates) propagates
#' to the corresponding output values.
#'
#' Besides the \code{i} and \code{i + step} columns \code{compute_n_hat}
#' uses, the realized leg requires the one-period yield column
#' \code{y\{step\}}; at \code{i == step} a \code{tp\{step\}} column must
#' still be present, though its value is ignored.
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step}
#'   (108 for standard ACM data with the default annual step), because
#'   \code{n_hat(i, t)} requires data at maturity \code{i + step}.
#'
#' @seealso \code{\link{compute_n_hat}}, \code{\link{compute_sdf_innovations}}
#'
#' @export
#'
#' @examples
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#' yields <- data[, paste0("y", seq(12, 120, 12))]
#' term_premia <- data[, paste0("tp", seq(12, 120, 12))]
#'
#' # Expected SDF for the 5-year (60-month) horizon (s = 5 news periods)
#' expected_sdf_60 <- compute_expected_sdf(yields, term_premia, i = 60)
#'
#' # With dates
#' expected_sdf_60_df <- compute_expected_sdf(
#'   yields, term_premia,
#'   i = 60,
#'   return_df = TRUE,
#'   dates = data$date
#' )
#'
compute_expected_sdf <- function(yields, term_premia, i,
                                 return_df = FALSE, dates = NULL,
                                 step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  validate_maturity_index(i, max_maturity = effective_max_maturity(step))
  validate_step_multiple(
    i, step,
    "the realized one-period yield is led whole news periods"
  )
  validate_row_alignment(yields, term_premia)

  # Expected log one-period price s periods ahead, n_hat(i, t) (decimals).
  # compute_n_hat validates yield units and the i / i+step columns.
  n_hat <- compute_n_hat(yields, term_premia, i, step = step)

  # Realized one-period bond is the step-maturity bond, percentage points
  y_step <- require_column(
    yields, acm_column_name("yields", step), "yields"
  )

  # The realized one-period price is observed s = i / step news periods
  # ahead, so the unconditional average pairs exp(n_hat(i, t)) at t with
  # the step-bond yield at t + s over T_i = {1, ..., T - s}
  horizon_periods <- i %/% step
  n_obs <- length(n_hat)
  assert_insufficient_data_ok(
    n_obs > horizon_periods,
    "Not enough observations. Need T > i/step news periods"
  )

  exp_n_hat <- exp(n_hat)
  exp_n_hat_paired <- exp_n_hat[seq_len(n_obs - horizon_periods)]
  y_step_future <- y_step[seq.int(horizon_periods + 1L, n_obs)]

  # Realized one-period price e^{-y^(1)_{t+s}} = exp(p^(step)_{t+s}):
  # the step-bond log price scales the annualized yield by its maturity
  # in years, then converts percentage points to decimals
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  realized_price <- exp(
    -m_step * y_step_future / HETID_CONSTANTS$PERCENT_TO_DECIMAL
  )

  # Empirical analogue of E[e^{-y^(1)} - exp(n_hat)]: average the
  # difference over the dates where both legs are finite, so the two
  # means share a single index set. is.finite() (not !is.na()) also
  # drops any Inf from an exp() overflow, which would otherwise poison
  # the scalar correction and the whole output series.
  valid <- is.finite(realized_price) & is.finite(exp_n_hat_paired)
  assert_insufficient_data_ok(
    any(valid),
    "No valid observations to estimate the expected-SDF correction"
  )
  correction <- mean(realized_price[valid] - exp_n_hat_paired[valid])

  # Conditional approximation plus the constant unconditional correction
  expected_sdf <- exp_n_hat + correction

  prepare_return_data(
    expected_sdf, return_df, dates, yields, "expected_sdf"
  )
}
