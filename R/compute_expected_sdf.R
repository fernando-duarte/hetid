#' Compute Expected Stochastic Discount Factor (expected_sdf)
#'
#' Computes the time series approximating the conditional expectation of
#' the one-period stochastic discount factor \code{s = i / step} news
#' periods ahead, \eqn{E_t[\mathrm{SDF}_{t+1+s}]}, using the expected
#' log one-period price \code{n_hat(i, t)} plus an empirical bias
#' correction. The unconditional expectation is replaced by the sample
#' average over the observations.
#'
#' Two corrections are available. The default (\code{paired = FALSE}) is
#' horizon-agnostic: it estimates the bias as a difference of full-sample
#' means and admits any \code{i} on the \code{i + step} grid (no
#' multiple-of-step requirement). The \code{Mathematical Formula},
#' \code{Time units}, and \code{Details} sections below describe the previous
#' \code{paired = TRUE} matched-forecast-error correction, which leads the
#' realized leg \code{i / step} rows and so requires \code{i} to be a
#' positive multiple of \code{step}.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-dates-required
#' @template param-step
#' @param paired Logical; selects the bias-correction estimator. The
#'   default \code{FALSE} uses the horizon-agnostic correction (a difference
#'   of full-sample means over one common finite set), admitting any
#'   \code{i} on the \code{i + step} grid. \code{TRUE} restores the previous
#'   matched-forecast-error
#'   correction (the mean gap pairing the realized price \code{i / step}
#'   rows ahead with \code{exp(n_hat(i, t))}), which requires \code{i} to be
#'   a positive multiple of \code{step}.
#'
#' @template return-dated-dataframe
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
#' With \code{paired = TRUE} the realized one-period yield is led
#' \code{i / step} rows. Rows are whatever observation frequency the caller
#' supplies; the shift counts news periods, not calendar time, so row
#' frequency must equal the intended news period. In that mode \code{i}
#' must be a positive multiple of \code{step}; the default
#' \code{paired = FALSE} uses no lead and accepts any \code{i}.
#'
#' @details
#' These details describe \code{paired = TRUE}. The default
#' (\code{paired = FALSE}) instead mean-matches over the common finite set
#' to the unshifted one-period price \eqn{e^{-y^{(1)}_t}}, not over
#' \eqn{T_i} to the led \eqn{e^{-y^{(1)}_{t+s}}}; the two corrections differ
#' in finite samples even on multiples of \code{step}.
#'
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
#' @note Passing \code{i = 0} returns the horizon-zero boundary
#'   \eqn{E_t[\mathrm{SDF}_{t+1}] = P^{(1)}_t = e^{-y^{(1)}_t}}, the realized
#'   one-period price observed at \eqn{t}. It is exact: no forecast, no
#'   approximation, and no bias correction (\code{paired} is ignored). A
#'   \code{hetid_warning_horizon_zero} warning is signalled to flag this.
#'
#' @note To read off the shifted-information expectation
#'   \eqn{E_{t-j}[\mathrm{SDF}_{t+m}]}: with \eqn{s = m + j - 1} (require
#'   \eqn{s \ge 1} and \eqn{s\cdot step} a valid maturity index \eqn{\le}
#'   \code{effective_max_maturity(step)}), it equals
#'   \code{compute_expected_sdf(i = (m + j - 1) * step, paired = TRUE)}
#'   evaluated at the formation date \eqn{t-j} -- the row whose \code{date} is
#'   \eqn{t-j}. The series is dated by its conditioning date, so the information
#'   lag \eqn{j} only selects which row to read, not a separate computation. The
#'   matched (\code{paired = TRUE}) correction is the estimator this identity is
#'   defined against.
#'
#' @seealso \code{\link{compute_n_hat}}, \code{\link{compute_sdf_innovations}},
#'   \code{\link{compute_expected_sdf_variance_bound}}
#'
#' @export
#'
#' @examples
#' # Extract ACM data (the i = 60 horizon needs maturities 12, 60, and 72)
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(12, 60, 72)
#' )
#' yields <- data[, paste0("y", c(12, 60, 72))]
#' term_premia <- data[, paste0("tp", c(12, 60, 72))]
#'
#' # Expected SDF for the 5-year (60-month) horizon (s = 5 news periods)
#' expected_sdf_60 <- compute_expected_sdf(
#'   yields, term_premia,
#'   i = 60,
#'   dates = data$date
#' )
#'
compute_expected_sdf <- function(yields, term_premia, i, dates = NULL,
                                 step = HETID_CONSTANTS$DEFAULT_STEP,
                                 paired = FALSE) {
  validate_step(step)
  # Lower bound 0 (not MIN_MATURITY): admits the horizon-0 boundary below.
  assert_scalar_integer_in_range(
    i, "Maturity index i", 0L, effective_max_maturity(step),
    arg = "i"
  )
  validate_row_alignment(yields, term_premia)
  assert_flag(paired, "paired")

  if (i == 0) {
    # Reuse compute_n_hat_previous: it applies the TP^(1) := 0 normalization
    # that n_hat_series(0) would skip.
    warn_horizon_zero(
      paste0(
        "i = 0 returns the realized one-period price (observed at t), exact ",
        "-- not a forecast, no approximation or bias correction."
      )
    )
    n_hat_0 <- compute_n_hat_previous(yields, term_premia, step, step = step)
    return(prepare_return_data(exp(n_hat_0), dates, yields, "expected_sdf"))
  }

  if (paired) {
    # Gap series also drives compute_expected_sdf_variance_bound().
    components <- compute_expected_sdf_gap(yields, term_premia, i, step = step)
    assert_insufficient_data_ok(
      length(components$gap) > 0,
      "No valid observations to estimate the expected-SDF correction"
    )
    exp_n_hat <- components$exp_n_hat
    correction <- mean(components$gap)
  } else {
    # No lead, so any maturity i (not just multiples of step) is admissible.
    exp_n_hat <- exp(n_hat_series(yields, term_premia, i, step = step))
    y_step <- require_column(yields, acm_column_name("yields", step), "yields")
    m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
    realized_price <- exp(-m_step * y_step / HETID_CONSTANTS$PERCENT_TO_DECIMAL)
    common <- is.finite(realized_price) & is.finite(exp_n_hat)
    assert_insufficient_data_ok(
      any(common),
      "No valid observations to estimate the expected-SDF correction"
    )
    correction <- mean(realized_price[common]) - mean(exp_n_hat[common])
  }

  expected_sdf <- exp_n_hat + correction

  prepare_return_data(
    expected_sdf, dates, yields, "expected_sdf"
  )
}
