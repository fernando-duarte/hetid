#' Compute Fourth Moment Estimator (k_hat) for Term Structure Analysis
#'
#' Computes k_hat_i which estimates E\[(p_(t+i)^(1) - E_(t+1)\[p_(t+i)^(1)\])^4\],
#' the realized-forecast-error fourth moment (K1) of the variance-bound
#' construction.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return A numeric scalar giving the fourth moment of log-price forecast
#'   errors, or \code{NA_real_} when no non-missing paired observations remain.
#'
#' @section Mathematical Formula:
#' With h = i/step news periods and m(step) the step maturity in years:
#' \deqn{k\_hat_i = \mathrm{mean}_t
#'   \left[\left(-m(step) y_{t+h}^{(step)} / 100 -
#'   n\_hat(i-step,t+1)\right)^4\right]}
#'
#' The mean is taken over the valid (non-missing) terms for
#' \eqn{t = 1, \dots, T-h}; with complete data the divisor is \eqn{T-h}.
#'
#' @section Time units:
#' The realized-vs-forecast pairing shifts \code{i/step} rows. Rows are
#' whatever observation frequency the caller supplies; the shift counts
#' news periods, not calendar time, so row frequency must equal the
#' intended news period. \code{i} must be a positive multiple of
#' \code{step}.
#'
#' @note Unlike \code{compute_c_hat}, \code{compute_k2_hat}, and
#'   \code{compute_variance_bound} (capped at \code{MAX_MATURITY - step}),
#'   \code{i} here may run up to \code{MAX_MATURITY}: this estimator reads
#'   maturities \code{step}, \code{i - step}, and \code{i} when
#'   \code{i > step}. At \code{i == step}, only the step-maturity yield
#'   is needed; the result is zero when paired observations remain and
#'   all paired values are finite.
#'   For \code{i > step}, \code{compute_n_hat_previous()} evaluates
#'   \code{n_hat} at \code{i - step}, never \code{i + step}.
#'
#' @details
#' The fourth moment estimator summarizes the tail thickness of forecast errors
#' in the term structure model, providing information about tail risks.
#'
#' Supply numeric data frames or matrices with the same number of rows,
#' already aligned by date and ordered in time. Values must be annualized
#' percentage points. The calculation uses row positions and does not inspect
#' dates. A \code{hetid_warning_unit_scale} warning flags yields whose maximum
#' absolute non-missing value is below one; it does not rescale the inputs.
#'
#' \code{step} must be an integer from one through
#' \code{HETID_CONSTANTS$MAX_MATURITY \%/\% 2}; its default is the annual
#' news period \code{HETID_CONSTANTS$DEFAULT_STEP}. Here, \code{i} is a
#' positive multiple of \code{step} no larger than
#' \code{HETID_CONSTANTS$MAX_MATURITY}, including the upper boundary when
#' it is a multiple of \code{step}.
#'
#' For \code{i > step}, yields at \code{step}, \code{i - step}, and
#' \code{i}, and term premia at \code{i - step} and \code{i}, are required.
#' The step-maturity term premium is normalized to zero when it enters the
#' forecast. At \code{i == step}, term premia are unused but must still
#' have the same row count. The term-premium rollover convention must match
#' the news period; the calculation does not reconcile different conventions.
#'
#' Pairs with a missing yield or forecast (including \code{NaN}) are omitted.
#' Infinite inputs are not removed and can produce non-finite results.
#' There must be more than \code{i/step} rows, even when all values are
#' missing; otherwise a \code{hetid_error_insufficient_data} is raised.
#' Invalid scalar arguments or missing required columns raise
#' \code{hetid_error_bad_argument}; unequal row counts raise
#' \code{hetid_error_dimension_mismatch}.
#'
#' @seealso \code{\link{compute_n_hat}}, \code{\link{compute_variance_bound}}
#'
#' @export
#'
#' @examples
#' # Monthly rows and a monthly news step use the same time unit
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(1, 11, 12),
#'   start_date = "2000-01-01",
#'   end_date = "2020-12-31"
#' )
#' yields <- data[, paste0("y", c(1, 11, 12))]
#' term_premia <- data[, paste0("tp", c(1, 11, 12))]
#'
#' compute_k_hat(yields, term_premia, i = 12, step = 1)
#' compute_k_hat(yields, term_premia, i = 1, step = 1)
#'
compute_k_hat <- function(yields, term_premia, i,
                          step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_news_kernel_inputs(
    yields, term_premia, i, step,
    step_multiple_reason =
      "the realized-vs-forecast pairing shifts whole news periods",
    max_index = FALSE
  )
  if (i == step) {
    # The i == step branch of compute_n_hat_previous skips n_hat_series's unit check
    validate_percent_units(yields)
  }

  y_step <- require_acm_col(yields, "yields", step)

  n_hat_i_minus_1 <- compute_n_hat_previous(
    yields, term_premia, i,
    step = step
  )

  horizon_periods <- i %/% step
  n_obs <- length(y_step)

  assert_insufficient_data_ok(
    n_obs > horizon_periods,
    HETID_CONSTANTS$INSUFFICIENT_NEWS_MSG
  )

  # The row-count guard prevents seq.int() from creating descending index ranges
  y_shifted <- y_step[seq.int(horizon_periods + 1, n_obs)]
  n_hat_shifted <- n_hat_i_minus_1[seq.int(2, n_obs - horizon_periods + 1)]
  valid <- !is.na(y_shifted) & !is.na(n_hat_shifted)

  if (!any(valid)) {
    return(NA_real_)
  }

  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  khat_terms <- (
    -m_step * y_shifted[valid] / HETID_CONSTANTS$PERCENT_TO_DECIMAL -
      n_hat_shifted[valid]
  )^4
  mean(khat_terms)
}
