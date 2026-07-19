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
#' @return Numeric value of k_hat_i, or \code{NA_real_} when no valid
#'   paired observations remain.
#'
#' @section Mathematical Formula:
#' With h = i/step news periods and m(step) the step maturity in years:
#' \deqn{k\_hat_i = \mathrm{mean}_t (-m(step) y_{t+h}^{(step)} - n\_hat(i-step,t+1))^4}
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
#'   only maturities \code{step} and \code{i} (via
#'   \code{compute_n_hat_previous(i - step)}), never \code{i + step}.
#'
#' @details
#' The fourth moment estimator summarizes the tail thickness of forecast errors
#' in the term structure model, providing information about tail risks.
#'
#' @export
#'
#' @examples
#' # Extract ACM data - need maturities step, i-step, and i (months)
#' # For i = 60 with the default annual step: 12, 48, and 60
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(12, 48, 60)
#' )
#' yields <- data[, paste0("y", c(12, 48, 60))]
#' term_premia <- data[, paste0("tp", c(12, 48, 60))]
#'
#' # Compute k_hat for the 5-year (60-month) maturity
#' k_hat_60 <- compute_k_hat(yields, term_premia, i = 60)
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
    # i > step validates units transitively via n_hat_series; the i == step
    # boundary (compute_n_hat_previous direct branch) is the one gap
    validate_percent_units(yields)
  }

  y_step <- require_acm_col(yields, "yields", step)

  n_hat_i_minus_1 <- compute_n_hat_previous(
    yields, term_premia, i,
    step = step
  )

  horizon_periods <- i %/% step # shift counts news periods (rows), not months
  n_obs <- length(y_step)

  assert_insufficient_data_ok(
    n_obs > horizon_periods,
    HETID_CONSTANTS$INSUFFICIENT_NEWS_MSG
  )

  # guard above keeps seq.int() ranges ascending (no 1:0 inversion)
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
