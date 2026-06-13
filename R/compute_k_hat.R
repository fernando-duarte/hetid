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
#' @return Numeric value of k_hat_i
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
#' @details
#' The fourth moment estimator captures the kurtosis of forecast errors in
#' the term structure model, providing information about tail risks.
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
  # Use standardized validation
  validate_step(step)
  validate_maturity_index(i)
  assert_bad_argument_ok(
    i >= step && i %% step == 0,
    paste0(
      "Maturity index i must be a positive multiple of step (", step,
      "): the realized-vs-forecast pairing shifts whole news periods"
    ),
    arg = "i"
  )
  validate_row_alignment(yields, term_premia)

  # Realized one-period yield: the step-maturity bond
  y_step <- require_column(
    yields, acm_column_name("yields", step), "yields"
  )

  n_hat_i_minus_1 <- compute_n_hat_previous(
    yields, term_premia, i,
    step = step
  )

  # Shifts count news periods (rows), not maturity units
  horizon_periods <- i %/% step

  # Number of observations
  n_obs <- length(y_step)

  assert_insufficient_data_ok(
    n_obs > horizon_periods,
    "Not enough observations. Need T > i/step news periods"
  )

  # Compute the fourth moment (vectorized)
  y_shifted <- y_step[(horizon_periods + 1):n_obs]
  n_hat_shifted <- n_hat_i_minus_1[2:(n_obs - horizon_periods + 1)]
  valid <- !is.na(y_shifted) & !is.na(n_hat_shifted)

  if (!any(valid)) {
    return(NA_real_)
  }

  # The realized log price scales the annualized yield by the
  # step-bond maturity in years
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  khat_terms <- (
    -m_step * y_shifted[valid] / HETID_CONSTANTS$PERCENT_TO_DECIMAL -
      n_hat_shifted[valid]
  )^4
  mean(khat_terms)
}
