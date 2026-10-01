#' Compute Expected Log Bond Price Estimator (n_hat)
#'
#' Estimates the conditional expected log price of the step-maturity bond
#' at a horizon of \code{i} months from yields and term premia.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-dates-required
#' @template param-step
#'
#' @template return-dated-dataframe
#'
#' @details
#' Supply numeric yield and term-premium columns in annualized percentage
#' points, with rows already aligned by date. Nonempty data frames and numeric
#' matrices are accepted. Columns for maturities \code{i} and \code{i + step} are
#' required in each input, including when \code{i == step}.
#'
#' With maturity weights in years, \eqn{m(i) = i /
#' \mathrm{MATURITY\_UNITS\_PER\_YEAR}}, and the percentage-to-decimal
#' divisor \eqn{c = \mathrm{PERCENT\_TO\_DECIMAL}}, the formula is
#' \deqn{n\_hat(i,t) = [m(i) y_t^{(i)} - m(i+step) y_t^{(i+step)} +
#'   m(i+step) TP_t^{(i+step)} - m(i) TP_t^{(i)}] / c.}
#'
#' At the one-period maturity \code{i == step} the term premium of the
#' step-maturity bond is zero by definition (the imposed normalization
#' TP^(1):=0, since A^(step) = E_t\[y^(step)\]), so the m(i)*TP_t^(i)
#' term is dropped: any supplied value is overwritten by zero. This
#' matches the boundary leg of \code{compute_n_hat_previous()}.
#'
#' The returned columns are \code{date} and \code{n_hat}, with one row per
#' input observation in the original order. The numeric \code{n_hat} column
#' is in decimal log-price units. Missing values in the used columns propagate
#' without dropping rows, except that the overwritten one-period term premium
#' does not affect the result.
#' Dates must be non-missing; supplied dates are not normalized or sorted.
#' Zero-row numeric matrices return an empty result; zero-row data frames
#' are rejected by yield validation.
#'
#' The default \code{step} is annual. Allowed steps are positive integers no
#' larger than \code{HETID_CONSTANTS$MAX_MATURITY \%/\% 2L}. The maturity
#' \code{i} need not be a multiple of \code{step}. Invalid indices, missing
#' columns, invalid dates, and mismatched row counts raise structured
#' \code{hetid_error} conditions. Yields whose maximum absolute non-missing
#' value is below one trigger a \code{hetid_warning_unit_scale} warning;
#' the function does not rescale such inputs automatically.
#'
#' @note The effective maximum for \code{i} is
#'   \code{MAX_MATURITY - step}, because this function requires data at
#'   maturity \code{i + step}.
#'
#' @note The supplied \code{term_premia} are taken as the step-period
#'   expectations-component inputs (TP^(n) = y^(n) - A^(n)). If they follow
#'   a different rollover convention than the news step - e.g. the published
#'   ACM premia are monthly-rollover objects while the default step is
#'   annual - a convention wedge is inherited from the input. The
#'   construction assumes a consistent step-period term-premium input and
#'   does not reconcile rollover conventions.
#'
#' @export
#'
#' @examples
#' # Monthly data and step match the ACM rollover convention
#' mats <- c(60, 61)
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"), maturities = mats
#' )
#' n_hat_60 <- compute_n_hat(
#'   data[, paste0("y", mats)], data[, paste0("tp", mats)],
#'   i = 60, step = 1, dates = data$date
#' )
#' head(n_hat_60)
compute_n_hat <- function(yields, term_premia, i, dates = NULL,
                          step = HETID_CONSTANTS$DEFAULT_STEP) {
  prepare_return_data(
    n_hat_series(yields, term_premia, i, step = step),
    dates, yields, "n_hat"
  )
}

#' Bare n_hat(i, t) Series (Internal Numeric Kernel)
#'
#' The undated numeric core of \code{\link{compute_n_hat}}, used by the internal
#' computational chain (price news, SDF innovations, the variance-bound scalars)
#' which need the bare vector. Holds the input validation so every caller is
#' checked identically.
#'
#' @inheritParams compute_n_hat
#' @return Numeric vector \code{n_hat(i, t)}.
#' @noRd
n_hat_series <- function(yields, term_premia, i,
                         step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_news_kernel_inputs(yields, term_premia, i, step)
  validate_percent_units(yields)

  y_i <- require_acm_col(yields, "yields", i)
  y_next <- require_acm_col(yields, "yields", i + step)
  tp_i <- require_acm_col(term_premia, "term_premia", i)
  tp_next <- require_acm_col(term_premia, "term_premia", i + step)

  if (i == step) {
    tp_i <- 0
  }

  m_i <- i / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  m_next <- (i + step) / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR

  n_hat <- m_i * y_i - m_next * y_next + m_next * tp_next - m_i * tp_i
  n_hat / HETID_CONSTANTS$PERCENT_TO_DECIMAL
}
