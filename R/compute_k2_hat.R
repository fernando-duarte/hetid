#' Compute Fourth-Moment News Estimator (k2_hat)
#'
#' Computes k2_hat_i, the fourth moment of the price-news term that, with
#' \code{\link{compute_k_hat}} (k1_hat), forms the variance-bound leading
#' term U_i = (1/4) * C_i * (k1_hat_i + k2_hat_i).
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return Numeric value of k2_hat_i
#'
#' @section Mathematical Formula:
#' \deqn{k2\_hat_i = \mathrm{mean}_t (\Delta_{t+1} p_{t+i}^{(1)})^4}
#' over the bound index set \eqn{t = 1, \dots, T - i/step}, the same
#' dates as \code{\link{compute_k_hat}} and \code{\link{compute_c_hat}}.
#' \code{i} must be a positive multiple of \code{step}.
#'
#' @details
#' k2_hat captures the contribution of the nonzero conditional mean of the
#' centered approximation error to the variance bound. Unlike k1_hat it is
#' nonzero at the one-period maturity \code{i == step}, where the realized
#' forecast error vanishes but the price news does not.
#'
#' @seealso \code{\link{compute_k_hat}} for the companion k1 term and
#'   \code{\link{compute_variance_bound}} for the assembled bound.
#'
#' @export
#'
#' @examples
#' # Extract ACM data - need maturities i-step, i, i+step (months)
#' # For i = 60 with the default annual step: 48, 60, 72
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(48, 60, 72)
#' )
#' yields <- data[, paste0("y", c(48, 60, 72))]
#' term_premia <- data[, paste0("tp", c(48, 60, 72))]
#'
#' # Compute k2_hat for the 5-year (60-month) maturity
#' k2_hat_60 <- compute_k2_hat(yields, term_premia, i = 60)
#'
compute_k2_hat <- function(yields, term_premia, i,
                           step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  validate_maturity_index(i, max_maturity = effective_max_maturity(step))
  validate_step_multiple(
    i, step,
    "the bound index set trims whole news periods"
  )
  validate_row_alignment(yields, term_premia)

  # Price news Delta_(t+1) p_(t+i)^(1), positionally indexed by t (the bare
  # T-1 kernel shared with compute_price_news)
  delta_p <- compute_news_components(yields, term_premia, i, step = step)$delta_p

  # Trim to the bound index set T_i = {1, ..., T - i/step}
  horizon_periods <- i %/% step
  assert_insufficient_data_ok(
    length(delta_p) + 1L > horizon_periods,
    "Not enough observations. Need T > i/step news periods"
  )
  keep <- delta_p[seq_len(length(delta_p) - horizon_periods + 1L)]
  keep <- keep[!is.na(keep)]

  if (length(keep) == 0) {
    return(NA_real_)
  }

  mean(keep^4)
}
