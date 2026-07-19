#' Compute Supremum Estimator (c_hat) for Term Structure Analysis
#'
#' Computes c_hat_i which estimates sup_t exp(2*E_t\[p_(t+i)^(1)\]), the
#' deterministic envelope of the SDF-news variance-bound construction.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return Numeric value of c_hat_i, or \code{NA_real_} when no valid
#'   paired observations remain.
#'
#' @section Mathematical Formula:
#' \deqn{c\_hat_i = \max_{t \in T_i} \exp(2 \cdot n\_hat(i,t))}
#' over the bound index set \eqn{T_i = \{1, \dots, T - i/step\}}, the
#' same dates as \code{\link{compute_k_hat}} and
#' \code{\link{compute_k2_hat}} (the realized leg needs \code{i/step}
#' further news periods). \code{i} must be a positive multiple of
#' \code{step}.
#'
#' @details
#' The supremum estimator provides an upper bound for the exponential of twice
#' the expected log price at horizon i.
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step},
#'   because this function requires data at maturity \code{i + step}.
#'
#' @export
#'
#' @examples
#' # Extract ACM data - need maturities i and i+step (months)
#' # For i = 60 with the default annual step: 60 and 72
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(60, 72)
#' )
#' yields <- data[, paste0("y", c(60, 72))]
#' term_premia <- data[, paste0("tp", c(60, 72))]
#'
#' # Compute c_hat for the 5-year (60-month) maturity
#' c_hat_60 <- compute_c_hat(yields, term_premia, i = 60)
#'
compute_c_hat <- function(yields, term_premia, i,
                          step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_news_kernel_inputs(
    yields, term_premia, i, step,
    step_multiple_reason = HETID_CONSTANTS$BOUND_INDEX_TRIM_MSG
  )

  n_hat <- n_hat_series(yields, term_premia, i, step = step)
  n_hat_clean <- trim_to_bound_index_set(n_hat, i, step)

  if (length(n_hat_clean) == 0) {
    return(NA_real_)
  }

  max(exp(2 * n_hat_clean))
}
