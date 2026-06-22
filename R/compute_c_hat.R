#' Compute Supremum Estimator (c_hat) for Term Structure Analysis
#'
#' Computes c_hat_i which estimates sup_t exp(2*E_t\[p_(t+i)^(1)\]), the
#' deterministic envelope of the SDF-news variance-bound construction.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return Numeric value of c_hat_i
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
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step}
#'   (108 for standard ACM data with the default annual step), because this
#'   function requires data at maturity \code{i + step}.
#'
#' @export
#'
#' @examples
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#' yields <- data[, paste0("y", seq(12, 120, 12))]
#' term_premia <- data[, paste0("tp", seq(12, 120, 12))]
#'
#' # Compute c_hat for the 5-year (60-month) maturity
#' c_hat_60 <- compute_c_hat(yields, term_premia, i = 60)
#'
compute_c_hat <- function(yields, term_premia, i,
                          step = HETID_CONSTANTS$DEFAULT_STEP) {
  # Use standardized validation
  validate_step(step)
  validate_maturity_index(i, max_maturity = effective_max_maturity(step))
  validate_step_multiple(
    i, step,
    "the bound index set trims whole news periods"
  )
  validate_row_alignment(yields, term_premia)

  # Compute n_hat series (bare numeric kernel)
  n_hat <- n_hat_series(yields, term_premia, i, step = step)

  # Restrict to the bound index set T_i = {1, ..., T - i/step}: the
  # envelope C_i shares the dates of K1/K2, whose realized leg needs
  # i/step further news periods (spec's common T_i for the bound).
  n_hat_clean <- trim_to_bound_index_set(n_hat, i, step)

  if (length(n_hat_clean) == 0) {
    return(NA_real_)
  }

  # Compute maximum of exp(2*n_hat) over T_i
  max(exp(2 * n_hat_clean))
}
