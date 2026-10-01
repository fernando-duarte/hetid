#' Compute Supremum Estimator (c_hat) for Term Structure Analysis
#'
#' Computes c_hat_i which estimates sup_t exp(2*E_t\[p_(t+i)^(1)\]), the
#' deterministic envelope of the SDF-news variance-bound construction.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return An unnamed numeric scalar estimating c_hat_i, or \code{NA_real_}
#'   when every n_hat value in the bound index set is missing.
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
#' The estimator is the sample maximum of the exponential of twice the
#' estimated expected log price, restricted to the bound index set.
#'
#' Supply numeric yields and term premia in annualized percentage points,
#' with columns for maturities \code{i} and \code{i + step}. Matrices with
#' these column names are also accepted. The inputs must have equal row
#' counts and be aligned by date before calling this function; dates are
#' not checked here. Each row must represent one news period of \code{step}
#' months. The default step is annual; monthly observations use \code{step = 1}.
#' The step must be a positive integer no greater than
#' \code{HETID_CONSTANTS$MAX_MATURITY \%/\% 2}.
#'
#' Missing n_hat values (including \code{NaN}) are omitted after trimming.
#' Infinite values are retained, so exponentiation can return zero or
#' \code{Inf}. Fewer than or exactly \code{i/step} rows raise
#' \code{hetid_error_insufficient_data}, rather than returning a missing value.
#' Invalid maturities, steps, or missing required columns raise
#' \code{hetid_error_bad_argument}; unequal row counts raise
#' \code{hetid_error_dimension_mismatch}. Yields whose maximum absolute
#' value is below one trigger \code{hetid_warning_unit_scale} because they
#' may be in decimal units.
#'
#' At \code{i == step}, the supplied term premium at maturity \code{i}
#' is replaced by zero in the calculation. The step-period term-premium
#' convention and rollover caveat of \code{\link{compute_n_hat}} also apply.
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step},
#'   because this function requires data at maturity \code{i + step}.
#'
#' @export
#'
#' @examples
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(60, 61),
#'   frequency = "monthly"
#' )
#' yields <- data[, paste0("y", c(60, 61))]
#' term_premia <- data[, paste0("tp", c(60, 61))]
#'
#' c_hat_60 <- compute_c_hat(yields, term_premia, i = 60, step = 1)
#' c_hat_60
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
