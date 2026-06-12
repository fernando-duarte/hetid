#' Compute Supremum Estimator (c_hat) for Term Structure Analysis
#'
#' Computes c_hat_i which estimates sup_t exp(2*E_t\[p_(t+i)^(1)\]) following
#' the methodology in Adrian, Crump, and Moench (2013).
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return Numeric value of c_hat_i
#'
#' @section Mathematical Formula:
#' \deqn{c\_hat_i = \max_t \exp(2 \cdot n\_hat(i,t))}
#'
#' @template section-acm-methodology
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
  validate_row_alignment(yields, term_premia)

  # Compute n_hat series
  n_hat <- compute_n_hat(yields, term_premia, i, step = step)

  # Remove NA values
  n_hat_clean <- n_hat[!is.na(n_hat)]

  if (length(n_hat_clean) == 0) {
    return(NA_real_)
  }

  # Compute maximum of exp(2*n_hat)
  max(exp(2 * n_hat_clean))
}
