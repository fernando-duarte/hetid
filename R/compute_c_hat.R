#' Compute Supremum Estimator (c_hat) for Term Structure Analysis
#'
#' Computes c_hat_i which estimates sup_t exp(2*E_t\[p_(t+i)^(1)\]) following
#' the methodology in Adrian, Crump, and Moench (2013).
#'
#' @template acm-data
#' @template param-maturity-index
#'
#' @return Numeric value of c_hat_i
#'
#' @section Mathematical Formula:
#' \deqn{c\_hat_i = \max_t \exp(2 \cdot n\_hat(i,t))}
#'
#' @section Literature Reference:
#' This implements the supremum estimator from Adrian, Crump, and Moench (2013)
#' for term structure analysis.
#'
#' @details
#' The supremum estimator provides an upper bound for the exponential of twice
#' the expected log price at horizon i.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#'
#' # Compute c_hat for i=5
#' c_hat_5 <- compute_c_hat(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5
#' )
#' }
#'
compute_c_hat <- function(yields, term_premia, i) {
  # Use standardized validation
  validate_maturity_index(i)

  # Compute n_hat series
  n_hat <- compute_n_hat(yields, term_premia, i)

  # Remove NA values
  n_hat_clean <- n_hat[!is.na(n_hat)]

  if (length(n_hat_clean) == 0) {
    return(NA)
  }

  # Compute maximum of exp(2*n_hat)
  max(exp(2 * n_hat_clean))
}
