#' Compute Variance Bound
#'
#' Computes the empirical upper bound for Var(error(i,t+1))
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#'
#' @return Numeric value of the variance bound (1/4)*c_hat_i*k_hat_i
#'
#' @details
#' The variance bound is:
#' Var(error(i,t+1)) <= (1/4)*c_hat_i*k_hat_i
#'
#' @note For standard ACM data (10 maturities), the effective maximum for
#'   \code{i} is 9, because this function requires data at maturity \code{i+1}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#' yields <- data[, grep("^y", names(data))]
#' term_premia <- data[, grep("^tp", names(data))]
#'
#' # Compute variance bound for i=5
#' var_bound_5 <- compute_variance_bound(yields, term_premia, i = 5)
#' }
#'
compute_variance_bound <- function(yields, term_premia, i) {
  validate_maturity_index(i, max_maturity = HETID_CONSTANTS$MAX_MATURITY - 1)

  # Compute components
  c_hat <- compute_c_hat(yields, term_premia, i)
  k_hat <- compute_k_hat(yields, term_premia, i)

  # Return variance bound
  0.25 * c_hat * k_hat
}
