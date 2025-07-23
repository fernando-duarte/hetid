#' Compute Variance Bound
#'
#' Computes the empirical upper bound for Var(error(i,t+1))
#'
#' @param yields Data frame with columns y1, y2, ..., containing yields
#' @param term_premia Data frame with columns tp1, tp2, ..., containing term premia
#' @param i Integer, the horizon (must be >= 1)
#'
#' @return Numeric value of the variance bound (1/4)*c_hat_i*k_hat_i
#'
#' @details
#' The variance bound is:
#' Var(error(i,t+1)) <= (1/4)*c_hat_i*k_hat_i
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#'
#' # Compute variance bound for i=5
#' var_bound_5 <- compute_variance_bound(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5
#' )
#' }
#'
compute_variance_bound <- function(yields, term_premia, i) {
  if (i < 1) {
    stop("i must be >= 1")
  }

  # Compute components
  c_hat <- compute_c_hat(yields, term_premia, i)
  k_hat <- compute_k_hat(yields, term_premia, i)

  # Return variance bound
  0.25 * c_hat * k_hat
}
