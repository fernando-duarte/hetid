#' Compute Supremum Estimator (c_hat)
#'
#' Computes c_hat_i which estimates sup_t exp(2*E_t\[p_(t+i)^(1)\])
#'
#' @param yields Data frame with columns y1, y2, ..., containing yields
#' @param term_premia Data frame with columns tp1, tp2, ..., containing term premia
#' @param i Integer, the horizon (must be >= 1)
#'
#' @return Numeric value of c_hat_i
#'
#' @details
#' The formula is:
#' c_hat_i = max over t of exp(2*n_hat(i,t))
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
  if (i < 1) {
    stop("i must be >= 1")
  }

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
