#' Compute Fourth Moment Estimator (k_hat)
#'
#' Computes k_hat_i which estimates E\[(p_(t+i)^(1) - E_(t+1)\[p_(t+i)^(1)\])^4\]
#'
#' @param yields Data frame with columns y1, y2, ..., containing yields
#' @param term_premia Data frame with columns tp1, tp2, ..., containing term premia
#' @param i Integer, the horizon (must be >= 1)
#'
#' @return Numeric value of k_hat_i
#'
#' @details
#' The formula is:
#' k_hat_i = (1/(T-i)) * sum_t=1^(T-i) (-y_(t+i)^(1) - n_hat(i-1,t+1))^4
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract ACM data
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#'
#' # Compute k_hat for i=5
#' k_hat_5 <- compute_k_hat(
#'   yields = data[, grep("^y", names(data))],
#'   term_premia = data[, grep("^tp", names(data))],
#'   i = 5
#' )
#' }
#'
compute_k_hat <- function(yields, term_premia, i) {
  if (i < 1) {
    stop("i must be >= 1")
  }

  # Get y1 series
  y1 <- yields[["y1"]]
  if (is.null(y1)) {
    stop("y1 column not found in yields")
  }

  # Special case for i=1: n_hat_0 = -y1
  if (i == 1) {
    # For i=1, n_hat(0,t) = E_t\[p_(t+0)^(1)\] = p_t^(1) = -y_t^(1)
    n_hat_i_minus_1 <- -y1 / 100 # Convert to decimal
  } else {
    # Compute n_hat(i-1) series
    n_hat_i_minus_1 <- compute_n_hat(yields, term_premia, i - 1)
  }

  # Number of observations
  n_obs <- length(y1)

  if (n_obs <= i) {
    stop("Not enough observations. Need T > i")
  }

  # Compute the fourth moment
  # Need to align time indices properly
  sum_val <- 0
  count <- 0

  for (t in 1:(n_obs - i)) {
    if (!is.na(y1[t + i]) && !is.na(n_hat_i_minus_1[t + 1])) {
      # Convert y1 from percentage to decimal
      term <- (-y1[t + i] / 100 - n_hat_i_minus_1[t + 1])^4
      sum_val <- sum_val + term
      count <- count + 1
    }
  }

  if (count == 0) {
    return(NA)
  }

  sum_val / count
}
