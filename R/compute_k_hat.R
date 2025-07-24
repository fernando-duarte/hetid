#' Compute Fourth Moment Estimator (k_hat) for Term Structure Analysis
#'
#' Computes k_hat_i which estimates E\[(p_(t+i)^(1) - E_(t+1)\[p_(t+i)^(1)\])^4\]
#' following the methodology in Adrian, Crump, and Moench (2013).
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#'
#' @return Numeric value of k_hat_i
#'
#' @section Mathematical Formula:
#' \deqn{k\_hat_i = \frac{1}{T-i} \sum_{t=1}^{T-i} (-y_{t+i}^{(1)} - n\_hat(i-1,t+1))^4}
#'
#' @template section-acm-methodology
#'
#' @details
#' The fourth moment estimator captures the kurtosis of forecast errors in
#' the term structure model, providing information about tail risks.
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
#' # Compute k_hat for i=5
#' k_hat_5 <- compute_k_hat(yields, term_premia, i = 5)
#' }
#'
compute_k_hat <- function(yields, term_premia, i) {
  # Use standardized validation
  validate_maturity_index(i)

  # Get y1 series
  y1 <- yields[["y1"]]
  if (is.null(y1)) {
    stop("y1 column not found in yields")
  }

  # Special case for i=1: n_hat_0 = -y1
  if (i == 1) {
    # For i=1, n_hat(0,t) = E_t\[p_(t+0)^(1)\] = p_t^(1) = -y_t^(1)
    n_hat_i_minus_1 <- -y1 / HETID_CONSTANTS$PERCENT_TO_DECIMAL # Convert to decimal
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
      term <- (-y1[t + i] / HETID_CONSTANTS$PERCENT_TO_DECIMAL - n_hat_i_minus_1[t + 1])^4
      sum_val <- sum_val + term
      count <- count + 1
    }
  }

  if (count == 0) {
    return(NA)
  }

  sum_val / count
}
