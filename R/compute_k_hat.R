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
#' # Extract ACM data - need maturities 1, i-1, i, i+1 for maturity i
#' # For i=5, we need maturities 1, 4, 5, 6
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(1, 4, 5, 6)
#' )
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
    stop("y1 column not found in yields", call. = FALSE)
  }

  n_hat_i_minus_1 <- compute_n_hat_previous(
    yields, term_premia, i
  )

  # Number of observations
  n_obs <- length(y1)

  if (n_obs <= i) {
    stop("Not enough observations. Need T > i", call. = FALSE)
  }

  # Compute the fourth moment (vectorized)
  y1_shifted <- y1[(i + 1):n_obs]
  n_hat_shifted <- n_hat_i_minus_1[2:(n_obs - i + 1)]
  valid <- !is.na(y1_shifted) & !is.na(n_hat_shifted)

  if (!any(valid)) {
    return(NA)
  }

  khat_terms <- (
    -y1_shifted[valid] / HETID_CONSTANTS$PERCENT_TO_DECIMAL -
      n_hat_shifted[valid]
  )^4
  mean(khat_terms)
}
