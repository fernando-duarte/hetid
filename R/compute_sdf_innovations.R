#' Compute SDF Innovations Time Series
#'
#' Computes the time series of SDF innovations given by:
#' exp(n_hat(i,t)) * (Delta_(t+1)p_(t+i)^(1) + 0.5*(Delta_(t+1)p_(t+i)^(1))^2 -
#' 0.5*E\\[(Delta_(t+1)p_(t+i)^(1))^2\\])
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-return-df-dates
#'
#' @template return-numeric-or-dataframe
#'
#' @details
#' The SDF innovation is computed as:
#' exp(n_hat(i,t)) * (Delta_(t+1)p_(t+i)^(1) + 0.5*(Delta_(t+1)p_(t+i)^(1))^2 -
#' 0.5*E\\[(Delta_(t+1)p_(t+i)^(1))^2\\])
#'
#' Where:
#' - Delta_(t+1)p_(t+i)^(1) = n_hat(i-1,t+1) - n_hat(i,t)
#' - E\\[(Delta_(t+1)p_(t+i)^(1))^2\\] = (1/(T-1)) * sum(n_hat(i-1,t+1) - n_hat(i,t))^2
#'   for t=1 to T-1
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
#' # Compute SDF innovations for i=5
#' sdf_innovations_5 <- compute_sdf_innovations(yields, term_premia, i = 5)
#'
#' # Compute SDF innovations with dates
#' sdf_innovations_5_df <- compute_sdf_innovations(
#'   yields, term_premia,
#'   i = 5,
#'   return_df = TRUE,
#'   dates = data$date
#' )
#' }
#'
compute_sdf_innovations <- function(yields, term_premia, i,
                                    return_df = FALSE, dates = NULL) {
  # Validate maturity parameter
  validate_maturity_param(i)

  # Compute n_hat(i,t) series
  n_hat_i <- compute_n_hat_validated(yields, term_premia, i,
    return_df = FALSE, dates = dates
  )

  # Compute price news (Delta_(t+1)p_(t+i)^(1))
  delta_p <- compute_price_news(yields, term_premia, i,
    return_yield_news = FALSE,
    return_df = FALSE, dates = dates
  )

  # Compute E[(Delta_(t+1)p_(t+i)^(1))^2] using utility function
  expected_delta_p_squared <- compute_expected_squared(
    delta_p,
    "No valid SDF news values to compute expectation"
  )

  # Define SDF transformation function
  sdf_transform <- function(n_hat_val, delta_p_val, expected_sq) {
    exp(n_hat_val) * (delta_p_val + 0.5 * delta_p_val^2 - 0.5 * expected_sq)
  }

  # Compute SDF innovations using utility function
  sdf_innovations <- apply_time_series_transform(
    n_hat_i[seq_along(delta_p)], delta_p, sdf_transform, expected_delta_p_squared
  )

  # Return data frame with dates if requested using utility function
  prepare_return_data(sdf_innovations, return_df, dates, yields, "sdf_innovations")
}
