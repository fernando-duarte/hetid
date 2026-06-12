#' Compute SDF Innovations Time Series
#'
#' Computes the time series of SDF innovations given by:
#' exp(n_hat(i,t)) * (Delta_(t+1)p_(t+i)^(1) + 0.5*(Delta_(t+1)p_(t+i)^(1))^2 -
#' 0.5*E\\[(Delta_(t+1)p_(t+i)^(1))^2\\])
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-return-df-dates
#' @template param-step
#'
#' @template return-numeric-or-dataframe
#'
#' @details
#' The SDF innovation is computed as:
#' exp(n_hat(i,t)) * (Delta_(t+1)p_(t+i)^(1) + 0.5*(Delta_(t+1)p_(t+i)^(1))^2 -
#' 0.5*E\\[(Delta_(t+1)p_(t+i)^(1))^2\\])
#'
#' Where:
#' - Delta_(t+1)p_(t+i)^(1) = n_hat(i-step,t+1) - n_hat(i,t)
#' - E\\[(Delta_(t+1)p_(t+i)^(1))^2\\] is the mean of
#'   (n_hat(i-step,t+1) - n_hat(i,t))^2 over the valid (non-missing) news
#'   terms; with complete data this averages the T-1 available terms
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step}
#'   (108 for standard ACM data with the default annual step), because this
#'   function requires data at maturity \code{i + step}.
#'
#' @export
#'
#' @examples
#' # Extract ACM data - need maturities i-step, i, i+step (months)
#' # For i = 60 with the default annual step: 48, 60, 72
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(48, 60, 72)
#' )
#' yields <- data[, paste0("y", c(48, 60, 72))]
#' term_premia <- data[, paste0("tp", c(48, 60, 72))]
#'
#' # Compute SDF innovations for i = 60
#' sdf_innovations_60 <- compute_sdf_innovations(yields, term_premia, i = 60)
#'
#' # Compute SDF innovations with dates
#' sdf_innovations_60_df <- compute_sdf_innovations(
#'   yields, term_premia,
#'   i = 60,
#'   return_df = TRUE,
#'   dates = data$date
#' )
#'
compute_sdf_innovations <- function(yields, term_premia, i,
                                    return_df = FALSE, dates = NULL,
                                    step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_row_alignment(yields, term_premia)

  # Shared maturity validation, n_hat(i,t) level, and price-news
  # difference. The t-alignment between exp(n_hat_i[t]) and delta_p[t]
  # holds: delta_p[t] = n_hat(i-step,t+1) - n_hat(i,t) pairs with n_hat_i[t].
  components <- compute_news_components(yields, term_premia, i, step = step)
  n_hat_i <- components$n_hat_i
  delta_p <- components$delta_p

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
