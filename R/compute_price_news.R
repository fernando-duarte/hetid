#' Compute Price News
#'
#' Computes the time series of price news Delta_(t+1)p_(t+i)^(1) or Delta_(t+1)y_(t+i)^(1)
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @param return_yield_news Logical, if TRUE returns yield news instead of log price news
#' @template param-return-df-dates
#'
#' @template return-numeric-or-dataframe
#'
#' @details
#' The price news for log prices is:
#' Delta_(t+1)p_(t+i)^(1) = n_hat(i-1,t+1) - n_hat(i,t)
#'
#' The price news for yields is:
#' Delta_(t+1)y_(t+i)^(1) = -Delta_(t+1)p_(t+i)^(1)
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
#' # Compute price news for i=5 (log price news)
#' price_news_5 <- compute_price_news(yields, term_premia, i = 5)
#'
#' # Compute yield news with dates
#' yield_news_5_df <- compute_price_news(
#'   yields, term_premia,
#'   i = 5,
#'   return_yield_news = TRUE,
#'   return_df = TRUE,
#'   dates = data$date
#' )
#' }
#'
compute_price_news <- function(yields, term_premia, i,
                               return_yield_news = FALSE,
                               return_df = FALSE, dates = NULL) {
  # Validate maturity parameter
  validate_maturity_param(i)

  # Compute n_hat series
  n_hat_i <- compute_n_hat_validated(yields, term_premia, i,
    return_df = FALSE, dates = dates
  )

  # Compute previous period n_hat (handles special case for i=1)
  n_hat_i_minus_1 <- compute_n_hat_previous(yields, term_premia, i, dates)

  # Compute price news using utility function
  price_news <- compute_time_series_news(n_hat_i, n_hat_i_minus_1, negate = return_yield_news)

  # Return data frame with dates if requested using utility function
  prepare_return_data(price_news, return_df, dates, yields, "price_news")
}
