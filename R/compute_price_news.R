#' Compute Price News
#'
#' Computes the time series of price news Delta_(t+1)p_(t+i)^(1) or Delta_(t+1)y_(t+i)^(1)
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @param return_yield_news Logical, if TRUE returns yield news instead of log price news
#' @template param-dates-required
#' @template param-step
#'
#' @template return-dated-dataframe
#'
#' @details
#' The price news for log prices is
#' \deqn{\Delta_{t+1} p_{t+i}^{(1)} = n\_hat(i-step,t+1) - n\_hat(i,t)}
#'
#' The price news for yields is
#' \deqn{\Delta_{t+1} y_{t+i}^{(1)} = -\Delta_{t+1} p_{t+i}^{(1)}}
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step},
#'   because this function requires data at maturity \code{i + step}.
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
#' # Compute price news for i = 60 (log price news, dated data frame)
#' price_news_60 <- compute_price_news(
#'   yields, term_premia,
#'   i = 60,
#'   dates = data$date
#' )
#'
#' # Compute yield news with dates
#' yield_news_60 <- compute_price_news(
#'   yields, term_premia,
#'   i = 60,
#'   return_yield_news = TRUE,
#'   dates = data$date
#' )
#'
compute_price_news <- function(yields, term_premia, i,
                               return_yield_news = FALSE, dates = NULL,
                               step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_row_alignment(yields, term_premia)

  components <- compute_news_components(yields, term_premia, i, step = step)
  price_news <- if (return_yield_news) -components$delta_p else components$delta_p

  prepare_return_data(
    price_news, dates, yields, "price_news",
    is_news = TRUE
  )
}
