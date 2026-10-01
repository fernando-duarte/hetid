#' Compute Price News
#'
#' Computes the time series of price news \eqn{\Delta_{t+1}p^{(1)}_{t+i}} or
#' \eqn{\Delta_{t+1}y^{(1)}_{t+i}}.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @param return_yield_news A single non-missing logical value. If \code{TRUE},
#'   returns yield news instead of log price news. Defaults to \code{FALSE}.
#' @template param-dates-required
#' @template param-step
#'
#' @template return-dated-dataframe
#'
#' @details
#' Supply numeric yields and term premia in annualized percentage points,
#' with the same number of rows and already aligned by date. Required columns
#' have month-suffixed names: \code{y<i>} and \code{tp<i>} at maturities
#' \code{i - step}, \code{i}, and \code{i + step}. At \code{i == step},
#' only maturities \code{step} and \code{2 * step} are required.
#' The step must be a positive integer no larger than
#' \code{HETID_CONSTANTS$MAX_MATURITY \%/\% 2}. The horizon must equal
#' \code{step} or satisfy \code{i - step >= HETID_CONSTANTS$MIN_MATURITY};
#' it need not be a multiple of \code{step}.
#'
#' The news operator differences adjacent rows, without resampling or checking
#' their calendar spacing. Supply one observation per \code{step}-month period.
#' Dates must be non-missing; they are preserved without normalization.
#'
#' The price news for log prices is
#' \deqn{\Delta_{t+1} p_{t+i}^{(1)} = n\_hat(i-step,t+1) - n\_hat(i,t)}
#'
#' The price news for yields is
#' \deqn{\Delta_{t+1} y_{t+i}^{(1)} = -\Delta_{t+1} p_{t+i}^{(1)}}
#'
#' The result has \code{nrow(yields)} rows and columns \code{date} and
#' \code{price_news}, including when \code{return_yield_news = TRUE}.
#' Values are in decimal log-price or yield units. Each news value is dated
#' at its realization, \code{dates[t + 1]}, rather than its conditioning date.
#' Missing input values propagate through the arithmetic; rows are not dropped.
#' One input row returns a single missing news value; zero rows are rejected.
#' Invalid flags, dates, maturities, steps, or missing
#' required columns raise structured \code{hetid_error} conditions. Yields
#' whose maximum absolute value is below one trigger a unit-scale warning.
#'
#' At \code{i == step}, the previous-period leg is the realized log price of
#' the step-maturity bond, and its term premium is normalized to zero in the
#' current-period leg. Inputs must use a consistent step-period term-premium
#' convention; rollover differences in supplied ACM premia are not reconciled.
#' See \code{\link{compute_n_hat}} for the level estimator and this caveat.
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step},
#'   because this function requires data at maturity \code{i + step}.
#'
#' @note The returned series is a news series, so its first row value is
#'   always \code{NA_real_}: there are T rows but only T-1 news observations.
#'
#' @export
#'
#' @examples
#' # Monthly data and step match the ACM rollover convention
#' mats <- c(59, 60, 61)
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"), maturities = mats
#' )
#' yields <- data[, paste0("y", mats)]
#' term_premia <- data[, paste0("tp", mats)]
#' price_news_60 <- compute_price_news(
#'   yields, term_premia,
#'   i = 60, step = 1, dates = data$date
#' )
#' head(price_news_60)
#' yield_news_60 <- compute_price_news(
#'   yields, term_premia,
#'   i = 60, step = 1,
#'   return_yield_news = TRUE, dates = data$date
#' )
#' head(yield_news_60)
compute_price_news <- function(yields, term_premia, i,
                               return_yield_news = FALSE, dates = NULL,
                               step = HETID_CONSTANTS$DEFAULT_STEP) {
  assert_flag(return_yield_news, "return_yield_news")
  validate_row_alignment(yields, term_premia)

  components <- compute_news_components(yields, term_premia, i, step = step)
  price_news <- if (return_yield_news) -components$delta_p else components$delta_p

  prepare_return_data(
    price_news, dates, yields, "price_news",
    is_news = TRUE
  )
}
