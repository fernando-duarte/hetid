#' Compute Shared Price-News Components
#'
#' Validates the maturity index and computes the n_hat level series and
#' the price-news difference shared by \code{compute_price_news} and
#' \code{compute_sdf_innovations}, so the delta_p definition lives in a
#' single place.
#'
#' @param yields Yields data
#' @param term_premia Term premia data
#' @param i Maturity index
#'
#' @return A list with two elements: \code{n_hat_i}, the n_hat(i, t)
#'   level series, and \code{delta_p}, the price news (T-1 elements)
#'   \code{delta_p[t] = n_hat(i - 1, t + 1) - n_hat(i, t)}.
#' @keywords internal
compute_news_components <- function(yields, term_premia, i) {
  validate_maturity_index(i, max_maturity = HETID_CONSTANTS$EFFECTIVE_MAX_MATURITY)
  n_hat_i <- compute_n_hat(yields, term_premia, i, return_df = FALSE)
  n_hat_i_minus_1 <- compute_n_hat_previous(yields, term_premia, i)
  delta_p <- compute_time_series_news(n_hat_i, n_hat_i_minus_1)
  list(n_hat_i = n_hat_i, delta_p = delta_p)
}
