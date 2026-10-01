#' Compute Shared Price-News Components
#'
#' Validates the maturity index and computes the n_hat level series and
#' the price-news difference shared by \code{compute_price_news} and
#' \code{compute_sdf_innovations}, so the delta_p definition lives in a
#' single place.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @details
#' The inputs must be numeric matrices or data frames with the same number
#' of rows, aligned in time before calling this helper. Yields and term
#' premia are in annualized percentage points. Required maturity columns
#' are \code{i} and \code{i + step}, plus \code{i - step} when
#' \code{i > step}. The integer horizon must satisfy \code{i == step} or
#' \code{i - step >= HETID_CONSTANTS$MIN_MATURITY}, and
#' \code{i <= HETID_CONSTANTS$MAX_MATURITY - step}. The positive integer
#' \code{step} cannot exceed \code{HETID_CONSTANTS$MAX_MATURITY %/% 2}.
#'
#' The step-maturity term premium is normalized to zero; see
#' \code{\link{compute_n_hat}} for the normalization and term-premium
#' convention. Missing values are retained and propagate through the
#' arithmetic, except in that normalized term premium. Yields whose
#' largest absolute non-missing value is below one trigger a unit-scale
#' warning; the inputs are not rescaled automatically. Invalid maturities,
#' steps, missing columns, and unequal row counts raise structured
#' \code{hetid_error} conditions.
#'
#' @return A list of three numeric vectors, with \code{T = nrow(yields)}:
#'   \code{n_hat_i}, the length-T n_hat(i, t) level series;
#'   \code{n_hat_i_minus_1}, the length-T previous-maturity series
#'   n_hat(i - step, t) (the realized log step-bond price at the
#'   \code{i == step} boundary), consumed by
#'   \code{\link{compute_news_q_bound}} for its led leg; and
#'   \code{delta_p}, the log price news (T-1 elements when T >= 2)
#'   \code{delta_p[t] = n_hat(i - step, t + 1) - n_hat(i, t)}.
#'   All three vectors are undated and in log-price units. For inputs with
#'   T < 2 that pass validation, \code{delta_p} is \code{numeric(0)}.
#' @keywords internal
compute_news_components <- function(yields, term_premia, i,
                                    step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_news_maturity_index(i, step = step)
  n_hat_i <- n_hat_series(yields, term_premia, i, step = step)
  n_hat_i_minus_1 <- compute_n_hat_previous(yields, term_premia, i, step = step)
  delta_p <- compute_time_series_news(n_hat_i, n_hat_i_minus_1)
  list(
    n_hat_i = n_hat_i,
    n_hat_i_minus_1 = n_hat_i_minus_1,
    delta_p = delta_p
  )
}
