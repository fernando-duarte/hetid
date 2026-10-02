#' Compute Fourth-Moment News Estimator (k2_hat)
#'
#' Computes k2_hat_i, the fourth moment of the price-news term that, with
#' \code{\link{compute_k_hat}} (k1_hat), forms the variance-bound leading
#' term \eqn{U_i = (1/4) c\_hat_i (k\_hat_i + k2\_hat_i)}.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return A numeric scalar, the mean fourth power of the retained decimal
#'   log-price news, or \code{NA_real_} when all retained
#'   observations are missing.
#'
#' @section Mathematical Formula:
#' \deqn{k2\_hat_i = \mathrm{mean}_t (\Delta_{t+1} p_{t+i}^{(1)})^4}
#' over the bound index set \eqn{t = 1, \dots, T - i/step}, where
#' \eqn{T} is the number of input rows. This is the same index set as
#' \code{\link{compute_k_hat}} and \code{\link{compute_c_hat}};
#' missing values are omitted separately by each estimator.
#' \code{i} must be a positive multiple of \code{step}.
#'
#' @details
#' k2_hat captures the contribution of the nonzero conditional mean of the
#' centered approximation error to the variance bound. Unlike k1_hat it
#' can be nonzero at the one-period maturity \code{i == step}, where the
#' realized forecast error vanishes. It is zero when every retained
#' price-news value is zero.
#'
#' Supply numeric yield and term-premium columns in annualized percentage
#' points, with rows already aligned to the same dates in time order.
#' Required maturities are \code{i - step}, \code{i}, and \code{i + step}
#' in both inputs when \code{i > step}; at \code{i == step}, supply
#' \code{step} and \code{2 * step}. The step-maturity term premium is
#' normalized to zero by the news kernel. The term premia must follow the
#' same rollover convention as the news step; see
#' \code{\link{compute_n_hat}}.
#'
#' Each row is one news period, so the observation frequency must match
#' \code{step} months. The default step is annual; monthly observations
#' require \code{step = 1}. A valid \code{step} is a positive integer no
#' larger than \code{HETID_CONSTANTS$MAX_MATURITY \%/\% 2}; \code{i} must
#' not exceed \code{HETID_CONSTANTS$MAX_MATURITY - step}.
#'
#' The mean omits \code{NA} and \code{NaN} price-news values after
#' trimming, using the number of retained non-missing observations as
#' its divisor. Infinite values are not removed. Fewer than or equal to
#' \code{i / step} input rows raise \code{hetid_error_insufficient_data}.
#' Invalid indices or steps, missing required columns, and unequal input
#' row counts raise structured \code{hetid_error} conditions. Yields
#' whose maximum absolute non-missing value is below one produce a
#' \code{hetid_warning_unit_scale} warning because they may be decimals.
#'
#' @seealso \code{\link{compute_k_hat}} for the companion k1 term and
#'   \code{\link{compute_variance_bound}} for the assembled bound.
#'
#' @export
#'
#' @examples
#' # Monthly observations need a one-month news step
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(59, 60, 61)
#' )
#' yields <- data[, paste0("y", c(59, 60, 61))]
#' term_premia <- data[, paste0("tp", c(59, 60, 61))]
#'
#' k2_hat_60 <- compute_k2_hat(yields, term_premia, i = 60, step = 1)
#' k2_hat_60
#'
compute_k2_hat <- function(yields, term_premia, i,
                           step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_news_kernel_inputs(
    yields, term_premia, i, step,
    step_multiple_reason = HETID_CONSTANTS$BOUND_INDEX_TRIM_MSG
  )

  delta_p <- compute_news_components(yields, term_premia, i, step = step)$delta_p
  keep <- trim_to_bound_index_set(delta_p, i, step, len_offset = 1L)

  if (length(keep) == 0) {
    return(NA_real_)
  }

  mean(keep^4)
}
