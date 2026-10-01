#' Compute Variance-Bound Leading Term
#'
#' Computes the plug-in leading fourth-order term of the SDF-news
#' approximation-error variance bound,
#' \eqn{U_i = (1/4) c\_hat_i (k\_hat_i + k2\_hat_i)}.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#' @param c_bar Optional positive finite numeric scalar representing a
#'   deterministic envelope. When \code{NULL}
#'   (default) the sample maximum \code{\link{compute_c_hat}} is used;
#'   when a positive scalar is supplied it replaces the envelope, giving
#'   the spec's envelope-conservative variant
#'   \eqn{U_i^{bd} = (1/4)\,\bar C_i\,(k1 + k2)}.
#'
#' @return A numeric value of \code{0.25 * c_hat_i * (k_hat_i + k2_hat_i)}, or
#'   \code{0.25 * c_bar * (k_hat_i + k2_hat_i)} when \code{c_bar} is supplied, or
#'   \code{NA_real_} when a required component estimator
#'   (\code{\link{compute_c_hat}} when \code{c_bar} is \code{NULL},
#'   \code{\link{compute_k_hat}}, or \code{\link{compute_k2_hat}}) has no
#'   valid paired observations.
#'
#' @details
#' The leading fourth-order term is
#' \deqn{U_i = \frac{1}{4} C_i (K1_i + K2_i)}
#' estimated by the plug-in \eqn{\frac{1}{4} c\_hat_i (k\_hat_i +
#' k2\_hat_i)}. It is a plug-in approximation
#' to the leading part of the theoretical bound, not a literal
#' finite-sample upper bound: sample fourth moments can lie below their
#' population counterparts and the higher-order remainder is omitted. At
#' the one-period maturity \code{i == step} the k_hat (k1) term is zero,
#' so only the k2_hat contribution remains; it can be zero when all
#' retained price-news observations are zero.
#'
#' Supplying \code{c_bar} substitutes a user-specified deterministic
#' envelope for the sample maximum (the spec's U^bd variant); its
#' population target is conservative relative to the minimal-envelope
#' target when \code{c_bar} is at least the minimal envelope.
#'
#' Inputs must contain numeric yield and term-premium columns in annualized
#' percentage points, with rows aligned to the same observation dates. Date
#' columns are not read or matched here. There must be more than \code{i / step}
#' rows, and the row frequency must equal the news period: the default annual
#' step requires annual observations. The term-premium rollover convention must
#' also match the news period; see \code{\link{compute_n_hat}}.
#'
#' Each component uses dates \code{1, ..., T - i / step}, where \code{T} is the
#' number of rows, and omits its own missing terms. Components can therefore
#' use different observations. No missing values are imputed. With sufficient
#' rows but no usable observations for a required component, the result is
#' \code{NA_real_}, including when \code{c_bar} is supplied.
#'
#' Invalid maturity, step, envelope, or missing required columns raise
#' \code{hetid_error_bad_argument}; unequal row counts raise
#' \code{hetid_error_dimension_mismatch}; too few rows raise
#' \code{hetid_error_insufficient_data}. Yields that look like decimal rates
#' trigger a unit-scale warning; they are not automatically rescaled.
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step}, because
#'   \code{\link{compute_k2_hat}} runs on every call
#'   and reads data at maturity \code{i + step}, whether or not \code{c_bar}
#'   replaces the estimated envelope. Separately, \code{i} must
#'   be a positive multiple of \code{step}, as required by
#'   \code{\link{compute_k_hat}} and \code{\link{compute_k2_hat}}.
#'   The step must be a positive integer no larger than
#'   \code{HETID_CONSTANTS$MAX_MATURITY %/% 2L}.
#'
#' @export
#'
#' @examples
#' # Monthly data and step match the ACM rollover convention
#' mats <- c(1, 59, 60, 61)
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"), maturities = mats
#' )
#' yields <- data[, paste0("y", mats)]
#' term_premia <- data[, paste0("tp", mats)]
#' var_bound_60 <- compute_variance_bound(yields, term_premia, i = 60, step = 1)
#' var_bound_60
#' # A supplied envelope replaces only the estimated maximum
#' compute_variance_bound(yields, term_premia, i = 60, step = 1, c_bar = 1.05)
compute_variance_bound <- function(yields, term_premia, i,
                                   step = HETID_CONSTANTS$DEFAULT_STEP,
                                   c_bar = NULL) {
  validate_news_kernel_inputs(yields, term_premia, i, step)

  if (is.null(c_bar)) {
    c_hat <- compute_c_hat(yields, term_premia, i, step = step)
  } else {
    assert_bad_argument_ok(
      is.numeric(c_bar) && length(c_bar) == 1L && is.finite(c_bar) && c_bar > 0,
      "c_bar must be a single positive finite number (a deterministic envelope)",
      arg = "c_bar"
    )
    c_hat <- c_bar
  }

  k_hat <- compute_k_hat(yields, term_premia, i, step = step)
  k2_hat <- compute_k2_hat(yields, term_premia, i, step = step)
  0.25 * c_hat * (k_hat + k2_hat)
}
