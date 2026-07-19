#' Compute Variance-Bound Leading Term
#'
#' Computes the plug-in leading fourth-order term of the SDF-news
#' approximation-error variance bound,
#' U_i = (1/4) * c_hat_i * (k_hat_i + k2_hat_i).
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#' @param c_bar Optional deterministic envelope. When \code{NULL}
#'   (default) the sample maximum \code{\link{compute_c_hat}} is used;
#'   when a positive scalar is supplied it replaces the envelope, giving
#'   the spec's envelope-conservative variant
#'   \eqn{U_i^{bd} = (1/4)\,\bar C_i\,(k1 + k2)}.
#'
#' @return Numeric value of (1/4)*c_hat_i*(k_hat_i + k2_hat_i), or
#'   (1/4)*c_bar*(k_hat_i + k2_hat_i) when \code{c_bar} is supplied, or
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
#' so the bound is the strictly positive k2_hat contribution.
#'
#' Supplying \code{c_bar} substitutes a user-specified deterministic
#' envelope for the sample maximum (the spec's U^bd variant); its
#' population target is conservative relative to the minimal-envelope
#' target when \code{c_bar} is at least the minimal envelope.
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step},
#'   because the envelope c_hat needs data at maturity \code{i + step}.
#'   \code{i} must
#'   be a positive multiple of \code{step} (enforced by
#'   \code{\link{compute_k_hat}} and \code{\link{compute_k2_hat}}).
#'
#' @export
#'
#' @examples
#' # Extract ACM data - need maturities step, i-step, i, i+step (months)
#' # For i = 60 with the default annual step: 12, 48, 60, and 72
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(12, 48, 60, 72)
#' )
#' yields <- data[, paste0("y", c(12, 48, 60, 72))]
#' term_premia <- data[, paste0("tp", c(12, 48, 60, 72))]
#'
#' # Compute variance bound for the 5-year (60-month) maturity
#' var_bound_60 <- compute_variance_bound(yields, term_premia, i = 60)
#'
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
