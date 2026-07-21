#' Compute the First-Order-Cancelled Variance Bound for SDF News
#'
#' Computes \eqn{\widehat U_{q,i}^{N}}, the two-leg first-order-cancelled
#' (Minkowski) bound on the unconditional variance of the centered
#' quadratic SDF-news approximation error whose envelope bound is
#' \code{\link{compute_variance_bound}}:
#' \deqn{\widehat U_{q,i}^{N} = \Big[\hat\sigma(\hat q^{(1)}) +
#'   \hat\sigma(\hat q^{(0)}) + \hat\sigma(\hat g)\Big]^2,}
#' with \eqn{\hat\sigma(\cdot)} the square root of the divisor-\eqn{N}
#' centered variance. Neither this bound nor the envelope bound dominates
#' the other, so the reported news bound is their pointwise minimum
#' (taken by the caller; the two estimators stay atomic).
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return Numeric scalar \eqn{\widehat U_{q,i}^{N} \ge 0}; \code{Inf} when
#'   a leg overflows on a nonempty primitive sample (conservative: the
#'   envelope arm then wins the caller's min); \code{NA_real_} only when no
#'   finite primitive observations remain.
#'
#' @section Mathematical Formula:
#' With \eqn{s = i / step}, over the paired set \eqn{T_i = \{1, \dots,
#' T - s\}} restricted to rows where the primitives
#' \eqn{n\_hat(i, t)}, \eqn{n\_hat(i - step, t + 1)}, and the realized log
#' one-period price \eqn{x_t = -m(\mathrm{step})
#' y^{(\mathrm{step})}_{t+s}/100} are all finite:
#' \deqn{\hat q^{(0)}_t = e^{a_t}(e^{x_t - a_t} - 1 - (x_t - a_t)), \quad
#'   a_t = n\_hat(i, t),}
#' \deqn{\hat q^{(1)}_t = e^{b_t}(e^{x_t - b_t} - 1 - (x_t - b_t)), \quad
#'   b_t = n\_hat(i - step, t + 1),}
#' \deqn{\hat g_t = e^{a_t}\big(e^{d_t} - 1 - d_t - d_t^2/2\big), \quad
#'   d_t = b_t - a_t.}
#' The mask is on the primitives, never on the transformed legs: a row a
#' leg overflows on is kept, and the affected \eqn{\hat\sigma} arm returns
#' \code{Inf} instead (dropping extreme observations would understate a
#' variance bound).
#'
#' @note At the boundary \code{i == step} the led leg \eqn{b_t} is the
#'   realized log price of the step-maturity bond (term premium zero by
#'   construction), supplied by the shared news components.
#'
#' @note The bound is exact (no remainder is discarded) and \eqn{O(\sigma^4)}
#'   like the envelope bound, but each leg pays the full unprojected carrier
#'   variance and the Minkowski step ignores the cancellation between the
#'   two level errors, so it is not uniformly tighter; on the shipped ACM
#'   data it wins only at the shortest news maturity.
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step},
#'   because \code{n_hat(i, t)} requires data at maturity \code{i + step};
#'   the minimum is \code{step} (there is no horizon-zero news).
#'
#' @seealso \code{\link{compute_variance_bound}},
#'   \code{\link{compute_expected_sdf_variance_bound}}
#'
#' @export
#'
#' @examples
#' # The i = 60 news bound needs maturities 12, 48, 60, and 72
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(12, 48, 60, 72)
#' )
#' yields <- data[, paste0("y", c(12, 48, 60, 72))]
#' term_premia <- data[, paste0("tp", c(12, 48, 60, 72))]
#'
#' news_q_60 <- compute_news_q_bound(yields, term_premia, i = 60)
#'
compute_news_q_bound <- function(yields, term_premia, i,
                                 step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_news_kernel_inputs(
    yields, term_premia, i, step,
    step_multiple_reason = HETID_CONSTANTS$BOUND_INDEX_TRIM_MSG
  )

  # shared news alignment: n_hat_i, the previous-maturity leg (realized log
  # step-bond price at i == step), and the delta_p definition live here
  components <- compute_news_components(yields, term_premia, i, step = step)
  y_step <- require_acm_col(yields, "yields", step)

  horizon_periods <- i %/% step
  n_obs <- length(components$n_hat_i)
  assert_insufficient_data_ok(
    n_obs > horizon_periods,
    HETID_CONSTANTS$INSUFFICIENT_NEWS_MSG
  )

  paired <- seq_len(n_obs - horizon_periods)
  n_hat_0 <- components$n_hat_i[paired]
  n_hat_1 <- components$n_hat_i_minus_1[paired + 1L]
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  realized_log <- -m_step * y_step[paired + horizon_periods] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL

  # mask on the primitives, never on the transformed legs: an overflowing
  # leg keeps its row and turns its variance arm Inf instead
  mask <- is.finite(n_hat_0) & is.finite(n_hat_1) & is.finite(realized_log)
  if (!any(mask)) {
    return(NA_real_)
  }
  n_hat_0 <- n_hat_0[mask]
  n_hat_1 <- n_hat_1[mask]
  realized_log <- realized_log[mask]

  q_0 <- q_kernel(n_hat_0, realized_log - n_hat_0)
  q_1 <- q_kernel(n_hat_1, realized_log - n_hat_1)
  # third-order news gap on the news step d (= delta_p over the paired rows).
  # For tiny |d| the subtraction reaches the floating-point floor, harmless
  # because sigma(g) is added to two much larger terms
  d <- n_hat_1 - n_hat_0
  g <- exp(n_hat_0) * (expm1(d) - d - d^2 / 2)

  (sqrt(guarded_centered_var(q_1)) +
    sqrt(guarded_centered_var(q_0)) +
    sqrt(guarded_centered_var(g)))^2
}
