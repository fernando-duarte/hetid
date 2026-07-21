#' Compute the Variance Bound for the Expected SDF
#'
#' Computes \eqn{\widehat U_{i}^{(0)}}, the reported bound on the
#' unconditional variance of the expected-SDF level-approximation error
#' \eqn{\varepsilon_{i,t}^{(0)}} produced by
#' \code{compute_expected_sdf(paired = TRUE)} (the bound is built from the
#' paired gap, so it pairs with that estimator, not the decoupled default).
#' It returns \eqn{\min\{(1/4)\,\widehat C\,\widehat K,\;
#' \widehat{\mathrm{Var}}(q)\}}: the fourth-order component of the
#' envelope/Taylor bound paired with the first-order-cancelled
#' \eqn{q}-bound, where \eqn{q_t = e^{n\_hat}(e^u - 1 - u)} (with
#' \eqn{u = x - n\_hat} the raw log forecast error),
#' \eqn{\widehat C = \max_t e^{2 n\_hat}}, and
#' \eqn{\widehat K = \mathrm{mean}(u^4)}. The variance uses divisor
#' \eqn{N} via \code{\link{centered_cov}}.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return Numeric scalar \eqn{\widehat U_{i}^{(0)} \ge 0}; \code{Inf} when
#'   both arms overflow on a nonempty paired sample (conservative, never
#'   sharp); \code{NA_real_} only when no finite paired observations remain.
#'
#' @section Mathematical Formula:
#' With \eqn{s = i / step}, \eqn{T_i = \{1, \dots, T - s\}}, and \eqn{N_i =
#' |T_i|} the number of finite paired observations,
#' \deqn{\widehat U_{i}^{(0)} = \min\!\left\{
#'   \frac{1}{4}\,\Big(\max_t e^{2 a_t}\Big)\,\frac{1}{N_i}\sum_t u_t^4,\;
#'   \frac{1}{N_i}\sum_{t}(q_t - \bar q)^2
#' \right\}}
#' where \eqn{q_t = e^{a_t}(e^{u_t} - 1 - u_t)},
#' \eqn{x_t = -m(\mathrm{step}) y^{(\mathrm{step})}_{t+s}/100},
#' \eqn{a_t = n\_hat(i,t)}, \eqn{u_t = x_t - a_t}. Every ingredient is
#' computed on the estimator's own finite paired mask (one sample for the
#' point estimate, the centering, and both bound arms). The q variance is
#' centered at its own sample mean with divisor \eqn{N_i} (the spec's
#' \eqn{1/N}), computed with \code{\link{centered_cov}}, not the
#' \eqn{1/(N-1)} of \code{stats::var}.
#'
#' @section Time units:
#' The realized one-period yield is led \code{i / step} rows; rows count
#' news periods, not calendar time, so \code{i} must be a positive multiple
#' of \code{step} and the row frequency must equal the news period.
#'
#' @note The projection (gap-variance) bound \eqn{\widehat{\mathrm{Var}}(g)}
#'   is no longer part of the returned min: on financial data it exceeds the
#'   q arm by orders of magnitude and never binds. The component arm
#'   replaces it because the two remaining arms are NOT ordered: the
#'   truncated \eqn{(1/4) C K} can fall below \eqn{\mathrm{Var}(q)}, so the
#'   min is not redundant.
#'
#' @note The component arm is the plug-in leading fourth-order term of the
#'   envelope/Taylor bound, not a literal finite-sample upper bound: the
#'   full bound adds the remainder moment and a cross term, and sample
#'   moments can lie below their population counterparts. When the
#'   component arm wins the min, the reported value carries the same
#'   leading-term status as the SDF-news bound of
#'   \code{\link{compute_variance_bound}}; when the q arm wins (every
#'   maturity on the shipped ACM data), the reported value is the exact
#'   first-order-cancelled bound estimate.
#'
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step},
#'   because \code{n_hat(i, t)} requires data at maturity \code{i + step}.
#'
#' @note Passing \code{i = 0} returns \code{0}: the horizon-0 expected SDF is
#'   the realized one-period price (exact, no approximation), so its
#'   error-variance bound is identically zero. A
#'   \code{hetid_warning_horizon_zero} warning is signaled, mirroring
#'   \code{\link{compute_expected_sdf}}.
#'
#' @note Calling this with \code{i = (m + j - 1) * step} returns the bound for
#'   the shifted-information expectation \eqn{E_{t-j}[\mathrm{SDF}_{t+m}]} (read
#'   off \code{compute_expected_sdf(i = (m + j - 1) * step, paired = TRUE)} at the
#'   formation date \eqn{t-j}); the bound depends only on the horizon
#'   \eqn{s = m + j - 1} and is shift-invariant in the information lag \code{j}.
#'
#' @seealso \code{\link{compute_expected_sdf}},
#'   \code{\link{compute_variance_bound}}, \code{\link{compute_news_q_bound}}
#'
#' @export
#'
#' @examples
#' # Extract ACM data (the i = 60 horizon needs maturities 12, 60, and 72)
#' data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = c(12, 60, 72)
#' )
#' yields <- data[, paste0("y", c(12, 60, 72))]
#' term_premia <- data[, paste0("tp", c(12, 60, 72))]
#'
#' # Variance bound for the 5-year (60-month) horizon
#' bound_60 <- compute_expected_sdf_variance_bound(yields, term_premia, i = 60)
#'
compute_expected_sdf_variance_bound <- function(yields, term_premia, i,
                                                step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_expected_sdf_inputs(yields, term_premia, i, step)

  if (i == 0) {
    # Horizon 0 is exact (M^(0)_{0,t} = P^(1)_t): no approximation error, so the
    # bound is identically 0
    warn_horizon_zero(
      paste0(
        "i = 0: the horizon-0 expected SDF is exact (the realized one-period ",
        "price), so its approximation-error variance bound is identically 0."
      )
    )
    return(0)
  }

  # compute_expected_sdf_gap owns the multiple-of-step contract (it does the
  # i/step row shift), so it is validated there, not here
  components <- compute_expected_sdf_gap(yields, term_premia, i, step = step)

  # Degenerate data (no finite paired observations): no bound to estimate.
  # Matches the NA_real_ contract of compute_c_hat / compute_k_hat
  if (length(components$gap) == 0) {
    return(NA_real_)
  }

  # q arm: Inf when q overflows on the shared mask (guarded, never dropped)
  var_q <- guarded_centered_var(components$q)

  # Component arm (1/4)*C*K. Either factor can overflow -- or 0 * Inf can
  # produce NaN -- only at astronomically large |n_hat|; a non-finite
  # component loses the min, so failure is conservative, never sharp
  component <- 0.25 * max(exp(2 * components$n_hat)) * mean(components$u^4)
  if (!is.finite(component)) {
    component <- Inf
  }

  min(component, var_q)
}
