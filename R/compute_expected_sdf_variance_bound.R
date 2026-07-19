#' Compute the Variance Bound for the Expected SDF
#'
#' Computes \eqn{\widehat U_{i}^{(0)}}, the tighter empirical bound on the
#' unconditional variance of the expected-SDF level-approximation error
#' \eqn{\varepsilon_{i,t}^{(0)}} produced by
#' \code{compute_expected_sdf(paired = TRUE)} (the bound is built from the
#' paired gap, so it pairs with that estimator, not the decoupled default).
#' It returns \eqn{\min\{\widehat{\mathrm{Var}}(g), \widehat{\mathrm{Var}}(q)\}},
#' where \eqn{g_t = e^{-y^{(1)}_{t+s}} - e^{n\_hat(i,t)}} is the gap series
#' and \eqn{q_t = e^{n\_hat}(e^u - 1 - u)} (with \eqn{u = x - n\_hat} the raw
#' log forecast error) is the first-order-cancelled gap; both variances use
#' divisor \eqn{N} via \code{\link{centered_cov}}.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return Numeric scalar \eqn{\widehat U_{i}^{(0)} \ge 0}, or
#'   \code{NA_real_} when no finite paired observations remain.
#'
#' @section Mathematical Formula:
#' With \eqn{s = i / step}, \eqn{T_i = \{1, \dots, T - s\}}, and \eqn{N_i =
#' |T_i|} the number of finite paired observations,
#' \deqn{\widehat U_{i}^{(0)} = \min\!\left\{
#'   \frac{1}{N_i}\sum_{t}(g_t - \bar g)^2,\;
#'   \frac{1}{N_i}\sum_{t}(q_t - \bar q)^2
#' \right\}}
#' where \eqn{g_t = e^{x_t} - e^{a_t}},
#' \eqn{q_t = e^{a_t}(e^{u_t} - 1 - u_t)},
#' \eqn{x_t = -m(\mathrm{step}) y^{(\mathrm{step})}_{t+s}/100},
#' \eqn{a_t = n\_hat(i,t)}, \eqn{u_t = x_t - a_t}.
#' \eqn{\bar g} is exactly the centering correction added by
#' \code{compute_expected_sdf(paired = TRUE)}; both \eqn{g} and \eqn{q} are centered at
#' their own sample means (\code{centered_cov} centers internally), and
#' \eqn{E[g] = E[q]} only in population. The divisor is \eqn{N_i} (the
#' spec's \eqn{1/N}), computed with \code{\link{centered_cov}}, not the
#' \eqn{1/(N-1)} of \code{stats::var}.
#'
#' @section Time units:
#' The realized one-period yield is led \code{i / step} rows; rows count
#' news periods, not calendar time, so \code{i} must be a positive multiple
#' of \code{step} and the row frequency must equal the news period.
#'
#' @note The bound estimates the population \eqn{\mathrm{Var}(\varepsilon)},
#'   with \eqn{\mathrm{Var}(q)} the sharper estimate: since
#'   \eqn{E[u_t | \mathrm{info}_t] = 0} and \eqn{q_t = O(u_t^2)}, the
#'   \eqn{q}-variance is \eqn{O(\sigma^4)} rather than \eqn{O(\sigma^2)}.
#'   On real financial data \eqn{\mathrm{Var}(q)} binds and is several orders
#'   of magnitude below \eqn{\mathrm{Var}(g)}, so the returned bound is far
#'   smaller than a pure gap-variance bound. The \eqn{\min} therefore
#'   essentially always selects \eqn{\widehat{\mathrm{Var}}(q)}: the returned
#'   value is the sharper bound's estimate, not a min-of-noise artifact of two
#'   comparably sized variances.
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
#' @seealso \code{\link{compute_expected_sdf}}, \code{\link{compute_variance_bound}}
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
  gap <- components$gap

  # Degenerate data (no finite paired observations): no bound to estimate.
  # Matches the NA_real_ contract of compute_c_hat / compute_k_hat
  if (length(gap) == 0) {
    return(NA_real_)
  }

  q_gap <- components$q
  var_g <- centered_var(gap)
  # var_q defaults to Inf so a non-finite q or variance cannot turn min() into
  # NA; length(q) == length(gap) > 0 here, so no length check is needed
  var_q <- Inf
  if (all(is.finite(q_gap))) {
    candidate <- centered_var(q_gap)
    if (is.finite(candidate)) {
      var_q <- candidate
    }
  }
  min(var_g, var_q)
}
