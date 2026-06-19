#' Compute the Variance Bound for the Expected SDF
#'
#' Computes \eqn{\widehat U_{i}^{(0)}}, the tighter empirical bound on the
#' unconditional variance of the expected-SDF level-approximation error
#' \eqn{\varepsilon_{i,t}^{(0)}} produced by \code{\link{compute_expected_sdf}}.
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
#' \code{\link{compute_expected_sdf}}; both \eqn{g} and \eqn{q} are centered at
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
#' @note The effective maximum for \code{i} is \code{MAX_MATURITY - step}
#'   (108 with the default annual step), because \code{n_hat(i, t)} requires
#'   data at maturity \code{i + step}.
#'
#' @note Calling this with \code{i = (m + j - 1) * step} returns the bound for
#'   the shifted estimator \code{\link{compute_expected_sdf_at}}
#'   (\eqn{E_{t-j}[\mathrm{SDF}_{t+m}]}); the bound depends only on the horizon
#'   \eqn{s = m + j - 1} and is shift-invariant in the information lag \code{j}.
#'
#' @seealso \code{\link{compute_expected_sdf}}, \code{\link{compute_variance_bound}},
#'   \code{\link{compute_expected_sdf_at}}
#'
#' @export
#'
#' @examples
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#' yields <- data[, paste0("y", seq(12, 120, 12))]
#' term_premia <- data[, paste0("tp", seq(12, 120, 12))]
#'
#' # Variance bound for the 5-year (60-month) horizon
#' bound_60 <- compute_expected_sdf_variance_bound(yields, term_premia, i = 60)
#'
compute_expected_sdf_variance_bound <- function(yields, term_premia, i,
                                                step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step(step)
  validate_maturity_index(i, max_maturity = effective_max_maturity(step))
  validate_row_alignment(yields, term_premia)

  # compute_expected_sdf_gap owns the multiple-of-step contract (it does
  # the i/step row shift), so it is validated there, not here. It returns
  # the gap series already filtered to finite paired observations.
  components <- compute_expected_sdf_gap(yields, term_premia, i, step = step)
  gap <- components$gap

  # Degenerate data (no finite paired observations): no bound to estimate.
  # Matches the NA_real_ contract of compute_c_hat / compute_k_hat.
  if (length(gap) == 0) {
    return(NA_real_)
  }

  # Two valid divisor-N bounds on Var(epsilon): the gap variance
  # and the variance of q (the first-order-cancelled gap), which is sharper in
  # the small-shock regime. Return the tighter one (the "single best direct
  # bound"). q is over the same paired set as gap (one common mask); guard the
  # never-real case of a non-finite q entry so min() cannot be wiped to NA.
  q_gap <- components$q
  var_g <- centered_cov(gap, gap)[1, 1]
  # var_q defaults to Inf so a non-finite q (pathological: a yield -> +Inf
  # makes q = +Inf; a finite but extreme n_hat can underflow e^{n_hat} to 0
  # while expm1(u) overflows, giving 0*Inf = NaN) -- or a non-finite variance
  # output -- cannot turn min() into NA. length(q) == length(gap) > 0 here (one
  # common mask + the length(gap)==0 guard above), so no length check is needed.
  var_q <- Inf
  if (all(is.finite(q_gap))) {
    candidate <- centered_cov(q_gap, q_gap)[1, 1]
    if (is.finite(candidate)) {
      var_q <- candidate
    }
  }
  min(var_g, var_q)
}
