#' Shared Gap Series for the Expected-SDF Level Approximation
#'
#' Builds the components shared by \code{\link{compute_expected_sdf}} and
#' \code{\link{compute_expected_sdf_variance_bound}}: the full-length
#' \eqn{e^{n\_hat(i,t)}} series, the paired gap series
#' \eqn{g_t = e^{-y^{(1)}_{t+s}} - e^{n\_hat(i,t)}}, and the
#' first-order-cancelled gap \eqn{q_t = e^{n\_hat}(e^u - 1 - u)} where
#' \eqn{u = x - n\_hat} is the raw log forecast error (\eqn{x} the realized
#' log price). Both \eqn{g} and \eqn{q} are filtered to the common finite
#' paired set \eqn{T_i = \{1, \dots, T - s\}}, \eqn{s = i / step}. The
#' gap's mean is the centering correction; \eqn{\min\{Var(g), Var(q)\}} is
#' the variance bound returned by
#' \code{\link{compute_expected_sdf_variance_bound}}.
#'
#' Callers validate \code{step}, \code{i} (within
#' \code{effective_max_maturity(step)}), and row alignment before calling.
#' This helper owns the multiple-of-step contract (it performs the
#' \code{i/step} row shift) via \code{validate_step_multiple}, and raises
#' the structural \code{T > i/step} guard.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{\code{exp_n_hat}}{The full-length \eqn{T} series
#'       \eqn{e^{n\_hat(i,t)}}, used only by \code{compute_expected_sdf}
#'       for its output.}
#'     \item{\code{gap}}{The gap series \eqn{g_t} over the finite paired
#'       dates (length \eqn{\le T - s} after dropping any non-finite pair).
#'       Byte-identical to the previous implementation; \code{compute_expected_sdf}
#'       stays numerically invariant.}
#'     \item{\code{q}}{The first-order-cancelled gap \eqn{q_t =
#'       e^{n\_hat}(e^u - 1 - u)}, \eqn{u = x - n\_hat}, aligned to
#'       \code{gap}'s finite paired set (one common \code{is.finite(gap)} mask).
#'       \code{q} shares \code{g}'s conditional mean in population
#'       (\eqn{E[u | \mathrm{info}] = 0}) and its \eqn{1/N} variance is a
#'       sharper \eqn{O(\sigma^4)} bound. \code{q} is NOT itself guaranteed
#'       finite: a yield \eqn{\to +\infty} gives \eqn{q = +\infty}, and an
#'       extreme \eqn{n\_hat} can give \eqn{0 \cdot \infty = \mathrm{NaN}}.
#'       The bound function guards \code{var_q} against this.}
#'   }
#' @keywords internal
compute_expected_sdf_gap <- function(yields, term_premia, i,
                                     step = HETID_CONSTANTS$DEFAULT_STEP) {
  # The helper performs the i/step row shift, so the multiple-of-step
  # contract is intrinsic to it: validate here (single source of truth) so
  # the shared helper can never silently misalign for any caller.
  validate_step_multiple(
    i, step,
    "the realized one-period yield is led whole news periods"
  )

  n_hat <- n_hat_series(yields, term_premia, i, step = step)
  y_step <- require_column(
    yields, acm_column_name("yields", step), "yields"
  )

  horizon_periods <- i %/% step
  n_obs <- length(n_hat)
  assert_insufficient_data_ok(
    n_obs > horizon_periods,
    HETID_CONSTANTS$INSUFFICIENT_NEWS_MSG
  )

  exp_n_hat <- exp(n_hat)
  paired <- seq_len(n_obs - horizon_periods)
  exp_n_hat_paired <- exp_n_hat[paired]
  n_hat_paired <- n_hat[paired]
  y_step_future <- y_step[seq.int(horizon_periods + 1L, n_obs)]

  # Realized one-period log price x = -y^(1)_{t+s} and price e^x: the step-bond
  # log price scales the annualized yield by its maturity in years, then
  # percent to decimal.
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  realized_log <- -m_step * y_step_future / HETID_CONSTANTS$PERCENT_TO_DECIMAL
  realized_price <- exp(realized_log)

  # Gap g = e^x - e^{n_hat}. Left byte-identical so compute_expected_sdf stays
  # numerically invariant.
  gap <- realized_price - exp_n_hat_paired

  # expm1() removes the e^u - 1 cancellation; the trailing `- u` is numerically
  # irrelevant here (var(q) >> machine eps).
  u <- realized_log - n_hat_paired
  q_gap <- exp_n_hat_paired * (expm1(u) - u)

  # is.finite(gap) is the common finite mask for g and q: each leg is exp(), so
  # the difference is non-finite iff a leg is.
  mask <- is.finite(gap)
  list(exp_n_hat = exp_n_hat, gap = gap[mask], q = q_gap[mask])
}
