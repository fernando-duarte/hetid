#' Shared Gap Series for the Expected-SDF Level Approximation
#'
#' Builds the components shared by \code{\link{compute_expected_sdf}} and
#' \code{\link{compute_expected_sdf_variance_bound}}: the full-length
#' \eqn{e^{n\_hat(i,t)}} series and the paired gap series
#' \eqn{g_t = e^{-y^{(1)}_{t+s}} - e^{n\_hat(i,t)}} over
#' \eqn{T_i = \{1, \dots, T - s\}}, \eqn{s = i / step}. The gap's mean is
#' the centering correction; its 1/N variance is the projection bound.
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
#' @return A list with \code{exp_n_hat} (the full-length \eqn{T}
#'   \eqn{e^{n\_hat(i,t)}} series, used only by \code{compute_expected_sdf}
#'   for its output) and \code{gap} (the gap series \eqn{g_t} over the
#'   finite paired dates, length \eqn{\le T - s} after dropping any
#'   non-finite pair).
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

  n_hat <- compute_n_hat(yields, term_premia, i, step = step)
  y_step <- require_column(
    yields, acm_column_name("yields", step), "yields"
  )

  horizon_periods <- i %/% step
  n_obs <- length(n_hat)
  assert_insufficient_data_ok(
    n_obs > horizon_periods,
    "Not enough observations. Need T > i/step news periods"
  )

  exp_n_hat <- exp(n_hat)
  exp_n_hat_paired <- exp_n_hat[seq_len(n_obs - horizon_periods)]
  y_step_future <- y_step[seq.int(horizon_periods + 1L, n_obs)]

  # Realized one-period price e^{-y^(1)_{t+s}}: step-bond log price scales
  # the annualized yield by its maturity in years, then percent to decimal
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  realized_price <- exp(
    -m_step * y_step_future / HETID_CONSTANTS$PERCENT_TO_DECIMAL
  )

  # Pre-filter to finite pairs: is.finite(gap) equals the per-leg mask
  # is.finite(realized_price) & is.finite(exp_n_hat_paired) here, because
  # both legs are exp() (nonnegative, no finite-minus-finite overflow), so
  # the difference is non-finite iff either leg is. Returning the filtered
  # gap means callers consume it directly with no mask to forget.
  gap <- realized_price - exp_n_hat_paired
  list(exp_n_hat = exp_n_hat, gap = gap[is.finite(gap)])
}
