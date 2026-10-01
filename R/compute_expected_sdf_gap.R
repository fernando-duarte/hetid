#' Shared Gap Series for the Expected-SDF Level Approximation
#'
#' Builds the components shared by
#' \code{\link{compute_expected_sdf}} with \code{paired = TRUE} and
#' \code{\link{compute_expected_sdf_variance_bound}}: the full-length
#' \eqn{e^{n\_hat(i,t)}} series, the paired gap series
#' \eqn{g_t = e^{-y^{(1)}_{t+s}} - e^{n\_hat(i,t)}}, and the
#' first-order-cancelled gap \eqn{q_t = e^{n\_hat}(e^u - 1 - u)} where
#' \eqn{u = x - n\_hat} is the raw log forecast error (\eqn{x} the realized
#' log price), plus the masked \eqn{u} and \eqn{n\_hat} series themselves.
#' The paired series use one common \code{is.finite(gap)} mask over
#' \eqn{T_i = \{1, \dots, T - s\}}, \eqn{s = i / step}, so the
#' estimator's centering and every bound arm share a single sample.
#' The full-length \code{exp_n_hat} series is not filtered. The
#' gap's mean is the centering correction; the q variance and the
#' fourth-order component \eqn{(1/4)\max(e^{2 n\_hat})\,\mathrm{mean}(u^4)}
#' are the two arms of the min returned by
#' \code{\link{compute_expected_sdf_variance_bound}}.
#'
#' Callers validate \code{step}, \code{i} (within
#' \code{effective_max_maturity(step)}), and row alignment before calling.
#' The allowed \code{step} values are integers from 1 through
#' \code{HETID_CONSTANTS$MAX_MATURITY \%/\% 2}.
#' This helper requires \code{i} to be a positive multiple of \code{step};
#' it does not handle the public callers' \code{i = 0} boundary.
#' The realized one-period yield is led \code{i / step} rows. Input rows
#' must already be aligned by date, and their frequency must equal the
#' intended news period; this helper does not match dates or reorder rows.
#'
#' Yields and term premia are in annualized percentage points.
#' The realized log price is
#' \eqn{x_t = -m(\mathrm{step}) y^{(\mathrm{step})}_{t+s} / 100}, where
#' \eqn{m(\mathrm{step})} is
#' \code{step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR}; thus
#' \eqn{e^{-y^{(1)}_{t+s}} = e^{x_t}}. The required columns are the
#' \code{step}-maturity yield and the \code{i} and \code{i + step}
#' yields and term premia, as used by \code{\link{compute_n_hat}}.
#'
#' Invalid horizons or missing columns raise
#' \code{hetid_error_bad_argument}, unequal row counts raise
#' \code{hetid_error_dimension_mismatch}, and \code{T <= i / step}
#' raises \code{hetid_error_insufficient_data}. Yields that look like
#' decimals trigger \code{hetid_warning_unit_scale}; inputs are not rescaled.
#'
#' @template param-yields-term-premia
#' @template param-maturity-index
#' @template param-step
#'
#' @return A named list with five numeric vectors. The four paired vectors
#'   retain their input order and contain no dates; they are all
#'   \code{numeric(0)} if no finite gaps remain:
#'   \describe{
#'     \item{\code{exp_n_hat}}{The full-length \eqn{T} series
#'       \eqn{e^{n\_hat(i,t)}}, used only by \code{compute_expected_sdf}
#'       for its output. Missing or non-finite values are retained.}
#'     \item{\code{gap}}{The gap series \eqn{g_t} over the finite paired
#'       dates (length \eqn{\le T - s} after dropping any non-finite pair).}
#'     \item{\code{q}}{The first-order-cancelled gap \eqn{q_t =
#'       e^{n\_hat}(e^u - 1 - u)}, \eqn{u = x - n\_hat}, aligned to
#'       \code{gap}'s finite paired set (one common \code{is.finite(gap)} mask).
#'       \code{q} shares the gap's conditional mean in population
#'       (\eqn{E[u | \mathrm{info}] = 0}) and its \eqn{1/N} variance is a
#'       sharper \eqn{O(\sigma^4)} bound. \code{q} is \strong{not} itself guaranteed
#'       finite: a yield \eqn{\to +\infty} gives \eqn{q = +\infty}, and an
#'       extreme \eqn{n\_hat} can give \eqn{0 \cdot \infty = \mathrm{NaN}}.
#'       The bound function guards its variance against this.}
#'     \item{\code{u}}{The raw log forecast error \eqn{u = x - n\_hat} on the
#'       same mask; \eqn{\mathrm{mean}(u^4)} is the fourth-moment factor of
#'       the component arm.}
#'     \item{\code{n_hat}}{The paired forecast series on the same mask;
#'       \eqn{\max(e^{2 n\_hat})} is the envelope factor of the component
#'       arm.}
#'   }
#' @keywords internal
compute_expected_sdf_gap <- function(yields, term_premia, i,
                                     step = HETID_CONSTANTS$DEFAULT_STEP) {
  validate_step_multiple(
    i, step,
    "the realized one-period yield is led whole news periods"
  )

  n_hat <- n_hat_series(yields, term_premia, i, step = step)
  y_step <- require_acm_col(yields, "yields", step)

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

  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  realized_log <- -m_step * y_step_future / HETID_CONSTANTS$PERCENT_TO_DECIMAL
  realized_price <- exp(realized_log)

  gap <- realized_price - exp_n_hat_paired

  u <- realized_log - n_hat_paired
  q_gap <- q_kernel(n_hat_paired, u)

  mask <- is.finite(gap)
  list(
    exp_n_hat = exp_n_hat,
    gap = gap[mask],
    q = q_gap[mask],
    u = u[mask],
    n_hat = n_hat_paired[mask]
  )
}

#' First-Order-Cancelled Gap Kernel
#'
#' The numerically delicate kernel \eqn{q = e^{n\_hat}(e^u - 1 - u)},
#' shared by the level gap and both legs of the news q-bound so the three
#' uses cannot drift. \code{expm1()} evaluates \eqn{e^u - 1} accurately
#' near zero; subtracting \code{u} removes the first-order term.
#' The remaining subtraction can still lose precision for very small
#' \code{u}, and exponentiation or multiplication can produce non-finite
#' values. This kernel does not filter or guard its result.
#'
#' @param n_hat Numeric vector of log-price forecasts.
#' @param u Numeric vector of log forecast errors, the same length as
#'   \code{n_hat}; callers supply conformable vectors.
#' @return Numeric vector \eqn{e^{n\_hat}(\mathrm{expm1}(u) - u)},
#'   including any missing or non-finite results.
#' @noRd
q_kernel <- function(n_hat, u) {
  exp(n_hat) * (expm1(u) - u)
}
