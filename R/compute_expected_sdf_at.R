#' Conditional Expected SDF at a Shifted Information Date (expected_sdf_at)
#'
#' Convenience wrapper computing the in-sample fitted series
#' \eqn{E_{t-j}[\mathrm{SDF}_{t+m}]}: the expectation of the one-period
#' stochastic discount factor at calendar date \eqn{t+m}, conditional on
#' information available at the earlier date \eqn{t-j}. It re-bases the
#' time-\eqn{t} estimator \code{compute_expected_sdf(paired = TRUE)} by the
#' identity below; no new modeling is introduced. The matched (paired)
#' estimator is used because \eqn{G_s} is defined against it.
#'
#' @template param-yields-term-premia
#' @param j Integer information lag (\eqn{\ge 0}); the expectation
#'   conditions on \eqn{\mathcal{F}_{t-j}}. \code{j = 0} is the contemporaneous
#'   \code{compute_expected_sdf(paired = TRUE)} case.
#' @param m Integer SDF date offset; the discount factor is dated \eqn{t+m}.
#'   May be zero or negative provided \eqn{m + j \ge 2} (see Valid range). Note
#'   this \code{m} is the date offset, distinct from the symbol \eqn{m(step)}
#'   ("step maturity in years") used in \code{\link{compute_expected_sdf}}.
#' @template param-return-df-dates
#' @template param-step
#'
#' @template return-numeric-or-dataframe
#'
#' @section Mathematical Formula:
#' With \eqn{s = m + j - 1} news periods and maturity index
#' \eqn{i = s\cdot\mathrm{step}},
#' \deqn{E_{t-j}[\mathrm{SDF}_{t+m}] = G_s(t-j), \qquad
#'   G_s(\tau) = E_\tau[\mathrm{SDF}_{\tau+1+s}],}
#' where \eqn{G_s} is exactly \code{compute_expected_sdf(paired = TRUE)} at
#' maturity index \eqn{i}. As a series indexed by the reference row \eqn{t}, the result
#' is \eqn{G_s} lagged by \eqn{j} rows; the first \eqn{j} rows are \code{NA}.
#' At \eqn{j = 0} this is \eqn{E_t[\mathrm{SDF}_{t+m}]} with no shift.
#'
#' @section Valid range:
#' \eqn{s = m + j - 1} must lie in
#' \eqn{[1, \mathrm{effective\_max\_maturity}(step)/step]} (with the default
#' annual step, \eqn{s \in \{1,\dots,9\}}, i.e. \eqn{m + j \in \{2,\dots,10\}}).
#' The lower bound excludes the degenerate \eqn{s \le 0} case, where
#' \eqn{\mathrm{SDF}_{t+m}} is already \eqn{\mathcal{F}_{t-j}}-measurable (the
#' realized one-period price, requiring no forecast); the upper bound is the
#' reach of the \code{n_hat} maturity grid.
#'
#' @note Inherits the in-sample-fitted caveat of
#'   \code{\link{compute_expected_sdf}} (an unconditional bias correction
#'   estimated over the whole sample): the returned \eqn{E}-labelled series must
#'   not be fed into a real-time backtest. The \eqn{j}-row lag additionally
#'   yields \code{NA} in the first \eqn{j} rows.
#'
#' @note A bound on this series' approximation-error variance is
#'   \code{compute_expected_sdf_variance_bound(i = (m + j - 1) * step)} (the same
#'   maturity index used here); the bound is shift-invariant in \code{j}.
#'
#' @seealso \code{\link{compute_expected_sdf}}, \code{\link{compute_n_hat}},
#'   \code{\link{compute_expected_sdf_variance_bound}}
#'
#' @export
#'
#' @examples
#' data <- extract_acm_data(data_types = c("yields", "term_premia"))
#' yields <- data[, paste0("y", seq(12, 120, 12))]
#' term_premia <- data[, paste0("tp", seq(12, 120, 12))]
#'
#' # E_{t-1}[SDF_{t+5}]: SDF 5 periods ahead of t, conditioning on the
#' # one-period-lagged information set (s = m + j - 1 = 5 news periods, i = 60)
#' e <- compute_expected_sdf_at(yields, term_premia, j = 1, m = 5)
#'
compute_expected_sdf_at <- function(yields, term_premia, j, m,
                                    return_df = FALSE, dates = NULL,
                                    step = HETID_CONSTANTS$DEFAULT_STEP) {
  # Scalar checks on j, m only; they do not touch the data. `step` is validated
  # by effective_max_maturity() below, and the maturity index + row alignment
  # are delegated to compute_expected_sdf(), so we never inspect nrow(yields)
  # before it is known valid.
  assert_scalar_finite(j, "Information lag j")
  assert_bad_argument_ok(
    j %% 1 == 0 && j >= 0,
    "Information lag j must be a non-negative integer",
    arg = "j"
  )
  assert_scalar_finite(m, "SDF date offset m")
  assert_bad_argument_ok(
    m %% 1 == 0,
    "SDF date offset m must be an integer",
    arg = "m"
  )
  j <- as.integer(j) # strict integers so s and the lag indexing stay integer
  m <- as.integer(m)

  s <- m + j - 1L
  max_s <- effective_max_maturity(step) %/% step # effective_max validates step
  assert_bad_argument_ok(
    s >= 1L && s <= max_s,
    paste0(
      "m + j must be between 2 and ", max_s + 1L,
      " (news horizon s = m + j - 1 must be in [1, ", max_s,
      "]); got m = ", m, ", j = ", j, " (s = ", s, ")"
    ),
    arg = NULL # joint m + j constraint; message names both
  )

  # G_s(tau) over all base rows tau; compute_expected_sdf runs the standard
  # input validation and returns a length-T numeric vector.
  base_series <- compute_expected_sdf(
    yields, term_premia,
    i = s * step, step = step,
    paired = TRUE # G_s is the matched estimator this construction was built on
  )

  # Re-base to reference row t by lagging j rows: out[t] = G_s(t - j). The lag
  # must leave at least one in-sample base row, else the whole series is NA.
  n <- length(base_series)
  assert_insufficient_data_ok(
    j < n,
    paste0(
      "Information lag j (", j, ") leaves no in-sample base rows; need j < ", n
    )
  )
  shifted <- c(rep(NA_real_, j), base_series[seq_len(n - j)]) # pad-NA lag by j

  prepare_return_data(shifted, return_df, dates, yields, "expected_sdf")
}
