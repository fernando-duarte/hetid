# Manual reconstructions of the expected-SDF bound ingredients, shared by the
# contract and edge test files for compute_expected_sdf_variance_bound (and by
# the news q-bound tests). All mirror the implementation's construction: the
# paired set T_i, the is.finite(gap) mask, and divisor-N moments.

# Gap series e^{-y^(1)_{t+s}} - e^{n_hat(i,t)} over T_i, finite-masked
gap_series <- function(yields, term_premia, i,
                       step = HETID_CONSTANTS$DEFAULT_STEP) {
  n_hat <- n_hat_series(yields, term_premia, i, step = step)
  y_step <- yields[[acm_column_name("yields", step)]]
  s <- i %/% step
  n_obs <- length(n_hat)
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  exp_n_hat_paired <- exp(n_hat[seq_len(n_obs - s)])
  realized <- exp(-m_step * y_step[seq.int(s + 1L, n_obs)] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL)
  g <- realized - exp_n_hat_paired
  g[is.finite(g)]
}

# q_t = e^{a}(e^u - 1 - u) (u = x - a), on the gap's finite mask
q_series <- function(yields, term_premia, i,
                     step = HETID_CONSTANTS$DEFAULT_STEP) {
  n_hat <- n_hat_series(yields, term_premia, i, step = step)
  y_step <- yields[[acm_column_name("yields", step)]]
  s <- i %/% step
  n_obs <- length(n_hat)
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  a <- n_hat[seq_len(n_obs - s)]
  x <- -m_step * y_step[seq.int(s + 1L, n_obs)] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL
  g <- exp(x) - exp(a)
  q_gap <- exp(a) * (expm1(x - a) - (x - a)) # stable via expm1
  q_gap[is.finite(g)] # same common mask as the gap, matching the implementation
}

# Fourth-order component arm (1/4) * max(e^{2a}) * mean(u^4) on the gap mask
component_arm <- function(yields, term_premia, i,
                          step = HETID_CONSTANTS$DEFAULT_STEP) {
  n_hat <- n_hat_series(yields, term_premia, i, step = step)
  y_step <- yields[[acm_column_name("yields", step)]]
  s <- i %/% step
  n_obs <- length(n_hat)
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  a <- n_hat[seq_len(n_obs - s)]
  x <- -m_step * y_step[seq.int(s + 1L, n_obs)] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL
  g <- exp(x) - exp(a)
  keep <- is.finite(g)
  u <- (x - a)[keep]
  0.25 * max(exp(2 * a[keep])) * mean(u^4)
}

var_n <- function(z) sum((z - mean(z))^2) / length(z) # divisor-N variance

sd_n <- function(z) sqrt(max(0, var_n(z))) # clamped divisor-N standard deviation

# Manual spec reconstruction of the news q-bound: primitive-masked legs over
# T_i, q kernels on both forecasts, third-order news gap, Minkowski sum
news_q_manual <- function(yields, term_premia, i,
                          step = HETID_CONSTANTS$DEFAULT_STEP) {
  n_hat_0_full <- n_hat_series(yields, term_premia, i, step = step)
  n_hat_prev_full <- compute_n_hat_previous(yields, term_premia, i, step = step)
  y_step <- yields[[acm_column_name("yields", step)]]
  s <- i %/% step
  n_obs <- length(n_hat_0_full)
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  idx <- seq_len(n_obs - s)
  n0 <- n_hat_0_full[idx]
  n1 <- n_hat_prev_full[idx + 1L]
  x <- -m_step * y_step[idx + s] / HETID_CONSTANTS$PERCENT_TO_DECIMAL
  keep <- is.finite(n0) & is.finite(n1) & is.finite(x)
  n0 <- n0[keep]
  n1 <- n1[keep]
  x <- x[keep]
  q0 <- exp(n0) * (expm1(x - n0) - (x - n0))
  q1 <- exp(n1) * (expm1(x - n1) - (x - n1))
  d <- n1 - n0
  g <- exp(n0) * (expm1(d) - d - d^2 / 2)
  (sd_n(q1) + sd_n(q0) + sd_n(g))^2
}

# The reported bound, reconstructed: min{component, var_N(q)}
esdf_bound_manual <- function(yields, term_premia, i,
                              step = HETID_CONSTANTS$DEFAULT_STEP) {
  min(
    component_arm(yields, term_premia, i, step = step),
    var_n(q_series(yields, term_premia, i, step = step))
  )
}
