# Shared DGP for the whitening conditioning fixture: correlated AR
# instruments with a near-duplicate column and z-driven
# heteroskedasticity. Sourced by BOTH the conditioning test and its
# capture script so the pinned fixture and the test can never drift
# (any divergence fails the pin loudly).
make_whiten_conditioning_data <- function() {
  set.seed(2026)
  t_obs <- 200L
  ar_series <- function(n, rho) {
    as.numeric(stats::filter(rnorm(n), rho, method = "recursive"))
  }
  z_a <- ar_series(t_obs, 0.9)
  z_b <- 0.98 * z_a + 0.02 * ar_series(t_obs, 0.9)
  z_c <- ar_series(t_obs, 0.6)
  z_corr <- cbind(z_a, z_b, z_c)
  e2 <- matrix(rnorm(t_obs * 2), ncol = 2)
  w2 <- cbind(
    e2[, 1] * sqrt(0.3 + 0.7 * z_a^2 / mean(z_a^2)),
    e2[, 2] * sqrt(0.3 + 0.7 * z_c^2 / mean(z_c^2))
  )
  w1 <- 0.6 * w2[, 1] - 0.4 * w2[, 2] + rnorm(t_obs)
  start_fix <- matrix(rnorm(6), nrow = 3)
  list(z = z_corr, w1 = w1, w2 = w2, start = start_fix)
}
