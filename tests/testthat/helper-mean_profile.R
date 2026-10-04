mean_profile_fixture <- function(seed = 3L) {
  p <- variance_share_fixture(seed)
  compute_tau0_system(p$y, p$y2, p$x, p$z,
    impose_null = FALSE,
    gamma = matrix(1, 1, ncol(p$y2)), tol = 1e-8
  )
}

mean_profile_ball <- function(dimension = 2L) {
  list(A_i = list(diag(dimension)), b_i = list(numeric(dimension)), c_i = -1)
}

# Donor endpoints that the thin-set segment repair recovers. The donor runtime
# left them unreliable (NA); the pin tests check them against verified outer
# bounds and the bounded status instead of those historical pins.
REPAIRED_DONOR_PINS <- data.frame(
  scenario = "synthetic_seed34", tau = 0.05,
  field = rep(c("beta1_upper", "share_lo", "share_hi"), each = 2L),
  row = rep(2:3, 3L), stringsAsFactors = FALSE
)

repaired_pin_rows <- function(scenario, tau, field) {
  hit <- REPAIRED_DONOR_PINS$scenario == scenario & REPAIRED_DONOR_PINS$tau %in% tau &
    REPAIRED_DONOR_PINS$field == field
  REPAIRED_DONOR_PINS$row[hit]
}

# An attained upper endpoint lies at or below the verified outer upper bound of
# its structural coefficient, and within the repair's accuracy budget of it.
expect_beta1_upper_within_outer <- function(fit, tau, rows, values) {
  quadratic <- build_quadratic_system(fit$gamma, rep(tau, ncol(fit$w2)), fit$moments)$quadratic
  outer <- with_rng_scope(
    {
      evidence <- profile_evidence(quadratic, fit$beta2r, matrix(fit$point$theta, 1L))
      fit$beta1r - evidence$outer_bounds(fit$beta2r, refine = TRUE)$lower
    },
    seed = 1L,
    kind = c("Mersenne-Twister", "Inversion", "Rejection")
  )
  gap <- unname(outer[rows] - values[rows])
  expect_true(all(is.finite(values[rows])))
  expect_true(all(gap >= 0 & gap <= HETID_CONSTANTS$PROFILE_SEGMENT_OBJECTIVE_RTOL))
}
