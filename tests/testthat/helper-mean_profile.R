mean_profile_fixture <- function(seed = 3L) {
  with_rng_scope(
    {
      n <- 220L
      z <- rnorm(n)
      x <- stats::prcomp(matrix(rnorm(n * 5L), n, 5L))$x[, 1:3]
      colnames(x) <- paste0("expected_sdf_pc", 1:3)
      y2 <- stats::prcomp(sqrt(exp(0.4 + 0.9 * z)) * matrix(rnorm(n * 5L), n, 5L))$x[, 1:3]
      colnames(y2) <- paste0("sdf_news_pc", 1:3)
      e1 <- rnorm(n) + 0.4 * y2[, 1L] - 0.2 * y2[, 3L]
      y <- drop(0.3 + x %*% c(0.2, -0.1, 0.4) + y2 %*% c(0.5, -0.3, 0.2) + e1)
      z <- matrix(z, ncol = 1L, dimnames = list(NULL, "z"))
      compute_tau0_system(y, y2, x, z,
        impose_null = FALSE,
        gamma = matrix(1, 1, ncol(y2)), tol = 1e-8
      )
    },
    seed = seed,
    kind = c("Mersenne-Twister", "Inversion", "Rejection")
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
