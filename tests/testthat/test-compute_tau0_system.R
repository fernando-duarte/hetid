# Tests for compute_tau0_system: the tau = 0 mean-equation orchestrator
# (simulate_tau0_dgp lives in helper-tau0.R, shared across tasks)

test_that("reduced forms match lm oracles and the point composes", {
  d <- simulate_tau0_dgp()
  fit <- compute_tau0_system(d$y1, d$y2, d$x, d$z)
  oracle1 <- lm(d$y1 ~ d$x)
  expect_equal(unname(fit$w1), unname(residuals(oracle1)), tolerance = 1e-10)
  oracle2 <- lm(d$y2 ~ d$x)
  expect_equal(unname(fit$w2), unname(residuals(oracle2)), tolerance = 1e-10)
  moments <- compute_identification_moments(fit$w1, fit$w2, fit$z)
  comp <- compute_identified_set_components(fit$gamma, moments)
  expect_equal(fit$point, compute_tau0_point(comp), tolerance = 1e-12)
  expect_equal(fit$point$theta, d$theta_true, tolerance = 0.2)
})

test_that("beta1 equals the direct OLS of y1 - y2 theta on x", {
  d <- simulate_tau0_dgp()
  fit <- compute_tau0_system(d$y1, d$y2, d$x, d$z)
  direct <- coef(lm(I(d$y1 - drop(d$y2 %*% fit$point$theta)) ~ d$x))
  expect_equal(unname(fit$beta1), unname(direct), tolerance = 1e-10)
})

test_that("impose_null freezes the second reduced form", {
  d <- simulate_tau0_dgp()
  fit <- compute_tau0_system(d$y1, d$y2, d$x, d$z, impose_null = TRUE)
  expect_identical(fit$w2, d$y2)
  expect_true(all(fit$beta2r == 0))
  expect_identical(rownames(fit$beta2r), colnames(d$y2))
})

test_that("misalignment and NAs are structured errors", {
  d <- simulate_tau0_dgp()
  expect_error(compute_tau0_system(d$y1[-1], d$y2, d$x, d$z),
    class = "hetid_error_dimension_mismatch"
  )
  y1_na <- d$y1
  y1_na[5] <- NA
  expect_error(compute_tau0_system(y1_na, d$y2, d$x, d$z),
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_tau0_system(d$y1, d$y2, d$x, d$z, impose_null = NA),
    class = "hetid_error_bad_argument"
  )
})

test_that("multi-instrument z requires explicit gamma, then composes", {
  d <- simulate_tau0_dgp()
  z2 <- cbind(z1 = d$z, z2 = 0.3 * d$z + rnorm(length(d$z), sd = 0.5))
  expect_error(compute_tau0_system(d$y1, d$y2, d$x, z2),
    class = "hetid_error_bad_argument"
  )
  fit <- compute_tau0_system(d$y1, d$y2, d$x, z2, gamma = matrix(1, 2, 2))
  expect_identical(dim(fit$gamma), c(2L, 2L))
  expect_equal(fit$point$theta, d$theta_true, tolerance = 0.2)
  # a named gamma with permuted labels silently changes the estimand
  g_perm <- matrix(1, 2, 2,
    dimnames = list(c("z2", "z1"), colnames(d$y2))
  )
  expect_error(compute_tau0_system(d$y1, d$y2, d$x, z2, gamma = g_perm),
    class = "hetid_error_bad_argument"
  )
})

test_that("a rank-deficient tau=0 system yields a NULL point, not an error", {
  d <- simulate_tau0_dgp()
  # duplicating a y2 column makes the stacked Q system rank-deficient
  y2_dup <- cbind(d$y2, dup = d$y2[, 1])
  fit <- compute_tau0_system(d$y1, y2_dup, d$x, d$z)
  expect_null(fit$point)
  expect_null(fit$beta1)
  expect_output(print(fit), "no tau=0 point", fixed = TRUE)
})

test_that("x contract violations error loudly", {
  d <- simulate_tau0_dgp()
  # a constant column aliases the internally added intercept
  expect_error(compute_tau0_system(d$y1, d$y2, cbind(d$x, one = 1), d$z),
    class = "hetid_error"
  )
  # a regressor named y collides with the response inside run_pc_regression
  x_y <- d$x
  colnames(x_y) <- c("y", "x2")
  expect_error(compute_tau0_system(d$y1, d$y2, x_y, d$z),
    class = "hetid_error_bad_argument"
  )
})
