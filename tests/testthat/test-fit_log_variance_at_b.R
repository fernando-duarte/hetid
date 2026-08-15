# Tests for fit_log_variance_at_b(): the wrapper composing reduced-form
# residuals with a fixed structural b into the log-variance equation's
# response, completing the tau = 0 chain.

test_that("matches a direct call on (w1 - w2 %*% b)^2", {
  set.seed(11)
  t_obs <- 250
  w2 <- cbind(a = rnorm(t_obs), b = rnorm(t_obs))
  b <- c(0.6, -0.3)
  eta <- drop(cbind(1, w2) %*% c(-0.5, 0.4, -0.2))
  w1 <- drop(w2 %*% b) + sqrt(exp(eta)) * rnorm(t_obs)
  x <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))

  fit <- fit_log_variance_at_b(b, w1, w2, x)
  eps <- drop(w1 - w2 %*% b)
  direct <- fit_log_variance(eps^2, x)

  expect_equal(fit$coef, direct$coef, tolerance = 1e-12)
  expect_equal(fit$objective, direct$objective, tolerance = 1e-12)
  expect_equal(fit$y, eps^2, tolerance = 1e-12)
})

test_that("min_abs_eps is recorded and equals min(abs(eps))", {
  set.seed(12)
  t_obs <- 250
  w2 <- cbind(a = rnorm(t_obs), b = rnorm(t_obs))
  b <- c(0.6, -0.3)
  eta <- drop(cbind(1, w2) %*% c(-0.5, 0.4, -0.2))
  w1 <- drop(w2 %*% b) + sqrt(exp(eta)) * rnorm(t_obs)
  x <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))

  fit <- fit_log_variance_at_b(b, w1, w2, x)
  eps <- drop(w1 - w2 %*% b)
  expect_equal(fit$diagnostics$min_abs_eps, min(abs(eps)))
})

test_that("an exact-zero residual row still fits ok", {
  set.seed(13)
  t_obs <- 250
  w2 <- cbind(a = rnorm(t_obs), b = rnorm(t_obs))
  b <- c(0.6, -0.3)
  eta <- drop(cbind(1, w2) %*% c(-0.5, 0.4, -0.2))
  w1 <- drop(w2 %*% b) + sqrt(exp(eta)) * rnorm(t_obs)
  w1[1] <- drop(w2[1, , drop = FALSE] %*% b) # eps[1] is exactly zero
  x <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))

  fit <- fit_log_variance_at_b(b, w1, w2, x)
  expect_identical(min(abs(fit$y)), 0)
  expect_true(log_variance_fit_ok(fit))
})

test_that("dimension guards raise hetid_error_dimension_mismatch", {
  set.seed(14)
  t_obs <- 60
  w2 <- cbind(a = rnorm(t_obs), b = rnorm(t_obs))
  b <- c(0.6, -0.3)
  w1 <- rnorm(t_obs)
  x <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))

  expect_error(
    fit_log_variance_at_b(c(b, 0.1), w1, w2, x),
    class = "hetid_error_dimension_mismatch"
  )
  expect_error(
    fit_log_variance_at_b(b, w1[-1], w2, x),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("a permuted named b is rejected, an in-order named b works", {
  set.seed(15)
  t_obs <- 250
  w2 <- cbind(a = rnorm(t_obs), b = rnorm(t_obs))
  b_true <- c(0.6, -0.3)
  eta <- drop(cbind(1, w2) %*% c(-0.5, 0.4, -0.2))
  w1 <- drop(w2 %*% b_true) + sqrt(exp(eta)) * rnorm(t_obs)
  x <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))

  permuted <- stats::setNames(rev(b_true), rev(colnames(w2)))
  expect_error(
    fit_log_variance_at_b(permuted, w1, w2, x),
    class = "hetid_error_bad_argument"
  )

  ordered <- stats::setNames(b_true, colnames(w2))
  fit <- fit_log_variance_at_b(ordered, w1, w2, x)
  expect_true(log_variance_fit_ok(fit))
})

test_that("the tau = 0 composition end to end returns an ok fit", {
  d <- simulate_tau0_dgp()
  fit <- compute_tau0_system(d$y1, d$y2, d$x, d$z)
  expect_false(is.null(fit$point))

  logvar_fit <- fit_log_variance_at_b(fit$point$theta, fit$w1, fit$w2, d$x_var)
  expect_true(log_variance_fit_ok(logvar_fit))
})
