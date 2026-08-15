# Tests for the Harvey log-variance worker: the start ladder, the first-order
# condition it solves, the scaled-response guards, and the zero-response rows
# it treats as first-class. The internals are unexported, so they are reached
# via hetid:::.

test_that("a clean simulated response fits end to end", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x, estimator = "harvey")

  expect_true(log_variance_fit_ok(fit))
  expect_identical(fit$fit_status, "ok")
  expect_identical(attr(fit, "estimator"), "harvey")
  expect_output(print(fit), "estimator: harvey")
  expect_named(fit$coef, c("(Intercept)", "v1", "v2"))
  expect_named(fit$warm_start, c("(Intercept)", "v1", "v2"))
  expect_equal(fit$coef[-1], c(v1 = 0.6, v2 = -0.4), tolerance = 0.25)

  # both estimators solve a moment condition for the same E[y | x], so their
  # slopes agree up to the efficiency difference on this heavy-tailed response
  ppml <- fit_log_variance(d$y, d$x)
  expect_equal(fit$coef[-1], ppml$coef[-1], tolerance = 0.2)
})

test_that("the first-order condition holds at the reported coefficients", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x, estimator = "harvey")
  x_mat <- cbind(1, d$x)
  mu <- drop(exp(x_mat %*% fit$coef))
  scaled <- abs(crossprod(x_mat, d$y / mu - 1)) / colSums(abs(x_mat))
  expect_lte(max(scaled), LOG_VARIANCE_HARVEY_CONTROL$SCORE_TOLERANCE)
  expect_lte(fit$score_norm, LOG_VARIANCE_HARVEY_CONTROL$SCORE_TOLERANCE)
})

test_that("the criterion is convex, so the start does not move the answer", {
  d <- simulate_logvar_data()
  intercept_only <- fit_log_variance(d$y, d$x, estimator = "harvey")
  far <- fit_log_variance(d$y, d$x, estimator = "harvey", start = c(5, -3, 4))

  expect_equal(far$coef, intercept_only$coef, tolerance = 1e-8)
  attempts <- far$diagnostics$start_attempts
  expect_identical(attempts[[1L]]$source, "supplied")
  expect_true(is.na(attempts[[length(attempts)]]$error_class))
})

test_that("a returned warm start is accepted without another iteration", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x, estimator = "harvey")
  again <- fit_log_variance(
    d$y, d$x,
    estimator = "harvey", start = fit$warm_start
  )
  expect_identical(again$convergence_code, 0L)
  expect_identical(again$coef, fit$coef)
})

test_that("response_scale shifts only the intercept by log(s)", {
  d <- simulate_logvar_data()
  base <- fit_log_variance(d$y, d$x, estimator = "harvey")
  scaled <- fit_log_variance(
    d$y, d$x,
    estimator = "harvey", response_scale = 1000
  )
  expect_identical(scaled$coef[[1]], scaled$warm_start[[1]] + log(1000))
  expect_equal(scaled$coef, base$coef, tolerance = 1e-8)
  expect_equal(scaled$warm_start[-1], base$warm_start[-1], tolerance = 1e-8)
})

test_that("an all-zero response fails closed and its vcov is all NA", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(rep(0, nrow(d$x)), d$x, estimator = "harvey")

  expect_false(log_variance_fit_ok(fit))
  expect_identical(fit$fit_status, "nonconvergence")
  expect_identical(fit$diagnostics$error_class, "all_zero_response")
  expect_null(fit$coef)
  expect_identical(fit$convergence_code, -1L)

  vcov_list <- compute_log_variance_vcov(fit)
  expect_named(vcov_list, LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES)
  for (v in vcov_list) {
    expect_true(all(is.na(v)))
  }
})

test_that("an interior zero response row is first-class", {
  d <- simulate_logvar_data()
  y <- d$y
  y[10] <- 0
  fit <- fit_log_variance(y, d$x, estimator = "harvey")

  expect_true(log_variance_fit_ok(fit))
  expect_identical(fit$diagnostics$n_zero_response, 1L)
  expect_true(all(is.finite(fit$coef)))
})

test_that("the ladder records an overflowing start and keeps going", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(
    d$y, d$x,
    estimator = "harvey", start = c(1e6, 1e6, 1e6)
  )
  expect_true(log_variance_fit_ok(fit))

  attempts <- fit$diagnostics$start_attempts
  expect_length(attempts, 2L)
  expect_identical(attempts[[1L]]$source, "supplied")
  expect_identical(attempts[[1L]]$error_class, "invalid_start")
  expect_identical(attempts[[2L]]$source, "intercept_only")
  expect_true(is.na(attempts[[2L]]$error_class))
})

test_that("fallback starts are tried after the supplied start fails", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(
    d$y, d$x,
    estimator = "harvey", start = c(1e6, 1e6, 1e6),
    fallback_starts = list(c(0, 0, 0))
  )
  expect_true(log_variance_fit_ok(fit))

  attempts <- fit$diagnostics$start_attempts
  expect_length(attempts, 2L)
  expect_identical(attempts[[2L]]$source, "fallback")
  expect_true(is.na(attempts[[2L]]$error_class))
})

test_that("the scaled-response guards fail closed with distinct classes", {
  d <- simulate_logvar_data()
  x_mat <- hetid:::log_variance_design(d$x)

  y_partial <- d$y
  y_partial[1L] <- 1e-315
  partial <- hetid:::harvey_fit_response(
    y_partial, x_mat,
    response_scale = 1e10
  )
  expect_identical(partial$diagnostics$error_class, "scaled_response_underflow")

  y_over <- d$y
  y_over[1L] <- 1e300
  over <- hetid:::harvey_fit_response(y_over, x_mat, response_scale = 1e-300)
  expect_identical(over$diagnostics$error_class, "scaled_response_overflow")

  # a design the Cholesky rejects leaves no Fisher direction to fall back on
  singular <- hetid:::harvey_fit_response(
    d$y, hetid:::log_variance_design(cbind(d$x, dup = d$x[, 1]))
  )
  expect_identical(singular$diagnostics$error_class, "singular_design")

  for (failed in list(partial, over, singular)) {
    expect_false(log_variance_fit_ok(failed))
    expect_identical(failed$fit_status, "nonconvergence")
    expect_null(failed$warm_start)
  }
})
