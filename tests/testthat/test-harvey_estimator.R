# Tests for the Harvey estimator as the registry sees it: the spec entry, the
# covariance and standard-error boundaries keyed by its own SE types, and the
# two wrappers that pass an estimator id through to the worker.

test_that("the registry carries the harvey spec", {
  spec <- hetid:::log_variance_estimator("harvey")
  expect_identical(spec$id, "harvey")
  expect_identical(spec$se_types, LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES)
  expect_true(is.function(spec$fit_response))
  expect_true(is.function(spec$vcov))
})

test_that("the SE boundaries are keyed by the harvey variants", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x, estimator = "harvey")
  labels <- attr(fit, "coef_labels")

  vcov_list <- compute_log_variance_vcov(fit)
  expect_named(vcov_list, LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES)
  for (v in vcov_list) {
    expect_identical(dimnames(v), list(labels, labels))
    expect_true(all(is.finite(v)))
  }

  se <- compute_log_variance_se(fit)
  expect_identical(
    names(se),
    c("term", "coef", LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES)
  )
  expect_identical(se$term, labels)
  expect_true(all(se$expected > 0))
})

test_that("fit_log_variance_at_b passes the estimator through", {
  d <- simulate_tau0_dgp()
  system <- compute_tau0_system(d$y1, d$y2, d$x, d$z)
  fit <- fit_log_variance_at_b(
    system$point$theta, system$w1, system$w2, d$x_var,
    estimator = "harvey"
  )

  expect_true(log_variance_fit_ok(fit))
  expect_identical(attr(fit, "estimator"), "harvey")
  expect_named(fit$coef, c("(Intercept)", "v1", "v2"))
  eps <- drop(system$w1 - system$w2 %*% system$point$theta)
  expect_identical(fit$diagnostics$min_abs_eps, min(abs(eps)))
})

test_that("profile_log_variance_set runs on the harvey estimator", {
  d <- simulate_box_dgp()
  box <- compute_identified_set_box(
    compute_tau0_system(d$y1, d$y2, d$x, d$z),
    tau = 0.05, n_grid = 11L
  )
  profile <- profile_log_variance_set(box, d$x_var, estimator = "harvey")

  expect_identical(profile$term, c("(Intercept)", "v1", "v2"))
  expect_identical(attr(profile, "estimator"), "harvey")
  expect_identical(attr(profile, "n_failed"), 0L)
  expect_gt(attr(profile, "n_attempted"), 0L)
  expect_true(all(profile$lower <= profile$upper))

  direct <- fit_log_variance_at_b(
    box$arg_lower[1L, ], box$w1, box$w2, d$x_var,
    estimator = "harvey"
  )
  expect_true(all(direct$coef >= profile$lower - 1e-10))
  expect_true(all(direct$coef <= profile$upper + 1e-10))
})
