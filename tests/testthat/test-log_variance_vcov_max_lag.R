test_that("the existing fit API supports its largest accepted HAC lag", {
  d <- covariance_fixture()
  for (est in c("ppml", "harvey")) {
    fit <- fit_log_variance(d$y, d$x[, "v", drop = FALSE], estimator = est)
    expect_true(log_variance_fit_ok(fit))
    expected <- covariance_oracle(
      list(coef = fit$coef, y = fit$y, x = fit$x_design),
      est, as.double(.Machine$integer.max)
    )
    expect_warning(
      actual <- compute_log_variance_vcov(fit, hac_lags = .Machine$integer.max),
      NA
    )
    expect_identical(names(actual), names(expected))
    other <- setdiff(names(actual), "hac")
    expect_equal(actual[other], expected[other], tolerance = 1e-12)
    expect_identical(attributes(actual$hac), attributes(expected$hac))
    expect_true(all(is.finite(actual$hac)))
    # At a fitted coefficient this HAC matrix is a first-order-condition
    # residual; compare the different accumulation orders on an absolute scale
    expect_lt(max(abs(actual$hac - expected$hac)), 1e-14)
  }
})
