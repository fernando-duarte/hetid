test_that("interval targets require a non-NULL alpha", {
  fx <- bootstrap_fixture(20)
  for (target in c("pointwise", "containment")) {
    for (alpha in list(NULL, NA_real_, NaN, Inf, 0, 1, "0.1", c(0.1, 0.2))) {
      err <- tryCatch(
        bootstrap_set_interval(fx$full, fx$draws, target, alpha, 10, 0.85),
        error = identity
      )
      expect_s3_class(err, "hetid_error_bad_argument")
      expect_identical(err$arg, "alpha")
    }
  }
  expect_identical(
    withVisible(validate_bootstrap_gate(10, 0.85)),
    list(value = TRUE, visible = FALSE)
  )
  expect_identical(
    withVisible(validate_bootstrap_gate(10, 0.85, NULL)),
    list(value = TRUE, visible = FALSE)
  )
})

test_that("valid alpha retains endpoint ranks and calibrated intervals", {
  fx <- bootstrap_fixture(20)
  fx$full$lower <- 0
  fx$full$upper <- 1
  v <- seq(-0.3, 0.3, length.out = 20)
  fx$draws$lower[, 1] <- v
  fx$draws$upper[, 1] <- 1 + v
  for (target in c("pointwise", "containment")) {
    fit <- bootstrap_set_interval(fx$full, fx$draws, target, 0.1, 10, 0.85)
    expect_identical(fit$alpha, 0.1)
    expect_identical(fit$full, fx$full)
    expect_identical(fit$draws, fx$draws)
    expect_identical(names(fit$sides), "a")
    expect_identical(fit$summary$coef, "a")
    expect_equal(fit$summary$root_rank, 19)
    padding <- if (target == "pointwise") 0.268421052631579 else 0.3
    expect_equal(fit$summary$ci_lower, -padding, tolerance = 1e-12)
    expect_equal(fit$summary$ci_upper, 1 + padding, tolerance = 1e-12)
    expect_identical(fit$summary$reason, "reported")
  }
})
