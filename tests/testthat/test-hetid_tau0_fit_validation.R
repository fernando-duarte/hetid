# validate_tau0_fit_point must read beta1 exactly; $ partial-matches beta1r once the key is gone

test_that("a deleted beta1 key fails when the fit has a point", {
  d <- simulate_tau0_dgp()
  fit <- compute_tau0_system(d$y1, d$y2, d$x, d$z)
  fit$beta1 <- NULL
  expect_error(
    validate_hetid_tau0_fit(fit), "beta1 must be provided",
    class = "hetid_error_bad_argument"
  )
})

test_that("a deleted beta1 key passes when the fit has no point", {
  d <- simulate_tau0_dgp()
  fit <- compute_tau0_system(d$y1, d$y2, d$x, d$z)
  fit["point"] <- list(NULL)
  fit$beta1 <- NULL
  expect_identical(validate_hetid_tau0_fit(fit), fit)
})
