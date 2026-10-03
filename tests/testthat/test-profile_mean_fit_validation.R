test_that("mean wrappers require a current unique point and reject stale anchors", {
  fit <- mean_profile_fixture()
  subset_fit <- fit
  subset_fit$moments <- compute_identification_moments(fit$w1, fit$w2, fit$z,
    maturities = c(1L, 2L)
  )
  components <- compute_identified_set_components(subset_fit$gamma, subset_fit$moments)
  expect_null(compute_tau0_point(components, tol = attr(fit, "tol")))
  stale_fit <- fit
  stale_fit$point$theta <- stale_fit$point$theta + 0.01
  for (bad in list(subset_fit, stale_fit)) {
    expect_invisible(validate_box_fit(bad))
    expect_error(profile_mean_tau_path(bad, 0.1), class = "hetid_error_bad_argument")
    expect_error(find_mean_tau_star(bad), class = "hetid_error_bad_argument")
  }
  expect_invisible(validate_profile_fit(fit))
  near <- fit
  near$point$theta[1] <- near$point$theta[1] + attr(fit, "tol") / 2
  expect_invisible(validate_profile_fit(near))
})
