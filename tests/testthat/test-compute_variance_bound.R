test_that("compute_variance_bound returns single positive value", {
  test_env <- setup_standard_test_env()

  # Test for maturity 60
  var_bound_60 <- compute_variance_bound(test_env$yields, test_env$term_premia, i = 60)

  expect_type(var_bound_60, "double")
  expect_length(var_bound_60, 1)
  expect_true(is.finite(var_bound_60))
  expect_gt(var_bound_60, 0, label = "Variance bound should be positive")
})

test_that("one-period bound (i=12) is the strictly positive k2 term", {
  test_env <- setup_standard_test_env()

  # At the one-period maturity i = step the k_hat (k1) term is 0, but the
  # bound is the strictly positive k2_hat contribution (spec U_step).
  var_bound_12 <- compute_variance_bound(test_env$yields, test_env$term_premia, i = 12)
  c_hat_12 <- compute_c_hat(test_env$yields, test_env$term_premia, i = 12)
  k_hat_12 <- compute_k_hat(test_env$yields, test_env$term_premia, i = 12)
  k2_hat_12 <- compute_k2_hat(test_env$yields, test_env$term_premia, i = 12)

  expect_equal(k_hat_12, 0, label = "k1 is zero at the one-period maturity")
  expect_gt(var_bound_12, 0, label = "one-period bound is positive via k2")
  expect_equal(var_bound_12, 0.25 * c_hat_12 * k2_hat_12, tolerance = 1e-12)
})

test_that("variance bound is positive across the annual nodes", {
  test_env <- setup_standard_test_env()

  # Every node, including the one-period boundary i = 12, has a strictly
  # positive bound (the boundary is carried entirely by the k2 term).
  for (i in seq(12, 108, by = 12)) {
    var_bound_i <- compute_variance_bound(test_env$yields, test_env$term_premia, i = i)
    expect_gt(var_bound_i, 0,
      label = paste("Variance bound should be positive for i =", i)
    )
  }
})

test_that("variance bound formula verification", {
  test_env <- setup_standard_test_env()

  # Test for i=48
  i <- 48
  var_bound_48 <- compute_variance_bound(test_env$yields, test_env$term_premia, i = i)

  # Compute components
  c_hat_48 <- compute_c_hat(test_env$yields, test_env$term_premia, i = i)
  k_hat_48 <- compute_k_hat(test_env$yields, test_env$term_premia, i = i)
  k2_hat_48 <- compute_k2_hat(test_env$yields, test_env$term_premia, i = i)

  # Verify formula: variance_bound = 0.25 * c_hat * (k_hat + k2_hat)
  expected_bound <- 0.25 * c_hat_48 * (k_hat_48 + k2_hat_48)

  expect_equal(var_bound_48, expected_bound,
    tolerance = 1e-10,
    label = "Variance bound should equal 0.25 * c_hat * (k_hat + k2_hat)"
  )
})

test_that("variance bound generally increases with maturity", {
  test_env <- setup_standard_test_env()

  # Compute variance bounds for the annual nodes
  maturities <- seq(12, 108, by = 12)
  var_bounds <- numeric(length(maturities))
  for (k in seq_along(maturities)) {
    var_bounds[k] <- compute_variance_bound(
      test_env$yields, test_env$term_premia,
      i = maturities[k]
    )
  }

  # Check general increasing trend (allowing for some non-monotonicity)
  # Count how many increases vs decreases
  increases <- sum(diff(var_bounds[2:9]) > 0) # Compare the non-boundary nodes
  expect_gte(increases, 4,
    label = "Variance bound should generally increase with maturity"
  )
})

test_that("compute_variance_bound rejects mismatched yields and term_premia rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  expect_error(
    compute_variance_bound(syn_long$yields, syn_short$term_premia, i = 60),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("compute_variance_bound rejects invalid maturity values", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_variance_bound(test_env$yields, test_env$term_premia, i = 1.5),
    "integer"
  )
  expect_error(
    compute_variance_bound(test_env$yields, test_env$term_premia, i = 120),
    "between"
  )
})

test_that("compute_variance_bound accepts a user envelope c_bar (U^bd)", {
  test_env <- setup_standard_test_env()
  i <- 48

  k_hat <- compute_k_hat(test_env$yields, test_env$term_premia, i = i)
  k2_hat <- compute_k2_hat(test_env$yields, test_env$term_premia, i = i)

  c_bar <- 1.05
  vb_bd <- compute_variance_bound(test_env$yields, test_env$term_premia,
    i = i, c_bar = c_bar
  )
  expect_equal(vb_bd, 0.25 * c_bar * (k_hat + k2_hat),
    tolerance = 1e-12,
    label = "U^bd should use the supplied deterministic envelope"
  )

  # The supplied envelope differs from the sample-max default
  vb_default <- compute_variance_bound(test_env$yields, test_env$term_premia, i = i)
  expect_false(isTRUE(all.equal(vb_bd, vb_default)))
})

test_that("compute_variance_bound rejects an invalid c_bar", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_variance_bound(test_env$yields, test_env$term_premia, i = 48, c_bar = -1),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_variance_bound(
      test_env$yields, test_env$term_premia,
      i = 48, c_bar = c(1, 2)
    ),
    class = "hetid_error_bad_argument"
  )
})
