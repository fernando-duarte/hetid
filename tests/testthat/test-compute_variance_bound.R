test_that("compute_variance_bound returns single positive value", {
  test_env <- setup_standard_test_env()

  # Test for maturity 60
  var_bound_60 <- compute_variance_bound(test_env$yields, test_env$term_premia, i = 60)

  expect_type(var_bound_60, "double")
  expect_length(var_bound_60, 1)
  expect_true(is.finite(var_bound_60))
  expect_gt(var_bound_60, 0, label = "Variance bound should be positive")
})

test_that("special case i=12 returns exactly 0", {
  test_env <- setup_standard_test_env()

  # For i=12, variance bound should be exactly 0
  var_bound_12 <- compute_variance_bound(test_env$yields, test_env$term_premia, i = 12)

  expect_equal(var_bound_12, 0,
    label = "Variance bound should be 0 when i=12"
  )
})

test_that("variance bound is positive for all i>12", {
  test_env <- setup_standard_test_env()

  # Test across the annual nodes (including maturity 12)
  for (i in seq(12, 108, by = 12)) {
    var_bound_i <- compute_variance_bound(test_env$yields, test_env$term_premia, i = i)

    if (i == 12) {
      # For maturity 12, variance bound should be 0
      expect_equal(var_bound_i, 0,
        tolerance = 1e-10,
        label = "Variance bound should be 0 for i = 12"
      )
    } else {
      expect_gt(var_bound_i, 0,
        label = paste("Variance bound should be positive for i =", i)
      )
    }
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

  # Verify formula: variance_bound = 0.25 * c_hat * k_hat
  expected_bound <- 0.25 * c_hat_48 * k_hat_48

  expect_equal(var_bound_48, expected_bound,
    tolerance = 1e-10,
    label = "Variance bound should equal 0.25 * c_hat * k_hat"
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
  increases <- sum(diff(var_bounds[2:9]) > 0) # Exclude i=12 which is 0
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
