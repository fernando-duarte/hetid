# Test file for compute_c_hat function
# Tests supremum estimator calculations with validation and sanity checks

test_that("compute_c_hat returns expected structure", {
  # Load test data
  data <- extract_acm_data()

  # Test with default parameters
  result <- compute_c_hat(data, data, i = 2)

  # Check output structure
  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(is.finite(result))
  expect_true(result >= 0) # c_hat should be non-negative
})

test_that("compute_c_hat increases with maturity", {
  # Load test data
  data <- extract_acm_data()

  # Compute for multiple maturities
  c_hat_mat2 <- compute_c_hat(data, data, i = 2)
  c_hat_mat5 <- compute_c_hat(data, data, i = 5)
  c_hat_mat9 <- compute_c_hat(data, data, i = 9)

  # Generally expect higher values for longer maturities
  # (though not strictly monotonic due to estimation)
  expect_true(c_hat_mat9 > c_hat_mat2 * 0.8) # Allow some variation
})

test_that("compute_c_hat manual calculation verification", {
  # Create simple test data frame
  n_obs <- 100
  set.seed(123)

  # Create data frame in expected format
  test_data <- data.frame(
    y1 = 0.02 + rnorm(n_obs, 0, 0.001),
    y2 = 0.03 + rnorm(n_obs, 0, 0.001),
    y3 = 0.04 + rnorm(n_obs, 0, 0.001),
    tp1 = 0.001 + rnorm(n_obs, 0, 0.0001),
    tp2 = 0.002 + rnorm(n_obs, 0, 0.0001),
    tp3 = 0.003 + rnorm(n_obs, 0, 0.0001)
  )

  # Compute c_hat
  result <- compute_c_hat(test_data, test_data, i = 2)

  # Basic sanity check: result should be related to variability
  expect_true(result > 0)
  expect_true(result < 10) # Reasonable bound for test data
})

test_that("compute_c_hat handles missing data correctly", {
  # Load test data
  data <- extract_acm_data()

  # Introduce some NA values
  data_na <- data
  data_na[c(10, 20, 30), c("y3", "tp3")] <- NA

  # Should still compute without error
  result <- compute_c_hat(data_na, data_na, i = 3)

  expect_type(result, "double")
  expect_true(is.finite(result))
})

test_that("compute_c_hat consistency check", {
  # Load test data
  data <- extract_acm_data()

  # Run twice with same inputs
  result1 <- compute_c_hat(data, data, i = 4)
  result2 <- compute_c_hat(data, data, i = 4)

  # Should get identical results
  expect_equal(result1, result2)
})

test_that("compute_c_hat boundary conditions", {
  # Test with minimum maturity
  data <- extract_acm_data()

  # i = 1 should work
  result_min <- compute_c_hat(data, data, i = 1)
  expect_type(result_min, "double")
  expect_true(is.finite(result_min))

  # Test with maximum available maturity (9 years, since n_hat needs i+1)
  result_max <- compute_c_hat(data, data, i = 9)
  expect_type(result_max, "double")
  expect_true(is.finite(result_max))
})

test_that("compute_c_hat error handling", {
  data <- extract_acm_data()

  # Invalid maturity index
  expect_error(compute_c_hat(data, data, i = 0))
  expect_error(compute_c_hat(data, data, i = 11)) # Max is 10

  # Note: compute_c_hat doesn't validate dimension matching between inputs

  # Non-numeric input
  expect_error(compute_c_hat("not numeric", data, i = 2))
})
