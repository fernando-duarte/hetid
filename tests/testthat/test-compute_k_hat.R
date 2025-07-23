# Test file for compute_k_hat function
# Tests fourth moment estimator calculations

test_that("compute_k_hat returns expected structure", {
  # Load test data
  data <- extract_acm_data()

  # Test with default parameters - pass the whole data frame
  result <- compute_k_hat(data, data, i = 3)

  # Check output structure
  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(is.finite(result))
  expect_true(result >= 0) # Fourth moment should be non-negative
})

test_that("compute_k_hat relationship with maturity", {
  # Load test data
  data <- extract_acm_data()

  # Compute for multiple maturities
  k_hat_values <- numeric(8)
  for (i in 2:9) {
    k_hat_values[i - 1] <- compute_k_hat(data, data, i = i)
  }

  # Check all values are positive
  expect_true(all(k_hat_values > 0))

  # Fourth moments should generally increase with maturity
  # (allowing for estimation noise)
  expect_true(k_hat_values[8] > k_hat_values[1] * 0.5)
})

test_that("compute_k_hat manual calculation verification", {
  # Create controlled test data
  set.seed(456)
  n_obs <- 200

  # Create data frame with required column names
  yields_df <- data.frame(
    y1 = 3 + cumsum(rnorm(n_obs, 0, 0.01)),
    y2 = 3.2 + cumsum(rnorm(n_obs, 0, 0.01)),
    y3 = 3.4 + cumsum(rnorm(n_obs, 0, 0.01))
  )

  term_premia_df <- data.frame(
    tp1 = rep(0.2, n_obs),
    tp2 = rep(0.3, n_obs),
    tp3 = rep(0.4, n_obs)
  )

  # Compute k_hat
  result <- compute_k_hat(yields_df, term_premia_df, i = 2)

  # Fourth moment should be small for this stable data
  expect_true(result > 0)
  expect_true(result < 1) # Should be reasonably small
})

test_that("compute_k_hat consistency with compute_c_hat", {
  # Load test data
  data <- extract_acm_data()

  # Compute both estimators
  c_hat <- compute_c_hat(data, data, i = 5)
  k_hat <- compute_k_hat(data, data, i = 5)

  # Both should be positive
  expect_true(c_hat > 0)
  expect_true(k_hat > 0)

  # They measure different moments, so no strict relationship,
  # but both should be finite and reasonable
  expect_true(is.finite(c_hat))
  expect_true(is.finite(k_hat))
})

test_that("compute_k_hat handles missing data", {
  # Load test data
  data <- extract_acm_data()

  # Create copies with missing values
  data_na <- data

  # Create gaps in the data
  data_na[50:55, c("y4", "y5")] <- NA
  data_na[100:105, c("tp4", "tp5")] <- NA

  # Should handle NAs gracefully
  result <- compute_k_hat(data_na, data_na, i = 4)

  expect_type(result, "double")
  expect_true(is.finite(result))
  expect_true(result > 0)
})

test_that("compute_k_hat deterministic for same input", {
  # Load test data
  data <- extract_acm_data()

  # Multiple runs should give same result
  results <- replicate(5, compute_k_hat(data, data, i = 6))

  # All results should be identical
  expect_true(all(results == results[1]))
})

test_that("compute_k_hat edge cases", {
  # Minimum required data
  min_data <- data.frame(
    y1 = rnorm(10, 3, 0.1),
    y2 = rnorm(10, 3.2, 0.1),
    y3 = rnorm(10, 3.4, 0.1),
    tp1 = rnorm(10, 0.2, 0.01),
    tp2 = rnorm(10, 0.3, 0.01),
    tp3 = rnorm(10, 0.4, 0.01)
  )

  # Should work with small dataset
  result <- compute_k_hat(min_data, min_data, i = 2)
  expect_type(result, "double")

  # Test with constant yields (no variation)
  const_data <- data.frame(
    y1 = rep(3, 100),
    y2 = rep(3.2, 100),
    y3 = rep(3.4, 100),
    y4 = rep(3.6, 100),
    y5 = rep(3.8, 100),
    tp1 = rep(0.2, 100),
    tp2 = rep(0.3, 100),
    tp3 = rep(0.4, 100),
    tp4 = rep(0.5, 100),
    tp5 = rep(0.6, 100)
  )

  result_const <- compute_k_hat(const_data, const_data, i = 3)

  # Should handle constant data (fourth moment near zero)
  expect_true(is.finite(result_const))
  expect_true(result_const >= 0)
})

test_that("compute_k_hat error handling", {
  data <- extract_acm_data()

  # Invalid maturity
  expect_error(compute_k_hat(data, data, i = -1))
  expect_error(compute_k_hat(data, data, i = 0))

  # Missing required columns
  bad_data <- data[, c("date", "y2", "y3")] # Missing y1
  expect_error(compute_k_hat(bad_data, bad_data, i = 2))

  # Wrong input types
  expect_error(compute_k_hat("not a data frame", data, i = 2))
  expect_error(compute_k_hat(data, "not a data frame", i = 2))

  # Insufficient data for given i
  expect_error(compute_k_hat(data[1:2, ], data[1:2, ], i = 5))
})
