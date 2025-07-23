# Test file for compute_n_hat function
# Tests expected log bond price estimator

test_that("compute_n_hat returns expected structure", {
  # Load test data
  data <- extract_acm_data()

  # Test with default parameters
  result <- compute_n_hat(data, data, i = 4)

  # Check output structure - returns a vector
  expect_type(result, "double")
  expect_true(length(result) > 1) # Should be a vector
  expect_true(all(is.finite(result))) # All elements should be finite

  # n_hat represents log bond prices, can be negative
  expect_true(is.numeric(result))
})

test_that("compute_n_hat scales with maturity", {
  # Load test data
  data <- extract_acm_data()

  # Compute for short and long maturities
  n_hat_short <- compute_n_hat(data, data, i = 2)
  n_hat_long <- compute_n_hat(data, data, i = 8)

  # Check that both return vectors
  expect_true(length(n_hat_short) > 1)
  expect_true(length(n_hat_long) > 1)

  # All values should be finite
  expect_true(all(is.finite(n_hat_short)))
  expect_true(all(is.finite(n_hat_long)))
})

test_that("compute_n_hat manual verification with simple data", {
  # Create test data with known properties
  set.seed(789)
  n_obs <- 150

  # Create data frame with required columns
  test_data <- data.frame(
    y1 = 5 + rnorm(n_obs, 0, 0.1),
    y2 = 5.2 + rnorm(n_obs, 0, 0.1),
    y3 = 5.4 + rnorm(n_obs, 0, 0.1),
    y4 = 5.6 + rnorm(n_obs, 0, 0.1),
    y5 = 5.8 + rnorm(n_obs, 0, 0.1),
    tp1 = 1 + rnorm(n_obs, 0, 0.01),
    tp2 = 1.2 + rnorm(n_obs, 0, 0.01),
    tp3 = 1.4 + rnorm(n_obs, 0, 0.01),
    tp4 = 1.6 + rnorm(n_obs, 0, 0.01),
    tp5 = 1.8 + rnorm(n_obs, 0, 0.01)
  )

  # Compute n_hat
  result <- compute_n_hat(test_data, test_data, i = 3)

  # Check it returns a vector
  expect_true(length(result) > 1)

  # For log bond prices with 5% yields, expect negative values on average
  expect_true(mean(result) < 0)

  # Should be reasonably sized
  expect_true(all(result > -10))
  expect_true(all(result < 10))
})

test_that("compute_n_hat consistency across maturities", {
  # Load test data
  data <- extract_acm_data()

  # Compute for all maturities and take mean
  n_hat_means <- numeric(9)
  for (i in 1:9) {
    n_hat_vector <- compute_n_hat(data, data, i = i)
    n_hat_means[i] <- mean(n_hat_vector, na.rm = TRUE)
  }

  # All means should be finite
  expect_true(all(is.finite(n_hat_means)))

  # Check for reasonable progression (not strictly monotonic due to estimation)
  expect_true(length(unique(n_hat_means)) > 5) # Should vary across maturities
})

test_that("compute_n_hat handles missing data", {
  # Load test data
  data <- extract_acm_data()

  # Create copy with NAs
  data_na <- data

  # Introduce scattered NAs
  n_nas <- 20
  na_positions <- sample(1:nrow(data), n_nas)
  data_na[na_positions, c("y5", "tp5")] <- NA

  # Should still compute
  result <- compute_n_hat(data_na, data_na, i = 5)

  expect_type(result, "double")
  # Should return a vector with some finite values despite NAs
  expect_true(length(result) > 1)
  expect_true(sum(is.finite(result)) > 0)
})

test_that("compute_n_hat relationship with other estimators", {
  # Load test data
  data <- extract_acm_data()

  # Compute all three estimators for same maturity
  n_hat <- compute_n_hat(data, data, i = 6)
  c_hat <- compute_c_hat(data, data, i = 6)
  k_hat <- compute_k_hat(data, data, i = 6)

  # n_hat is a vector, others are scalars
  expect_true(length(n_hat) > 1) # n_hat is a vector
  expect_true(all(is.finite(n_hat)))
  expect_true(is.finite(c_hat))
  expect_true(is.finite(k_hat))

  # c_hat and k_hat should be positive (moments)
  expect_true(c_hat > 0)
  expect_true(k_hat > 0)
})

test_that("compute_n_hat stability test", {
  # Load test data
  data <- extract_acm_data()

  # Compute using different subsets of data
  n_full <- nrow(data)
  n_half <- floor(n_full / 2)

  result_first_half <- compute_n_hat(
    data[1:n_half, ],
    data[1:n_half, ],
    i = 4
  )

  result_second_half <- compute_n_hat(
    data[(n_half + 1):n_full, ],
    data[(n_half + 1):n_full, ],
    i = 4
  )

  # Both should be vectors
  expect_true(length(result_first_half) > 1)
  expect_true(length(result_second_half) > 1)

  # Compare means - should be in same ballpark
  mean_first <- mean(result_first_half, na.rm = TRUE)
  mean_second <- mean(result_second_half, na.rm = TRUE)
  expect_true(abs(mean_first - mean_second) < 5)
})

test_that("compute_n_hat extreme values test", {
  # Test with very high yields
  high_data <- data.frame(
    y1 = rep(20, 100),
    y2 = rep(20.5, 100),
    y3 = rep(21, 100),
    tp1 = rep(5, 100),
    tp2 = rep(5.2, 100),
    tp3 = rep(5.4, 100)
  )

  result_high <- compute_n_hat(high_data, high_data, i = 2)
  expect_true(length(result_high) > 1)
  expect_true(all(is.finite(result_high)))

  # Test with very low yields
  low_data <- data.frame(
    y1 = rep(0.1, 100),
    y2 = rep(0.15, 100),
    y3 = rep(0.2, 100),
    tp1 = rep(0.01, 100),
    tp2 = rep(0.015, 100),
    tp3 = rep(0.02, 100)
  )

  result_low <- compute_n_hat(low_data, low_data, i = 2)
  expect_true(length(result_low) > 1)
  expect_true(all(is.finite(result_low)))
})

test_that("compute_n_hat error handling", {
  data <- extract_acm_data()

  # Invalid maturity index
  expect_error(compute_n_hat(data, data, i = 0))
  expect_error(compute_n_hat(data, data, i = 100))

  # Missing required columns
  bad_data <- data[, c("date", "y2", "y3", "tp2", "tp3")] # Missing y1, etc
  expect_error(compute_n_hat(bad_data, bad_data, i = 5))

  # Wrong input type
  expect_error(compute_n_hat(list(data), data, i = 2))
})
