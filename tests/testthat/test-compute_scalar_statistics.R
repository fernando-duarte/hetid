test_that("compute_scalar_statistics validates w1 input", {
  expect_error(
    compute_scalar_statistics("bad", matrix(1:6, 3, 2)),
    "w1 must be a numeric vector"
  )
})

test_that("compute_scalar_statistics validates w2 input", {
  expect_error(
    compute_scalar_statistics(1:3, "bad"),
    "w2 must be a matrix or data frame"
  )
})

test_that("compute_scalar_statistics returns correct structure", {
  # Create test data
  set.seed(123)
  T_obs <- 100
  I <- 3
  w1 <- rnorm(T_obs)
  w2 <- matrix(rnorm(T_obs * I), T_obs, I)

  result <- compute_scalar_statistics(w1, w2)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("s_i_0", "sigma_i_sq"))

  # Check dimensions
  expect_length(result$s_i_0, I)
  expect_length(result$sigma_i_sq, I)

  # Check names
  expect_named(result$s_i_0, paste0("maturity_", 1:I))
  expect_named(result$sigma_i_sq, paste0("maturity_", 1:I))

  # Check all values are numeric
  expect_true(all(is.numeric(result$s_i_0)))
  expect_true(all(is.numeric(result$sigma_i_sq)))
})

test_that("compute_scalar_statistics computes correct values", {
  # Simple test case with known values
  w1 <- c(1, 2, 3)
  w2 <- matrix(c(2, 3, 4, 5, 6, 7), nrow = 3, ncol = 2)

  result <- compute_scalar_statistics(w1, w2)

  # S_i^(0) is the mean of the squared Hadamard products of w1 and w2_i
  expected_s_1_0 <- sum((w1 * w2[, 1])^2) / 3
  expect_equal(unname(result$s_i_0[1]), expected_s_1_0)

  # sigma_i^2 is the variance of the squared elements of w2_i
  w2_1_sq <- w2[, 1]^2
  term1 <- sum(w2_1_sq^2) / 3
  term2 <- (sum(w2_1_sq) / 3)^2
  expected_sigma_1_sq <- term1 - term2
  expect_equal(unname(result$sigma_i_sq[1]), expected_sigma_1_sq)
})

test_that("compute_scalar_statistics handles subset of maturities", {
  set.seed(456)
  T_obs <- 50
  I <- 5
  w1 <- rnorm(T_obs)
  w2 <- matrix(rnorm(T_obs * I), T_obs, I)

  # Test with subset of maturities
  maturities <- c(2, 4)
  result <- compute_scalar_statistics(w1, w2, maturities = maturities)

  # Check only requested maturities are computed
  expect_length(result$s_i_0, length(maturities))
  expect_length(result$sigma_i_sq, length(maturities))
  expect_named(result$s_i_0, paste0("maturity_", maturities))
  expect_named(result$sigma_i_sq, paste0("maturity_", maturities))
})

test_that("compute_scalar_statistics handles edge cases", {
  # Test with single observation
  w1 <- 2
  w2 <- matrix(3, nrow = 1, ncol = 1)

  result <- compute_scalar_statistics(w1, w2)

  # With a single observation, S_1^(0) is simply (w1 * w2)^2
  expect_equal(unname(result$s_i_0[1]), 36)

  # With a single observation, variance of w2^2 is zero
  expect_equal(unname(result$sigma_i_sq[1]), 0)

  # Test with zero residuals
  w1 <- rep(0, 10)
  w2 <- matrix(rnorm(20), nrow = 10, ncol = 2)

  result <- compute_scalar_statistics(w1, w2)

  # S_i^(0) should be 0 when w1 is all zeros
  expect_equal(unname(result$s_i_0[1]), 0)
  expect_equal(unname(result$s_i_0[2]), 0)
})
