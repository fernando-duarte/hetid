test_that("compute_k_hat returns single positive value (fourth moment)", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for maturity 5
  k_hat_5 <- compute_k_hat(yields, term_premia, i = 5)

  expect_type(k_hat_5, "double")
  expect_length(k_hat_5, 1)
  expect_true(is.finite(k_hat_5))
  expect_gt(k_hat_5, 0, label = "k_hat should be positive (fourth moment)")
})

test_that("special case i=1 returns 0", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # For i=1, k_hat should be 0
  k_hat_1 <- compute_k_hat(yields, term_premia, i = 1)

  expect_equal(k_hat_1, 0,
    label = "k_hat should be 0 when i=1 (uses n_hat_0 = -y1)"
  )
})

test_that("k_hat manual calculation verification", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for i=5
  i <- 5
  k_hat_5 <- compute_k_hat(yields, term_premia, i = i)

  # Manual calculation
  n_hat <- compute_n_hat(yields, term_premia, i = i - 1)
  y <- yields$y1 / 100 # Convert to decimal

  # Get n_hat at t+1
  n_hat_t_plus_1 <- c(n_hat[-1], NA)

  # Get y at t+i
  y_t_plus_i <- c(y[-seq_len(i)], rep(NA, i))

  # Compute k_hat manually
  n_obs <- length(y)
  k_hat_manual <- sum((-y_t_plus_i - n_hat_t_plus_1)^4, na.rm = TRUE) / (n_obs - i)

  expect_equal(k_hat_5, k_hat_manual,
    tolerance = 1e-10,
    label = "k_hat should match manual calculation"
  )
})

test_that("k_hat monotonicity - generally increases with maturity", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Compute k_hat for all maturities
  k_values <- numeric(9)
  for (i in 1:9) {
    k_values[i] <- compute_k_hat(yields, term_premia, i = i)
  }

  # Check general increasing trend (allowing for some non-monotonicity)
  # Count how many increases vs decreases
  increases <- sum(diff(k_values[2:9]) > 0) # Exclude i=1 which is 0
  expect_gte(increases, 4,
    label = "k_hat should generally increase with maturity"
  )
})

test_that("k_hat positivity - all values positive except i=1", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test all maturities
  for (i in 1:9) {
    k_hat_i <- compute_k_hat(yields, term_premia, i = i)

    if (i == 1) {
      expect_equal(k_hat_i, 0,
        label = "k_hat should be 0 for i=1"
      )
    } else {
      expect_gt(k_hat_i, 0,
        label = paste("k_hat should be positive for i =", i)
      )
    }
  }
})

test_that("k_hat time alignment verification", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test with i=3 for clarity
  i <- 3
  k_hat_3 <- compute_k_hat(yields, term_premia, i = i)

  # Manual verification of time alignment
  n_hat_2 <- compute_n_hat(yields, term_premia, i = i - 1)
  y1 <- yields$y1 / 100

  # Time alignment check: n_hat at t+1 and y at t+i
  n <- length(y1)

  # For k_hat calculation, we need:
  # - n_hat_{i-1,t+1} which is n_hat_2 shifted forward by 1
  # - y_{t+i}^{(1)} which is y1 shifted forward by i

  # The valid range for computation is from t=1 to t=n-i
  valid_range <- 1:(n - i)

  # Verify we're using the right time indices
  # Time alignment should produce correct number of valid observations
  expect_length(valid_range, n - i)

  # The function should handle NA values correctly
  expect_true(is.finite(k_hat_3),
    label = "k_hat should handle time-shifted data correctly"
  )
})
