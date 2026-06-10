test_that("compute_k_hat returns single positive value (fourth moment)", {
  # Setup standard test environment
  test_env <- setup_standard_test_env()

  # Test for maturity 5
  k_hat_5 <- compute_k_hat(test_env$yields, test_env$term_premia, i = 5)

  # Apply standard expectations for single positive value
  expect_single_finite_value(k_hat_5,
    should_be_positive = TRUE,
    label = "k_hat should be positive (fourth moment)"
  )
})

test_that("special case i=1 returns 0", {
  # Setup standard test environment
  test_env <- setup_standard_test_env()

  # For i=1, k_hat should be 0
  k_hat_1 <- compute_k_hat(test_env$yields, test_env$term_premia, i = 1)

  expect_equal(k_hat_1, 0,
    label = "k_hat should be 0 when i=1 (uses n_hat_0 = -y1)"
  )
})

test_that("k_hat manual calculation verification", {
  test_env <- setup_standard_test_env()

  # Test for i=5
  i <- 5
  k_hat_5 <- compute_k_hat(test_env$yields, test_env$term_premia, i = i)

  # Manual calculation
  n_hat <- compute_n_hat(test_env$yields, test_env$term_premia, i = i - 1)
  y <- test_env$yields$y1 / 100 # Convert to decimal

  # Get n_hat at t+1
  n_hat_t_plus_1 <- c(n_hat[-1], NA)

  # Get y at t+i
  y_t_plus_i <- c(y[-seq_len(i)], rep(NA, i))

  # Compute k_hat manually: mean over valid (non-missing) terms
  terms <- (-y_t_plus_i - n_hat_t_plus_1)^4
  k_hat_manual <- mean(terms[!is.na(terms)])

  expect_equal(k_hat_5, k_hat_manual,
    tolerance = 1e-10,
    label = "k_hat should match manual calculation"
  )
})

test_that("k_hat monotonicity - generally increases with maturity", {
  test_env <- setup_standard_test_env()

  # Compute k_hat for all maturities
  k_values <- numeric(9)
  for (i in 1:9) {
    k_values[i] <- compute_k_hat(test_env$yields, test_env$term_premia, i = i)
  }

  # Check general increasing trend (allowing for some non-monotonicity)
  # Count how many increases vs decreases
  increases <- sum(diff(k_values[2:9]) > 0) # Exclude i=1 which is 0
  expect_gte(increases, 4,
    label = "k_hat should generally increase with maturity"
  )
})

test_that("k_hat positivity - all values positive except i=1", {
  test_env <- setup_standard_test_env()

  # Test all maturities
  for (i in 1:9) {
    k_hat_i <- compute_k_hat(test_env$yields, test_env$term_premia, i = i)

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

test_that("k_hat averages over valid terms with interior NA in y1", {
  test_env <- setup_standard_test_env()
  i <- 5
  yields_na <- test_env$yields
  n <- nrow(yields_na)

  # Interior NA lands inside the shifted window and drops one term
  yields_na$y1[25] <- NA

  k_hat_na <- compute_k_hat(yields_na, test_env$term_premia, i = i)

  # Manual replication: only y1 enters as y_{t+i}, n_hat(4) is unaffected
  n_hat <- compute_n_hat(yields_na, test_env$term_premia, i = i - 1)
  y1 <- yields_na$y1 / 100
  y1_shifted <- y1[(i + 1):n]
  n_hat_shifted <- n_hat[2:(n - i + 1)]
  terms <- (-y1_shifted - n_hat_shifted)^4
  valid_terms <- terms[!is.na(terms)]

  expect_length(valid_terms, n - i - 1)
  expect_equal(k_hat_na, mean(valid_terms),
    tolerance = 1e-12,
    label = "k_hat should average over the valid terms only"
  )
  # The fixed divisor T - i (old documented formula) would understate it
  expect_gt(k_hat_na, sum(valid_terms) / (n - i))
})

test_that("k_hat time alignment matches hand-computed synthetic value", {
  # With y2 = tp1 = tp2 = 0, n_hat(1,t) reduces to y1[t] / 100, so
  # k_hat(2) = mean over t of ((-y1[t+2] - y1[t+1]) / 100)^4. The y1
  # values make consecutive-pair sums distinct, so any misalignment
  # (e.g. a shift by 2) would average different pairs
  y1_pct <- c(0, 1, 3, 6, 10, 15)
  n <- length(y1_pct)
  zeros <- numeric(n)
  yields <- data.frame(y1 = y1_pct, y2 = zeros)
  term_premia <- data.frame(tp1 = zeros, tp2 = zeros)

  k_hat_2 <- compute_k_hat(yields, term_premia, i = 2)

  aligned_sums <- y1_pct[3:n] + y1_pct[2:(n - 1)]
  expected <- mean((aligned_sums / 100)^4)

  misaligned_sums <- y1_pct[1:(n - 2)] + y1_pct[2:(n - 1)]
  misaligned <- mean((misaligned_sums / 100)^4)

  expect_false(isTRUE(all.equal(expected, misaligned)),
    label = "synthetic data must distinguish aligned from misaligned"
  )
  expect_equal(k_hat_2, expected,
    tolerance = 1e-12,
    label = "k_hat should pair y1[t+2] with n_hat(1,t+1)"
  )
})

test_that("k_hat degenerate branch returns typed numeric NA", {
  test_env <- setup_standard_test_env()
  yields_na <- test_env$yields
  yields_na$y1 <- NA_real_

  # vapply(..., numeric(1)) enforces a double return on the NA branch
  k_vals <- vapply(
    c(4, 5),
    function(i) compute_k_hat(yields_na, test_env$term_premia, i = i),
    numeric(1)
  )

  expect_type(k_vals, "double")
  expect_true(all(is.na(k_vals)))
  expect_identical(
    compute_k_hat(yields_na, test_env$term_premia, i = 5),
    NA_real_
  )
})

test_that("compute_k_hat rejects non-integer but accepts i=10", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_k_hat(test_env$yields, test_env$term_premia, i = 1.5),
    "integer"
  )
  # k_hat legitimately supports i=10 (uses n_hat(i-1), not n_hat(i))
  expect_no_error(
    compute_k_hat(test_env$yields, test_env$term_premia, i = 10)
  )
})
