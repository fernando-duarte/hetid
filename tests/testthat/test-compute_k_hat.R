test_that("compute_k_hat returns single positive value (fourth moment)", {
  # Setup standard test environment
  test_env <- setup_standard_test_env()

  # Test for maturity 60
  k_hat_60 <- compute_k_hat(test_env$yields, test_env$term_premia, i = 60)

  # Apply standard expectations for single positive value
  expect_single_finite_value(k_hat_60,
    should_be_positive = TRUE,
    label = "k_hat should be positive (fourth moment)"
  )
})

test_that("special case i=12 returns 0", {
  # Setup standard test environment
  test_env <- setup_standard_test_env()

  # For i=12, k_hat should be 0
  k_hat_12 <- compute_k_hat(test_env$yields, test_env$term_premia, i = 12)

  expect_equal(k_hat_12, 0,
    label = "k_hat should be 0 when i=12 (uses n_hat_0 = -y12)"
  )
})

test_that("k_hat manual calculation verification", {
  test_env <- setup_standard_test_env()

  # Test for i=60
  i <- 60
  k_hat_60 <- compute_k_hat(test_env$yields, test_env$term_premia, i = i)

  # Manual calculation
  n_hat <- compute_n_hat(test_env$yields, test_env$term_premia, i = i - 12)
  y <- test_env$yields$y12 / 100 # Convert to decimal

  # The realized-vs-forecast pairing shifts i/12 news periods (rows)
  horizon <- i %/% 12

  # Get n_hat at t+1
  n_hat_t_plus_1 <- c(n_hat[-1], NA)

  # Get y at t+horizon
  y_t_plus_h <- c(y[-seq_len(horizon)], rep(NA, horizon))

  # Compute k_hat manually: mean over valid (non-missing) terms
  terms <- (-y_t_plus_h - n_hat_t_plus_1)^4
  k_hat_manual <- mean(terms[!is.na(terms)])

  expect_equal(k_hat_60, k_hat_manual,
    tolerance = 1e-10,
    label = "k_hat should match manual calculation"
  )
})

test_that("k_hat monotonicity - generally increases with maturity", {
  test_env <- setup_standard_test_env()

  # Compute k_hat for the annual nodes
  maturities <- seq(12, 108, by = 12)
  k_values <- numeric(length(maturities))
  for (k in seq_along(maturities)) {
    k_values[k] <- compute_k_hat(
      test_env$yields, test_env$term_premia,
      i = maturities[k]
    )
  }

  # Check general increasing trend (allowing for some non-monotonicity)
  # Count how many increases vs decreases
  increases <- sum(diff(k_values[2:9]) > 0) # Exclude i=12 which is 0
  expect_gte(increases, 4,
    label = "k_hat should generally increase with maturity"
  )
})

test_that("k_hat positivity - all values positive except i=12", {
  test_env <- setup_standard_test_env()

  # Test all annual-node maturities
  for (i in seq(12, 108, by = 12)) {
    k_hat_i <- compute_k_hat(test_env$yields, test_env$term_premia, i = i)

    if (i == 12) {
      expect_equal(k_hat_i, 0,
        label = "k_hat should be 0 for i=12"
      )
    } else {
      expect_gt(k_hat_i, 0,
        label = paste("k_hat should be positive for i =", i)
      )
    }
  }
})

test_that("k_hat averages over valid terms with interior NA in y12", {
  test_env <- setup_standard_test_env()
  i <- 60
  yields_na <- test_env$yields
  n <- nrow(yields_na)

  # Interior NA lands inside the shifted window and drops one term
  yields_na$y12[25] <- NA

  k_hat_na <- compute_k_hat(yields_na, test_env$term_premia, i = i)

  # Manual replication: only y12 enters as y_{t+h}, n_hat(48) is
  # unaffected; the pairing shifts h = i/12 news periods (rows)
  horizon <- i %/% 12
  n_hat <- compute_n_hat(yields_na, test_env$term_premia, i = i - 12)
  y12 <- yields_na$y12 / 100
  y12_shifted <- y12[(horizon + 1):n]
  n_hat_shifted <- n_hat[2:(n - horizon + 1)]
  terms <- (-y12_shifted - n_hat_shifted)^4
  valid_terms <- terms[!is.na(terms)]

  expect_length(valid_terms, n - horizon - 1)
  expect_equal(k_hat_na, mean(valid_terms),
    tolerance = 1e-12,
    label = "k_hat should average over the valid terms only"
  )
  # The fixed divisor T - h (old documented formula) would understate it
  expect_gt(k_hat_na, sum(valid_terms) / (n - horizon))
})

test_that("k_hat time alignment matches hand-computed synthetic value", {
  # With y24 = tp12 = tp24 = 0, n_hat(12,t) reduces to y12[t] / 100, so
  # k_hat(24) = mean over t of ((-y12[t+2] - y12[t+1]) / 100)^4. The y12
  # values make consecutive-pair sums distinct, so any misalignment
  # (e.g. a shift by 2) would average different pairs
  y12_pct <- c(0, 1, 3, 6, 10, 15)
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros)

  k_hat_24 <- compute_k_hat(yields, term_premia, i = 24)

  aligned_sums <- y12_pct[3:n] + y12_pct[2:(n - 1)]
  expected <- mean((aligned_sums / 100)^4)

  misaligned_sums <- y12_pct[1:(n - 2)] + y12_pct[2:(n - 1)]
  misaligned <- mean((misaligned_sums / 100)^4)

  expect_false(isTRUE(all.equal(expected, misaligned)),
    label = "synthetic data must distinguish aligned from misaligned"
  )
  expect_equal(k_hat_24, expected,
    tolerance = 1e-12,
    label = "k_hat should pair y12[t+2] with n_hat(12,t+1)"
  )
})

test_that("k_hat degenerate branch returns typed numeric NA", {
  test_env <- setup_standard_test_env()
  yields_na <- test_env$yields
  yields_na$y12 <- NA_real_

  # vapply(..., numeric(1)) enforces a double return on the NA branch
  k_vals <- vapply(
    c(48, 60),
    function(i) compute_k_hat(yields_na, test_env$term_premia, i = i),
    numeric(1)
  )

  expect_type(k_vals, "double")
  expect_true(all(is.na(k_vals)))
  expect_identical(
    compute_k_hat(yields_na, test_env$term_premia, i = 60),
    NA_real_
  )
})

test_that("compute_k_hat rejects mismatched yields and term_premia rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  expect_error(
    compute_k_hat(syn_long$yields, syn_short$term_premia, i = 60),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("compute_k_hat rejects non-integer but accepts i=120", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_k_hat(test_env$yields, test_env$term_premia, i = 1.5),
    "integer"
  )
  # k_hat legitimately supports i=120 (uses n_hat(i-12), not n_hat(i))
  expect_no_error(
    compute_k_hat(test_env$yields, test_env$term_premia, i = 120)
  )
})
