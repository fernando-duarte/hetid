test_that("compute_variance_bound returns single positive value", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for maturity 5
  var_bound_5 <- compute_variance_bound(yields, term_premia, i = 5)

  expect_type(var_bound_5, "double")
  expect_length(var_bound_5, 1)
  expect_true(is.finite(var_bound_5))
  expect_gt(var_bound_5, 0, label = "Variance bound should be positive")
})

test_that("special case i=1 returns exactly 0", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # For i=1, variance bound should be exactly 0
  var_bound_1 <- compute_variance_bound(yields, term_premia, i = 1)

  expect_equal(var_bound_1, 0,
    label = "Variance bound should be 0 when i=1"
  )
})

test_that("variance bound is positive for all i>1", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test all maturities (including maturity 1)
  for (i in 1:9) {
    var_bound_i <- compute_variance_bound(yields, term_premia, i = i)

    if (i == 1) {
      # For maturity 1, variance bound should be 0
      expect_equal(var_bound_i, 0,
        tolerance = 1e-10,
        label = "Variance bound should be 0 for i = 1"
      )
    } else {
      expect_gt(var_bound_i, 0,
        label = paste("Variance bound should be positive for i =", i)
      )
    }
  }
})

test_that("variance bound formula verification", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for i=4
  i <- 4
  var_bound_4 <- compute_variance_bound(yields, term_premia, i = i)

  # Compute components
  c_hat_4 <- compute_c_hat(yields, term_premia, i = i)
  k_hat_4 <- compute_k_hat(yields, term_premia, i = i)

  # Verify formula: variance_bound = 0.25 * c_hat * k_hat
  expected_bound <- 0.25 * c_hat_4 * k_hat_4

  expect_equal(var_bound_4, expected_bound,
    tolerance = 1e-10,
    label = "Variance bound should equal 0.25 * c_hat * k_hat"
  )
})

test_that("variance bound generally increases with maturity", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Compute variance bounds for all maturities
  var_bounds <- numeric(9)
  for (i in 1:9) {
    var_bounds[i] <- compute_variance_bound(yields, term_premia, i = i)
  }

  # Check general increasing trend (allowing for some non-monotonicity)
  # Count how many increases vs decreases
  increases <- sum(diff(var_bounds[2:9]) > 0) # Exclude i=1 which is 0
  expect_gte(increases, 4,
    label = "Variance bound should generally increase with maturity"
  )
})
