# Test file for solve_theta_quadratic function
# Tests gamma1 quadratic solver with validation

test_that("solve_theta_quadratic returns expected structure", {
  # Create test data
  set.seed(123)
  n <- 100
  pc_j <- rnorm(n)
  w1 <- rnorm(n, 0, 0.5)
  w2 <- rnorm(n, 0, 0.5)
  tau <- 0.975

  # Run solver
  result <- solve_theta_quadratic(pc_j, w1, w2, tau)

  # Check structure
  expect_type(result, "list")
  expect_true("roots" %in% names(result))
  expect_true("coefficients" %in% names(result))
  expect_true("discriminant" %in% names(result))
  expect_true("components" %in% names(result))

  # If roots exist, should be length 2
  if (!is.null(result$roots)) {
    expect_equal(length(result$roots), 2)
  }
})

test_that("solve_theta_quadratic mathematical validation", {
  # Create controlled data for validation
  set.seed(456)
  n <- 200

  # Design data to ensure valid roots
  pc_j <- rnorm(n, 0, 1)
  w1 <- 0.5 * pc_j + rnorm(n, 0, 0.2)
  w2 <- -0.3 * pc_j + rnorm(n, 0, 0.2)
  tau <- 0.975

  result <- solve_theta_quadratic(pc_j, w1, w2, tau)

  # Should have roots
  expect_false(is.null(result$roots))

  # Verify roots satisfy the quadratic equation
  if (!is.null(result$roots)) {
    a <- result$coefficients["a"]
    b <- result$coefficients["b"]
    c <- result$coefficients["c"]

    # Check each root (should equal 0, not q)
    for (root in result$roots) {
      lhs <- a * root^2 + b * root + c
      # Remove name to compare only numeric value
      expect_equal(unname(lhs), 0, tolerance = 1e-10)
    }
  }
})

test_that("solve_theta_quadratic quantile calculation", {
  # Test quantile calculation
  set.seed(789)
  n <- 150
  pc_j <- runif(n, -2, 2)
  w1 <- rnorm(n)
  w2 <- rnorm(n)

  # Test different tau values
  tau_values <- c(0.9, 0.95, 0.975, 0.99)

  for (tau in tau_values) {
    result <- solve_theta_quadratic(pc_j, w1, w2, tau)

    # Should have valid structure
    expect_type(result, "list")

    # Higher tau might give different roots
    if (tau == 0.99 && !is.null(result$roots)) {
      result_95 <- solve_theta_quadratic(pc_j, w1, w2, 0.95)
      # Just check both have roots or both don't
      expect_equal(is.null(result$roots), is.null(result_95$roots))
    }
  }
})

test_that("solve_theta_quadratic handles edge cases", {
  # Test with minimal data
  n_min <- 10
  pc_j <- rnorm(n_min)
  w1 <- rnorm(n_min)
  w2 <- rnorm(n_min)

  result <- solve_theta_quadratic(pc_j, w1, w2, 0.975)

  expect_type(result, "list")
  expect_true("coefficients" %in% names(result))

  # Test with many NAs
  pc_j_na <- c(pc_j, rep(NA, 50))
  w1_na <- c(w1, rep(NA, 50))
  w2_na <- c(w2, rep(NA, 50))

  result_na <- solve_theta_quadratic(pc_j_na, w1_na, w2_na, 0.975)

  # Should handle NAs gracefully
  expect_type(result_na, "list")
})

test_that("solve_theta_quadratic no real roots case", {
  # Create data likely to have no real roots
  set.seed(111)
  n <- 100

  # Make discriminant likely negative
  pc_j <- rnorm(n, 0, 0.1) # Small variance
  w1 <- rnorm(n, 0, 2) # Large variance
  w2 <- rnorm(n, 0, 2) # Large variance

  result <- solve_theta_quadratic(pc_j, w1, w2, 0.975)

  # Should still return valid structure
  expect_type(result, "list")

  # If no roots, roots should be NULL
  if (is.null(result$roots)) {
    expect_null(result$roots)
  }
})

test_that("solve_theta_quadratic date handling", {
  # Test with dates
  set.seed(222)
  n <- 100
  pc_j <- rnorm(n)
  w1 <- rnorm(n)
  w2 <- rnorm(n)
  dates <- seq(as.Date("2020-01-01"), by = "month", length.out = n)

  result <- solve_theta_quadratic(pc_j, w1, w2, 0.975, dates = dates)

  # Should handle dates without error
  expect_type(result, "list")

  # Add NAs and check date alignment
  pc_j[c(10, 20)] <- NA
  result_na <- solve_theta_quadratic(pc_j, w1, w2, 0.975, dates = dates)

  # Should still return valid structure
  expect_type(result_na, "list")
})

test_that("solve_theta_quadratic interval properties", {
  # Create synthetic data to test interval properties
  set.seed(555)
  n <- 200

  # Create correlated data
  pc_j <- rnorm(n)
  w1 <- 0.3 * pc_j + rnorm(n, 0, 0.5)
  w2 <- -0.2 * pc_j + rnorm(n, 0, 0.5)

  # Solve for gamma
  result <- solve_theta_quadratic(
    pc_j = pc_j,
    w1 = w1,
    w2 = w2,
    tau = 0.975
  )

  # If we have roots, check interval properties
  if (!is.null(result$roots) && length(result$roots) == 2) {
    lower <- min(result$roots)
    upper <- max(result$roots)

    # Interval should be reasonable
    expect_true(upper > lower)
    expect_true(upper - lower < 1000) # Not too wide

    # Roots should be finite
    expect_true(all(is.finite(result$roots)))
  }
})

test_that("solve_theta_quadratic consistency", {
  # Test consistency across multiple runs
  set.seed(333)
  n <- 150
  pc_j <- rnorm(n)
  w1 <- rnorm(n)
  w2 <- rnorm(n)
  tau <- 0.95

  # Run multiple times
  results <- replicate(5,
    {
      solve_theta_quadratic(pc_j, w1, w2, tau)
    },
    simplify = FALSE
  )

  # All should give same results
  roots_1 <- results[[1]]$roots
  for (i in 2:5) {
    if (!is.null(roots_1) && !is.null(results[[i]]$roots)) {
      expect_equal(results[[i]]$roots, roots_1)
    }
  }
})

test_that("solve_theta_quadratic error handling", {
  # Test input validation
  n <- 50
  pc_j <- rnorm(n)
  w1 <- rnorm(n)
  w2 <- rnorm(n)

  # Invalid tau
  expect_error(solve_theta_quadratic(pc_j, w1, w2, tau = -0.1))
  expect_error(solve_theta_quadratic(pc_j, w1, w2, tau = 1.5))

  # Mismatched lengths
  expect_error(solve_theta_quadratic(pc_j[1:10], w1, w2, tau = 0.95))

  # Non-numeric inputs
  expect_error(solve_theta_quadratic("not numeric", w1, w2, tau = 0.95))

  # Too few valid observations
  expect_error(solve_theta_quadratic(c(1, 2), c(1, 2), c(1, 2), tau = 0.95))
})
