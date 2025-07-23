# Test file for compute_w2_residuals function
# Tests SDF innovation residual calculations

test_that("compute_w2_residuals returns expected structure", {
  # Create test data
  set.seed(123)
  n <- 100

  # Create yield and term premia data
  yields <- data.frame(
    y1 = rnorm(n, 2, 0.5),
    y2 = rnorm(n, 2.5, 0.5),
    y3 = rnorm(n, 3, 0.5),
    y4 = rnorm(n, 3.2, 0.5),
    y5 = rnorm(n, 3.5, 0.5),
    y6 = rnorm(n, 3.7, 0.5),
    y7 = rnorm(n, 3.8, 0.5),
    y8 = rnorm(n, 3.9, 0.5),
    y9 = rnorm(n, 4, 0.5),
    y10 = rnorm(n, 4.1, 0.5)
  )

  term_premia <- data.frame(
    tp1 = rnorm(n, 0.5, 0.2),
    tp2 = rnorm(n, 0.7, 0.2),
    tp3 = rnorm(n, 0.9, 0.2),
    tp4 = rnorm(n, 1.0, 0.2),
    tp5 = rnorm(n, 1.1, 0.2),
    tp6 = rnorm(n, 1.2, 0.2),
    tp7 = rnorm(n, 1.3, 0.2),
    tp8 = rnorm(n, 1.4, 0.2),
    tp9 = rnorm(n, 1.5, 0.2),
    tp10 = rnorm(n, 1.6, 0.2)
  )

  # Create PCs that match the data dimensions
  pcs <- matrix(rnorm(n * 4), n, 4)
  colnames(pcs) <- paste0("pc", 1:4)

  # Test with list output (default)
  result <- compute_w2_residuals(yields, term_premia, pcs = pcs)

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("residuals", "fitted", "coefficients", "r_squared", "n_obs"))

  # Check residuals
  expect_type(result$residuals, "list")
  expect_length(result$residuals, 9) # Default maturities 1:9
  expect_true(all(sapply(result$residuals, length) == n - 1)) # n-1 due to differencing

  # Check fitted values
  expect_type(result$fitted, "list")
  expect_length(result$fitted, 9)

  # Check coefficients
  expect_true(is.matrix(result$coefficients))
  expect_equal(nrow(result$coefficients), 9)
  expect_equal(ncol(result$coefficients), 5) # intercept + 4 PCs

  # Check R-squared
  expect_type(result$r_squared, "double")
  expect_length(result$r_squared, 9)
  expect_true(all(result$r_squared >= 0 & result$r_squared <= 1))

  # Check n_obs
  expect_type(result$n_obs, "double") # R often returns numeric not integer
  expect_true(all(result$n_obs == n - 1))
})

test_that("compute_w2_residuals R-squared validation", {
  set.seed(456)
  n <- 200

  # Create PCs
  pcs <- matrix(rnorm(n * 4), n, 4)

  # Create yields with strong PC relationship
  true_coefs <- matrix(rnorm(10 * 4), 10, 4)
  yields <- as.data.frame(pcs %*% t(true_coefs[1:10, ]) +
    matrix(rnorm(n * 10, sd = 0.1), n, 10))
  names(yields) <- paste0("y", 1:10)

  # Create term premia similarly
  term_premia <- as.data.frame(pcs %*% t(true_coefs[1:10, ] * 0.3) +
    matrix(rnorm(n * 10, sd = 0.1), n, 10))
  names(term_premia) <- paste0("tp", 1:10)

  result <- compute_w2_residuals(yields, term_premia, pcs = pcs)

  # With strong PC relationship, R-squared should be reasonably high
  expect_true(mean(result$r_squared) > 0.3)
})

test_that("compute_w2_residuals PC effects", {
  set.seed(789)
  n <- 150

  yields <- data.frame(matrix(rnorm(n * 10, 3, 0.5), n, 10))
  names(yields) <- paste0("y", 1:10)
  term_premia <- data.frame(matrix(rnorm(n * 10, 1, 0.3), n, 10))
  names(term_premia) <- paste0("tp", 1:10)

  # Create PC matrix for testing
  pcs_full <- matrix(rnorm(n * 6), n, 6)
  colnames(pcs_full) <- paste0("pc", 1:6)

  # Test with different numbers of PCs
  results <- list()
  for (n_pcs in 1:6) {
    results[[n_pcs]] <- compute_w2_residuals(yields, term_premia, n_pcs = n_pcs, pcs = pcs_full[, 1:n_pcs, drop = FALSE])
  }

  # More PCs should generally give higher R-squared
  r_squared_means <- sapply(results, function(r) mean(r$r_squared))
  expect_true(r_squared_means[6] >= r_squared_means[1])

  # Check coefficient dimensions match n_pcs
  for (n_pcs in 1:6) {
    expect_equal(ncol(results[[n_pcs]]$coefficients), n_pcs + 1) # +1 for intercept
  }
})

test_that("compute_w2_residuals maturity selection", {
  set.seed(111)
  n <- 100

  yields <- data.frame(matrix(rnorm(n * 10, 3, 0.5), n, 10))
  names(yields) <- paste0("y", 1:10)
  term_premia <- data.frame(matrix(rnorm(n * 10, 1, 0.3), n, 10))
  names(term_premia) <- paste0("tp", 1:10)

  # Create PCs for testing
  pcs <- matrix(rnorm(n * 4), n, 4)
  colnames(pcs) <- paste0("pc", 1:4)

  # Test specific maturities
  maturities <- c(2, 5, 8)
  result <- compute_w2_residuals(yields, term_premia, maturities = maturities, pcs = pcs)

  expect_length(result$residuals, 3)
  expect_length(result$r_squared, 3)
  expect_equal(nrow(result$coefficients), 3)

  # Test single maturity
  result_single <- compute_w2_residuals(yields, term_premia, maturities = 5, pcs = pcs)
  expect_length(result_single$residuals, 1)
})

test_that("compute_w2_residuals data frame output", {
  set.seed(222)
  n <- 80
  dates <- seq(as.Date("2020-01-01"), length.out = n, by = "month")

  yields <- data.frame(matrix(rnorm(n * 10, 3, 0.5), n, 10))
  names(yields) <- paste0("y", 1:10)
  term_premia <- data.frame(matrix(rnorm(n * 10, 1, 0.3), n, 10))
  names(term_premia) <- paste0("tp", 1:10)

  # Create PCs for testing
  pcs <- matrix(rnorm(n * 4), n, 4)
  colnames(pcs) <- paste0("pc", 1:4)

  # Test with return_df = TRUE
  result_df <- compute_w2_residuals(yields, term_premia,
    maturities = c(2, 5),
    return_df = TRUE,
    dates = dates,
    pcs = pcs
  )

  # Check structure
  expect_s3_class(result_df, "data.frame")
  expect_named(result_df, c("date", "maturity", "residuals", "fitted"))

  # Check dimensions
  expect_equal(nrow(result_df), 2 * (n - 1)) # 2 maturities, n-1 observations each

  # Check maturity values
  expect_equal(unique(result_df$maturity), c(2, 5))

  # Check dates (compute_w2_residuals uses all dates, not just 2:n)
  # The function handles the differencing internally but returns all dates
  expect_equal(length(unique(result_df$date)), n - 1)
})

test_that("compute_w2_residuals missing data handling", {
  set.seed(333)
  n <- 100

  yields <- data.frame(matrix(rnorm(n * 10, 3, 0.5), n, 10))
  names(yields) <- paste0("y", 1:10)
  term_premia <- data.frame(matrix(rnorm(n * 10, 1, 0.3), n, 10))
  names(term_premia) <- paste0("tp", 1:10)

  # Introduce some NAs
  yields[c(10, 20, 30), "y3"] <- NA
  term_premia[c(15, 25), "tp5"] <- NA

  # Create PCs for testing
  pcs <- matrix(rnorm(n * 4), n, 4)
  colnames(pcs) <- paste0("pc", 1:4)

  # Should handle NAs without error
  result <- compute_w2_residuals(yields, term_premia, maturities = c(3, 5), pcs = pcs)

  expect_type(result, "list")
  expect_length(result$residuals, 2)
})

test_that("compute_w2_residuals orthogonality", {
  set.seed(444)
  n <- 200

  # Create orthogonal PCs
  pcs <- qr.Q(qr(matrix(rnorm(n * 4), n, 4)))

  yields <- data.frame(matrix(rnorm(n * 10, 3, 0.5), n, 10))
  names(yields) <- paste0("y", 1:10)
  term_premia <- data.frame(matrix(rnorm(n * 10, 1, 0.3), n, 10))
  names(term_premia) <- paste0("tp", 1:10)

  result <- compute_w2_residuals(yields, term_premia, pcs = pcs)

  # Residuals should be approximately orthogonal to PCs
  for (i in 1:length(result$residuals)) {
    residuals <- result$residuals[[i]]
    pc_subset <- pcs[2:n, ] # Match dimensions after differencing

    correlations <- abs(cor(residuals, pc_subset))
    expect_true(all(correlations < 0.15)) # Small correlations
  }
})

test_that("compute_w2_residuals consistency", {
  set.seed(555)
  n <- 150

  yields <- data.frame(matrix(rnorm(n * 10, 3, 0.5), n, 10))
  names(yields) <- paste0("y", 1:10)
  term_premia <- data.frame(matrix(rnorm(n * 10, 1, 0.3), n, 10))
  names(term_premia) <- paste0("tp", 1:10)

  # Create PCs for testing
  pcs <- matrix(rnorm(n * 4), n, 4)
  colnames(pcs) <- paste0("pc", 1:4)

  # Run twice with same data
  result1 <- compute_w2_residuals(yields, term_premia, maturities = 1:5, pcs = pcs)
  result2 <- compute_w2_residuals(yields, term_premia, maturities = 1:5, pcs = pcs)

  # Results should be identical
  for (i in 1:5) {
    expect_equal(result1$residuals[[i]], result2$residuals[[i]])
  }
  expect_equal(result1$r_squared, result2$r_squared)
  expect_equal(result1$coefficients, result2$coefficients)
})

test_that("compute_w2_residuals extreme values", {
  set.seed(666)
  n <- 50

  # Create data with extreme values
  yields <- data.frame(matrix(c(
    rnorm(n * 5, 3, 0.5),
    rnorm(n * 5, 3, 5)
  ), n, 10)) # High variance in later maturities
  names(yields) <- paste0("y", 1:10)

  term_premia <- data.frame(matrix(rnorm(n * 10, 1, 0.3), n, 10))
  names(term_premia) <- paste0("tp", 1:10)

  # Create PCs for testing
  pcs <- matrix(rnorm(n * 4), n, 4)
  colnames(pcs) <- paste0("pc", 1:4)

  # Should handle without error
  result <- compute_w2_residuals(yields, term_premia, pcs = pcs)

  expect_type(result, "list")
  expect_false(any(is.na(result$r_squared)))
  expect_false(any(is.infinite(unlist(result$residuals))))
})

test_that("compute_w2_residuals error handling", {
  set.seed(777)
  n <- 50

  yields <- data.frame(matrix(rnorm(n * 10, 3, 0.5), n, 10))
  names(yields) <- paste0("y", 1:10)
  term_premia <- data.frame(matrix(rnorm(n * 10, 1, 0.3), n, 10))
  names(term_premia) <- paste0("tp", 1:10)

  # Mismatched dimensions
  expect_error(compute_w2_residuals(yields[1:40, ], term_premia))

  # Invalid maturities
  expect_error(compute_w2_residuals(yields, term_premia, maturities = 11))
  expect_error(compute_w2_residuals(yields, term_premia, maturities = 0))

  # Wrong column names
  yields_wrong <- yields
  names(yields_wrong) <- paste0("yield", 1:10)
  expect_error(compute_w2_residuals(yields_wrong, term_premia))

  # Insufficient observations
  expect_error(compute_w2_residuals(yields[1:3, ], term_premia[1:3, ]))
})
