# Test file for compute_w1_residuals function
# Tests consumption growth residual calculations

test_that("compute_w1_residuals returns expected structure", {
  # Test with default parameters
  result <- compute_w1_residuals()

  # Check structure when return_df = FALSE (default)
  expect_type(result, "list")
  expect_named(result, c("residuals", "fitted", "coefficients", "r_squared", "dates", "model"))

  # Check residuals and fitted values
  expect_type(result$residuals, "double")
  expect_type(result$fitted, "double")
  expect_equal(length(result$residuals), length(result$fitted))

  # Check coefficients (intercept + n_pcs)
  expect_length(result$coefficients, 5) # intercept + 4 PCs (default)

  # Check R-squared
  expect_type(result$r_squared, "double")
  expect_gte(result$r_squared, 0)
  expect_lte(result$r_squared, 1)

  # Check dates
  expect_s3_class(result$dates, "Date")
  expect_equal(length(result$dates), length(result$residuals))

  # Check model
  expect_s3_class(result$model, "lm")
})

test_that("compute_w1_residuals R-squared validation", {
  # Test with different numbers of PCs
  r_squared_values <- numeric(6)

  for (n_pcs in 1:6) {
    result <- compute_w1_residuals(n_pcs = n_pcs)
    r_squared_values[n_pcs] <- result$r_squared
  }

  # More PCs should generally give higher R-squared
  expect_true(r_squared_values[6] >= r_squared_values[1])

  # All R-squared values should be valid
  expect_true(all(r_squared_values >= 0 & r_squared_values <= 1))
})

test_that("compute_w1_residuals PC usage", {
  # Test different numbers of PCs
  for (n_pcs in 1:6) {
    result <- compute_w1_residuals(n_pcs = n_pcs)

    # Check coefficient count matches n_pcs + 1 (intercept)
    expect_length(result$coefficients, n_pcs + 1)

    # Check coefficient names
    expected_names <- c("(Intercept)", paste0("pc", 1:n_pcs))
    expect_equal(names(result$coefficients), expected_names)
  }
})

test_that("compute_w1_residuals properties", {
  result <- compute_w1_residuals()

  # Residuals should have mean approximately zero
  expect_lt(abs(mean(result$residuals)), 0.01)

  # Residuals + fitted should equal original Y1 (within numerical tolerance)
  # We can't verify this directly without access to the original data
  # but we can check that residuals and fitted have reasonable values
  expect_false(any(is.na(result$residuals)))
  expect_false(any(is.infinite(result$residuals)))
  expect_false(any(is.na(result$fitted)))
  expect_false(any(is.infinite(result$fitted)))
})

test_that("compute_w1_residuals data frame output", {
  # Test with return_df = TRUE
  result_df <- compute_w1_residuals(n_pcs = 4, return_df = TRUE)

  # Check it's a data frame
  expect_s3_class(result_df, "data.frame")
  expect_named(result_df, c("date", "residuals", "fitted"))

  # Check column types
  expect_s3_class(result_df$date, "Date")
  expect_type(result_df$residuals, "double")
  expect_type(result_df$fitted, "double")

  # Check dimensions
  expect_gt(nrow(result_df), 0)
  expect_equal(ncol(result_df), 3)

  # Compare with list output
  result_list <- compute_w1_residuals(n_pcs = 4, return_df = FALSE)
  expect_equal(result_df$residuals, unname(result_list$residuals))
  expect_equal(result_df$fitted, unname(result_list$fitted))
  expect_equal(result_df$date, result_list$dates)
})

test_that("compute_w1_residuals custom data", {
  # Create custom data matching expected format
  set.seed(123)
  n <- 200
  dates <- seq(as.Date("2010-01-01"), length.out = n, by = "quarter")

  # Create data with required variables
  custom_data <- data.frame(
    date = dates,
    gr1.pcecc96 = rnorm(n, 0.02, 0.01), # Consumption growth
    pc1 = rnorm(n),
    pc2 = rnorm(n),
    pc3 = rnorm(n),
    pc4 = rnorm(n),
    pc5 = rnorm(n),
    pc6 = rnorm(n)
  )

  # Test with custom data
  result <- compute_w1_residuals(n_pcs = 3, data = custom_data)

  # Should use our custom data (n-1 due to lagging)
  expect_equal(length(result$residuals), n - 1)
  expect_equal(result$dates, dates[2:n])
})

test_that("compute_w1_residuals orthogonality check", {
  # Get residuals with different PC counts
  result <- compute_w1_residuals(n_pcs = 4)

  # Load the actual PC data to check orthogonality
  data("variables", package = "hetid", envir = environment())
  pc_cols <- paste0("pc", 1:4)

  # Find matching dates between residuals and PCs
  common_dates <- intersect(variables$date, result$dates)

  if (length(common_dates) > 0) {
    # Get PCs for matching dates
    pc_data <- variables[variables$date %in% common_dates, pc_cols]
    residuals_matched <- result$residuals[result$dates %in% common_dates]

    # Residuals should be approximately orthogonal to PCs used in regression
    correlations <- cor(residuals_matched, pc_data)
    expect_true(all(abs(correlations) < 0.2)) # Relaxed tolerance for real data
  }
})

test_that("compute_w1_residuals consistency", {
  # Run the function twice with same parameters
  result1 <- compute_w1_residuals(n_pcs = 4)
  result2 <- compute_w1_residuals(n_pcs = 4)

  # Results should be identical
  expect_equal(result1$residuals, result2$residuals)
  expect_equal(result1$fitted, result2$fitted)
  expect_equal(result1$coefficients, result2$coefficients)
  expect_equal(result1$r_squared, result2$r_squared)
})

test_that("compute_w1_residuals error handling", {
  # Invalid n_pcs
  expect_error(compute_w1_residuals(n_pcs = 0), "n_pcs must be between 1 and 6")
  expect_error(compute_w1_residuals(n_pcs = 7), "n_pcs must be between 1 and 6")
  expect_error(compute_w1_residuals(n_pcs = -1), "n_pcs must be between 1 and 6")

  # Invalid data type
  expect_error(compute_w1_residuals(data = "not a data frame"))

  # Data missing required columns
  bad_data <- data.frame(
    date = seq(as.Date("2020-01-01"), length.out = 10, by = "month"),
    x = rnorm(10)
  )
  expect_error(compute_w1_residuals(data = bad_data))
})
