test_that("compute_w1_residuals returns residuals from PC regression", {
  # Test with default 4 PCs
  res_y1 <- compute_w1_residuals(n_pcs = 4)

  expect_type(res_y1, "list")
  expect_true("residuals" %in% names(res_y1))
  expect_true("r_squared" %in% names(res_y1))
  expect_true("coefficients" %in% names(res_y1))
  expect_true("dates" %in% names(res_y1))

  # Residuals should be numeric vector
  expect_type(res_y1$residuals, "double")
  expect_true(is.vector(res_y1$residuals))

  # R-squared should be between 0 and 1
  expect_gte(res_y1$r_squared, 0)
  expect_lte(res_y1$r_squared, 1)
})

test_that("R-squared matches manual regression", {
  # Load variables data
  data("variables")

  # Compute with 3 PCs
  res_y1 <- compute_w1_residuals(n_pcs = 3)

  # Manual regression
  cons_growth <- variables$gr1.pcecc96
  pc_cols <- paste0("pc", 1:3)
  pc_data <- as.matrix(variables[, pc_cols])

  # Create lagged PCs
  n <- nrow(variables)
  pc_lagged <- pc_data[1:(n - 1), ]
  y <- cons_growth[2:n]

  # Run regression manually
  manual_reg <- lm(y ~ pc_lagged)
  manual_r2 <- summary(manual_reg)$r.squared
  manual_coefs <- coef(manual_reg)

  expect_equal(res_y1$r_squared, manual_r2,
    tolerance = 1e-10,
    label = "R-squared should match manual regression"
  )

  # Check coefficients match
  # Rename manual coefficients to match function output
  names(manual_coefs) <- c("(Intercept)", paste0("pc", 1:3))

  expect_equal(res_y1$coefficients, manual_coefs,
    tolerance = 1e-10,
    label = "Coefficients should match manual regression"
  )
})

test_that("R-squared increases with more PCs", {
  # Test with different numbers of PCs
  r2_values <- numeric(6)
  for (j in 1:6) {
    res <- compute_w1_residuals(n_pcs = j)
    r2_values[j] <- res$r_squared
  }

  # R-squared should generally increase
  expect_true(all(diff(r2_values) >= -1e-10),
    label = "R-squared should not decrease with more PCs"
  )
})

test_that("compute_w1_residuals works with custom data", {
  # Load variables data
  data("variables")

  # Use a subset
  subset_vars <- variables[50:150, ]
  res_subset <- compute_w1_residuals(n_pcs = 2, data = subset_vars)

  expect_type(res_subset, "list")
  expect_length(res_subset$residuals, nrow(subset_vars) - 1)
  expect_equal(length(res_subset$dates), nrow(subset_vars) - 1)
})

test_that("residual mean is approximately zero", {
  # Compute residuals
  res_y1 <- compute_w1_residuals(n_pcs = 4)

  # Mean should be very close to 0
  expect_lt(abs(mean(res_y1$residuals)), 1e-10,
    label = "Residual mean should be essentially 0"
  )
})

test_that("coefficient structure is correct", {
  # Test with 3 PCs
  res_y1 <- compute_w1_residuals(n_pcs = 3)

  # Should have intercept + 3 PC coefficients
  expect_length(res_y1$coefficients, 4)
  expect_true("(Intercept)" %in% names(res_y1$coefficients))
  expect_true(all(paste0("pc", 1:3) %in% names(res_y1$coefficients)))
})

test_that("dates are properly aligned after lagging", {
  # Load variables data
  data("variables")

  # Compute residuals
  res_y1 <- compute_w1_residuals(n_pcs = 4)

  # Dates should be aligned with residuals (excluding first date due to lag)
  expect_equal(res_y1$dates, variables$date[-1])
  expect_length(res_y1$dates, length(res_y1$residuals))
})
