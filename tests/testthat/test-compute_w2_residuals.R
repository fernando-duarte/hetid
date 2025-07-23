test_that("compute_w2_residuals works for single maturity", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test single maturity - let the function handle PC loading internally
  res_y2 <- compute_w2_residuals(yields, term_premia,
    maturities = 5, n_pcs = 4
  )

  expect_type(res_y2, "list")
  expect_true("residuals" %in% names(res_y2))
  expect_true("r_squared" %in% names(res_y2))
  expect_true("coefficients" %in% names(res_y2))

  # Should have one element in residuals list
  expect_length(res_y2$residuals, 1)
  expect_length(res_y2$r_squared, 1)
})

test_that("compute_w2_residuals works for maturity 1", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test maturity 1 specifically
  res_y2_mat1 <- compute_w2_residuals(yields, term_premia,
    maturities = 1, n_pcs = 4
  )

  expect_type(res_y2_mat1, "list")
  expect_true("residuals" %in% names(res_y2_mat1))
  expect_true("r_squared" %in% names(res_y2_mat1))

  # Check that we get valid residuals for maturity 1
  expect_true("maturity_1" %in% names(res_y2_mat1$residuals))
  expect_true(length(res_y2_mat1$residuals$maturity_1) > 0)

  # R-squared should be reasonable
  expect_gt(res_y2_mat1$r_squared[1], 0)
  expect_lt(res_y2_mat1$r_squared[1], 1)
})

test_that("compute_w2_residuals works for multiple maturities", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test multiple maturities (including maturity 1) - let the function handle PC loading internally
  maturities <- c(1, 2, 5, 7)
  res_y2 <- compute_w2_residuals(yields, term_premia,
    maturities = maturities, n_pcs = 4
  )

  # Should have one element per maturity
  expect_length(res_y2$residuals, length(maturities))
  expect_length(res_y2$r_squared, length(maturities))
  expect_equal(nrow(res_y2$coefficients), length(maturities))
})

test_that("residual properties check", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for maturity 3 - let the function handle PC loading internally
  res_y2 <- compute_w2_residuals(yields, term_premia,
    maturities = 3, n_pcs = 4
  )
  residuals <- res_y2$residuals[[1]]

  # Mean should be near 0
  expect_lt(abs(mean(residuals, na.rm = TRUE)), 1e-10)

  # Should have finite values
  expect_true(all(is.finite(residuals) | is.na(residuals)))
})

test_that("compute_w2_residuals uses SDF innovations", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Get SDF innovations directly
  i <- 4
  sdf_innov <- compute_sdf_innovations(yields, term_premia, i = i)

  # Get W2 residuals - let the function handle PC loading internally
  res_y2 <- compute_w2_residuals(yields, term_premia,
    maturities = i, n_pcs = 4
  )

  # The dependent variable in the regression should be SDF innovations
  # The residuals length will be limited by the shorter of SDF innovations
  # and available PCs (from variables data)
  expect_true(length(res_y2$residuals[[1]]) <= length(sdf_innov))
  expect_true(length(res_y2$residuals[[1]]) > 0)

  # Check that we have the expected structure
  expect_type(res_y2$residuals[[1]], "double")
  expect_true(all(is.finite(res_y2$residuals[[1]]) | is.na(res_y2$residuals[[1]])))
})

test_that("R-squared matches manual regression", {
  # Load variables data
  data("variables")

  # Load ACM quarterly data
  acm_data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )

  # Create year-quarter identifiers for both datasets
  # Variables: Q1=Jan, Q2=Apr, Q3=Jul, Q4=Oct
  variables$year <- as.numeric(format(variables$date, "%Y"))
  variables$quarter <- ceiling(as.numeric(format(variables$date, "%m")) / 3)
  variables$year_quarter <- paste(variables$year, variables$quarter, sep = "-Q")

  # ACM: Q1=Mar, Q2=Jun, Q3=Sep, Q4=Dec
  acm_data$year <- as.numeric(format(acm_data$date, "%Y"))
  acm_data$quarter <- ceiling(as.numeric(format(acm_data$date, "%m")) / 3)
  acm_data$year_quarter <- paste(acm_data$year, acm_data$quarter, sep = "-Q")

  # Merge datasets by year-quarter
  merged_data <- merge(
    variables[, c("year_quarter", "date", paste0("pc", 1:4))],
    acm_data[, c("year_quarter", "date", grep("^(y|tp)", names(acm_data), value = TRUE))],
    by = "year_quarter",
    suffixes = c("_var", "_acm")
  )

  # Extract merged components
  pcs_merged <- as.matrix(merged_data[, paste0("pc", 1:4)])
  yields_merged <- merged_data[, grep("^y[0-9]", names(merged_data))]
  term_premia_merged <- merged_data[, grep("^tp", names(merged_data))]

  # Test for maturity 5
  i <- 5

  # Compute SDF innovations
  sdf_innov <- compute_sdf_innovations(yields_merged, term_premia_merged, i = i)

  # Create lagged PCs (aligned with SDF innovations which have length T-1)
  n_obs <- nrow(merged_data)
  pcs_lagged <- pcs_merged[1:(n_obs - 1), ]

  # Align SDF innovations and PCs
  complete_idx <- complete.cases(sdf_innov, pcs_lagged)
  y_clean <- sdf_innov[complete_idx]
  pcs_clean <- pcs_lagged[complete_idx, ]

  # Run manual regression
  reg_data_manual <- data.frame(
    y = y_clean,
    pc1 = pcs_clean[, 1],
    pc2 = pcs_clean[, 2],
    pc3 = pcs_clean[, 3],
    pc4 = pcs_clean[, 4]
  )

  manual_model <- lm(y ~ pc1 + pc2 + pc3 + pc4, data = reg_data_manual)
  manual_r2 <- summary(manual_model)$r.squared
  manual_coefs <- coef(manual_model)

  # Run function with the merged data
  res_w2 <- compute_w2_residuals(
    yields_merged,
    term_premia_merged,
    maturities = i,
    n_pcs = 4,
    pcs = pcs_merged
  )

  # Compare R-squared values
  expect_equal(res_w2$r_squared[1], manual_r2,
    tolerance = 1e-10,
    label = "R-squared should match manual calculation"
  )

  # Compare coefficients
  function_coefs <- res_w2$coefficients[1, ]
  names(function_coefs) <- names(manual_coefs)

  expect_equal(function_coefs, manual_coefs,
    tolerance = 1e-10,
    label = "Coefficients should match manual calculation"
  )

  # Verify residuals have the same properties
  expect_equal(length(res_w2$residuals[[1]]), length(residuals(manual_model)),
    label = "Residuals should have same length"
  )

  # Check that residuals have near-zero mean
  expect_lt(abs(mean(res_w2$residuals[[1]])), 1e-10,
    label = "Residuals should have near-zero mean"
  )
})

test_that("quarterly data alignment test", {
  # Load both datasets
  data("variables")
  acm_monthly <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "monthly"
  )
  acm_quarterly <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )

  # Quarterly data should have fewer rows
  expect_lt(nrow(acm_quarterly), nrow(acm_monthly))

  # Quarterly data should be end-of-quarter values
  # Check that quarterly dates are end-of-quarter
  quarterly_months <- format(acm_quarterly$date, "%m")
  expect_true(all(quarterly_months %in% c("03", "06", "09", "12")))
})

test_that("length verification for output", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test with multiple maturities - let the function handle PC loading internally
  maturities <- 1:9
  res_y2 <- compute_w2_residuals(yields, term_premia,
    maturities = maturities, n_pcs = 4
  )

  # Check dimensions
  expect_length(res_y2$residuals, 9)
  expect_length(res_y2$r_squared, 9)

  # Each residual vector should have same length
  residual_lengths <- sapply(res_y2$residuals, length)
  expect_true(all(residual_lengths == residual_lengths[1]))
})
