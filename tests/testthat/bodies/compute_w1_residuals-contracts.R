{
  data("variables", package = "hetid", envir = environment())

  res_y1 <- suppressMessages(compute_w1_residuals(n_pcs = 3))

  cons_growth <- variables$gr1.pcecc96
  pc_cols <- get_pc_column_names(3)
  pc_data <- as.matrix(variables[, pc_cols])

  n <- nrow(variables)
  pc_lagged <- pc_data[1:(n - 1), ]
  y <- cons_growth[2:n]

  manual_reg <- lm(y ~ pc_lagged)
  manual_r2 <- summary(manual_reg)$r.squared
  manual_coefs <- coef(manual_reg)

  expect_equal(res_y1$r_squared, manual_r2,
    tolerance = 1e-10,
    label = "R-squared should match manual regression"
  )

  names(manual_coefs) <- c("(Intercept)", get_pc_column_names(3))

  expect_equal(res_y1$coefficients, manual_coefs,
    tolerance = 1e-10,
    label = "Coefficients should match manual regression"
  )
}

{
  # Nested-model monotonicity only holds on a fixed common sample, so
  # subset to rows complete in all used columns before comparing
  data("variables", package = "hetid", envir = environment())
  used_cols <- c(
    HETID_CONSTANTS$CONSUMPTION_GROWTH_COL,
    get_pc_column_names(6)
  )
  common_rows <- complete.cases(
    as.data.frame(variables[, used_cols])
  )
  common_data <- variables[common_rows, ]

  r2_values <- numeric(6)
  for (j in 1:6) {
    res <- compute_w1_residuals(n_pcs = j, data = common_data)
    r2_values[j] <- res$r_squared
  }

  # R-squared should not decrease for nested models on the same sample
  expect_true(all(diff(r2_values) >= -1e-10),
    label = "R-squared should not decrease with more PCs"
  )
}
