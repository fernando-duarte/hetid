test_that("compute_w1_residuals returns residuals from PC regression", {
  res_y1 <- suppressMessages(compute_w1_residuals(n_pcs = 4))

  expect_type(res_y1, "list")
  expect_true("residuals" %in% names(res_y1))
  expect_true("r_squared" %in% names(res_y1))
  expect_true("coefficients" %in% names(res_y1))
  expect_true("dates" %in% names(res_y1))

  expect_type(res_y1$residuals, "double")
  expect_true(is.vector(res_y1$residuals))

  expect_gte(res_y1$r_squared, 0)
  expect_lte(res_y1$r_squared, 1)
})

test_that("R-squared matches manual regression", {
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
})

test_that("R-squared increases with more PCs", {
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
})

test_that("compute_w1_residuals works with custom data", {
  data("variables", package = "hetid", envir = environment())

  subset_vars <- variables[50:150, ]
  res_subset <- compute_w1_residuals(n_pcs = 2, data = subset_vars)

  expect_type(res_subset, "list")
  expect_length(res_subset$residuals, nrow(subset_vars) - 1)
  expect_equal(length(res_subset$dates), nrow(subset_vars) - 1)
})

test_that("residual mean is approximately zero", {
  res_y1 <- suppressMessages(compute_w1_residuals(n_pcs = 4))

  expect_lt(abs(mean(res_y1$residuals)), 1e-10,
    label = "Residual mean should be essentially 0"
  )
})

test_that("coefficient structure is correct", {
  res_y1 <- suppressMessages(compute_w1_residuals(n_pcs = 3))

  expect_length(res_y1$coefficients, 4)
  expect_true("(Intercept)" %in% names(res_y1$coefficients))
  expect_true(all(get_pc_column_names(3) %in% names(res_y1$coefficients)))
})

test_that("dates are properly aligned after lagging", {
  data("variables", package = "hetid", envir = environment())

  res_y1 <- suppressMessages(compute_w1_residuals(n_pcs = 4))

  # the bundled fallback normalizes the as-imported quarter-start labels to
  # period-end at ingestion, so compare against the normalized dates
  expect_equal(res_y1$dates, to_period_end(variables$date, "quarterly")[-1])
  expect_length(res_y1$dates, length(res_y1$residuals))
})

test_that("message emitted when falling back to package data", {
  expect_message(
    compute_w1_residuals(n_pcs = 2),
    "Using bundled"
  )
})

test_that("no message when user provides data", {
  data("variables", package = "hetid", envir = environment())
  expect_no_message(
    compute_w1_residuals(n_pcs = 2, data = variables)
  )
})

test_that("errors without a date column (date is always required)", {
  data("variables", package = "hetid", envir = environment())
  no_date <- variables[, setdiff(names(variables), "date")]

  # The date column is now mandatory in every return shape: a time series
  # cannot be returned without its realization dates
  expect_error(
    compute_w1_residuals(n_pcs = 2, data = no_date),
    class = "hetid_error"
  )
})

test_that("errors without date column when return_df = TRUE", {
  data("variables", package = "hetid", envir = environment())
  no_date <- variables[, setdiff(names(variables), "date")]

  expect_error(
    compute_w1_residuals(
      n_pcs = 2, data = no_date, return_df = TRUE
    ),
    "date"
  )
})

test_that("returns a data frame with dates when return_df = TRUE", {
  res <- suppressMessages(
    compute_w1_residuals(n_pcs = 4, return_df = TRUE)
  )

  expect_s3_class(res, "data.frame")
  expect_named(res, c("date", "residuals", "fitted"))
  expect_equal(nrow(res), length(res$residuals))
})

test_that(
  "compute_w1_residuals errors on single-row data",
  {
    one_row <- data.frame(
      date = as.Date("1959-03-31"),
      gr1.pcecc96 = 0.5,
      pc1 = 0.1, pc2 = 0.2,
      pc3 = 0.3, pc4 = 0.4
    )
    expect_error(
      suppressMessages(
        compute_w1_residuals(n_pcs = 4, data = one_row)
      ),
      class = "hetid_error_insufficient_data"
    )
  }
)

test_that(
  "compute_w1_residuals errors when observations cannot support regression",
  {
    # Five rows leave four usable pairs after lagging: fewer than the
    # n_pcs + 2 needed, so this must not return a saturated fit
    few_rows <- data.frame(
      date = seq(as.Date("1959-03-31"), by = "quarter", length.out = 5),
      gr1.pcecc96 = c(0.5, 0.2, 0.1, 0.4, 0.3),
      pc1 = c(0.1, 0.2, 0.3, 0.4, 0.5),
      pc2 = c(0.5, 0.4, 0.3, 0.2, 0.1),
      pc3 = c(0.1, 0.3, 0.2, 0.5, 0.4),
      pc4 = c(0.2, 0.1, 0.4, 0.3, 0.5)
    )
    expect_error(
      compute_w1_residuals(n_pcs = 4, data = few_rows),
      class = "hetid_error_insufficient_data"
    )
  }
)

test_that("compute_w1_residuals rejects matrix data with a type error", {
  mat <- cbind(
    gr1.pcecc96 = c(0.5, 0.2, 0.1, 0.4, 0.3, 0.6, 0.2, 0.1),
    pc1 = seq(0.1, 0.8, by = 0.1),
    pc2 = seq(0.8, 0.1, by = -0.1)
  )
  expect_error(
    compute_w1_residuals(n_pcs = 2, data = mat),
    regexp = "data frame",
    class = "hetid_error_bad_argument"
  )
})

test_that("compute_w1_residuals rejects non-tabular data", {
  expect_error(
    compute_w1_residuals(n_pcs = 2, data = list(pc1 = 1:10)),
    class = "hetid_error_bad_argument"
  )
})
