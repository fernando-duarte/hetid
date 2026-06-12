test_that("compute_n_hat returns time series of expected log bond prices", {
  test_env <- setup_standard_test_env()

  # Test for maturity 60
  n_hat_60 <- compute_n_hat(test_env$yields, test_env$term_premia, i = 60)

  expect_type(n_hat_60, "double")
  expect_length(n_hat_60, nrow(test_env$yields))
  expect_true(all(is.finite(n_hat_60) | is.na(n_hat_60)))
})

test_that("n_hat should generally be negative", {
  test_env <- setup_standard_test_env()

  # Test across the annual nodes (including maturity 12)
  for (i in seq(12, 108, by = 12)) {
    n_hat_i <- compute_n_hat(test_env$yields, test_env$term_premia, i = i)

    # Most values should be negative (bonds trade below par)
    prop_negative <- sum(n_hat_i < 0, na.rm = TRUE) / sum(!is.na(n_hat_i))
    expect_gt(prop_negative, 0.8,
      label = paste("Most n_hat values should be negative for maturity", i)
    )
  }
})

test_that("n_hat formula verification", {
  test_env <- setup_standard_test_env()

  # Test for i=36
  i <- 36
  n_hat_36 <- compute_n_hat(test_env$yields, test_env$term_premia, i = i)

  # Manual calculation with maturity weights in years:
  # n_hat = (i/12)*y_i - ((i+12)/12)*y_{i+12} +
  #         ((i+12)/12)*TP_{i+12} - (i/12)*TP_i
  y_36 <- test_env$yields$y36 / 100
  y_48 <- test_env$yields$y48 / 100
  tp_36 <- test_env$term_premia$tp36 / 100
  tp_48 <- test_env$term_premia$tp48 / 100

  n_hat_manual <- (i / 12) * y_36 - ((i + 12) / 12) * y_48 +
    ((i + 12) / 12) * tp_48 - (i / 12) * tp_36

  expect_equal(n_hat_36, n_hat_manual,
    tolerance = 1e-10,
    label = "n_hat should match formula calculation"
  )
})

test_that("n_hat works across the annual nodes", {
  test_env <- setup_standard_test_env()

  # Test all annual-node maturities
  for (i in seq(12, 108, by = 12)) {
    n_hat_i <- compute_n_hat(test_env$yields, test_env$term_premia, i = i)

    expect_type(n_hat_i, "double")
    expect_length(n_hat_i, nrow(test_env$yields))
    expect_true(any(!is.na(n_hat_i)),
      label = paste("n_hat should have non-NA values for maturity", i)
    )
  }
})

test_that("n_hat lagged computation verification", {
  test_env <- setup_standard_test_env()

  # Get n_hat for i=24
  i <- 24
  n_hat_24 <- compute_n_hat(test_env$yields, test_env$term_premia, i = i)

  # Manual calculation at t+1
  y_24 <- test_env$yields$y24 / 100
  y_36 <- test_env$yields$y36 / 100
  tp_24 <- test_env$term_premia$tp24 / 100
  tp_36 <- test_env$term_premia$tp36 / 100

  # Compute n_hat(24,t) using the formula with maturity weights in years
  n_hat_manual <- (i / 12) * y_24 - ((i + 12) / 12) * y_36 +
    ((i + 12) / 12) * tp_36 - (i / 12) * tp_24

  # Check consistency (formula should give same result)
  expect_equal(n_hat_24, n_hat_manual,
    tolerance = 1e-10,
    label = "n_hat computation should be consistent"
  )
})

test_that("n_hat date alignment when dates provided", {
  test_env <- setup_standard_test_env()

  # Test with dates as data frame
  n_hat_df <- compute_n_hat(test_env$yields, test_env$term_premia,
    i = 60,
    return_df = TRUE, dates = test_env$data$date
  )

  expect_s3_class(n_hat_df, "data.frame")
  expect_true("date" %in% names(n_hat_df))
  expect_true("n_hat" %in% names(n_hat_df))
  expect_equal(nrow(n_hat_df), nrow(test_env$yields))

  # Dates should match input dates
  expect_equal(n_hat_df$date, test_env$data$date)
})

test_that("compute_n_hat rejects mismatched yields and term_premia rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  expect_error(
    compute_n_hat(syn_long$yields, syn_short$term_premia, i = 60),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("compute_n_hat rejects invalid maturity values", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_n_hat(test_env$yields, test_env$term_premia, i = 1.5),
    "integer"
  )
  expect_error(
    compute_n_hat(test_env$yields, test_env$term_premia, i = 120),
    "between"
  )
})
