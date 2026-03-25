test_that("compute_n_hat returns time series of expected log bond prices", {
  test_env <- setup_standard_test_env()

  # Test for maturity 5
  n_hat_5 <- compute_n_hat(test_env$yields, test_env$term_premia, i = 5)

  expect_type(n_hat_5, "double")
  expect_length(n_hat_5, nrow(test_env$yields))
  expect_true(all(is.finite(n_hat_5) | is.na(n_hat_5)))
})

test_that("n_hat should generally be negative", {
  test_env <- setup_standard_test_env()

  # Test for multiple maturities (including maturity 1)
  for (i in 1:9) {
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

  # Test for i=3
  i <- 3
  n_hat_3 <- compute_n_hat(test_env$yields, test_env$term_premia, i = i)

  # Manual calculation: n_hat = i*y_i - (i+1)*y_{i+1} + (i+1)*TP_{i+1} - i*TP_i
  y_3 <- test_env$yields$y3 / 100
  y_4 <- test_env$yields$y4 / 100
  tp_3 <- test_env$term_premia$tp3 / 100
  tp_4 <- test_env$term_premia$tp4 / 100

  n_hat_manual <- i * y_3 - (i + 1) * y_4 + (i + 1) * tp_4 - i * tp_3

  expect_equal(n_hat_3, n_hat_manual,
    tolerance = 1e-10,
    label = "n_hat should match formula calculation"
  )
})

test_that("n_hat works for all maturities 1-9", {
  test_env <- setup_standard_test_env()

  # Test all maturities
  for (i in 1:9) {
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

  # Get n_hat for i=2
  i <- 2
  n_hat_2 <- compute_n_hat(test_env$yields, test_env$term_premia, i = i)

  # Manual calculation at t+1
  y_2 <- test_env$yields$y2 / 100
  y_3 <- test_env$yields$y3 / 100
  tp_2 <- test_env$term_premia$tp2 / 100
  tp_3 <- test_env$term_premia$tp3 / 100

  # Compute n_hat(2,t) using the formula
  n_hat_manual <- i * y_2 - (i + 1) * y_3 + (i + 1) * tp_3 - i * tp_2

  # Check consistency (formula should give same result)
  expect_equal(n_hat_2, n_hat_manual,
    tolerance = 1e-10,
    label = "n_hat computation should be consistent"
  )
})

test_that("n_hat date alignment when dates provided", {
  test_env <- setup_standard_test_env()

  # Test with dates as data frame
  n_hat_df <- compute_n_hat(test_env$yields, test_env$term_premia,
    i = 5,
    return_df = TRUE, dates = test_env$data$date
  )

  expect_s3_class(n_hat_df, "data.frame")
  expect_true("date" %in% names(n_hat_df))
  expect_true("n_hat" %in% names(n_hat_df))
  expect_equal(nrow(n_hat_df), nrow(test_env$yields))

  # Dates should match input dates
  expect_equal(n_hat_df$date, test_env$data$date)
})

test_that("compute_n_hat rejects invalid maturity values", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_n_hat(test_env$yields, test_env$term_premia, i = 1.5),
    "integer"
  )
  expect_error(
    compute_n_hat(test_env$yields, test_env$term_premia, i = 10),
    "between"
  )
})
