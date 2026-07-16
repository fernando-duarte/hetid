test_that("compute_n_hat returns time series of expected log bond prices", {
  test_env <- setup_standard_test_env()

  n_hat_60 <- n_hat_series(test_env$yields, test_env$term_premia, i = 60)

  expect_type(n_hat_60, "double")
  expect_length(n_hat_60, nrow(test_env$yields))
  expect_true(all(is.finite(n_hat_60) | is.na(n_hat_60)))
})

test_that("n_hat should generally be negative", {
  test_env <- setup_standard_test_env()

  for (i in seq(12, 108, by = 12)) {
    n_hat_i <- n_hat_series(test_env$yields, test_env$term_premia, i = i)

    # Most values should be negative (bonds trade below par)
    prop_negative <- sum(n_hat_i < 0, na.rm = TRUE) / sum(!is.na(n_hat_i))
    expect_gt(prop_negative, 0.8,
      label = paste("Most n_hat values should be negative for maturity", i)
    )
  }
})

test_that("n_hat formula verification", {
  test_env <- setup_standard_test_env()

  i <- 36
  n_hat_36 <- n_hat_series(test_env$yields, test_env$term_premia, i = i)

  # maturity weights in years:
  # n_hat = (i/12)*y_i - ((i+12)/12)*y_{i+12} + ((i+12)/12)*TP_{i+12} - (i/12)*TP_i
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

  for (i in seq(12, 108, by = 12)) {
    n_hat_i <- n_hat_series(test_env$yields, test_env$term_premia, i = i)

    expect_type(n_hat_i, "double")
    expect_length(n_hat_i, nrow(test_env$yields))
    expect_true(any(!is.na(n_hat_i)),
      label = paste("n_hat should have non-NA values for maturity", i)
    )
  }
})

test_that("n_hat lagged computation verification", {
  test_env <- setup_standard_test_env()

  i <- 24
  n_hat_24 <- n_hat_series(test_env$yields, test_env$term_premia, i = i)

  y_24 <- test_env$yields$y24 / 100
  y_36 <- test_env$yields$y36 / 100
  tp_24 <- test_env$term_premia$tp24 / 100
  tp_36 <- test_env$term_premia$tp36 / 100

  # Compute n_hat(24,t) using the formula with maturity weights in years
  n_hat_manual <- (i / 12) * y_24 - ((i + 12) / 12) * y_36 +
    ((i + 12) / 12) * tp_36 - (i / 12) * tp_24

  expect_equal(n_hat_24, n_hat_manual,
    tolerance = 1e-10,
    label = "n_hat computation should be consistent"
  )
})

test_that("n_hat date alignment when dates provided", {
  test_env <- setup_standard_test_env()

  # Test with dates supplied: always a dated data frame
  n_hat_df <- compute_n_hat(test_env$yields, test_env$term_premia,
    i = 60,
    dates = test_env$data$date
  )

  expect_s3_class(n_hat_df, "data.frame")
  expect_true("date" %in% names(n_hat_df))
  expect_true("n_hat" %in% names(n_hat_df))
  expect_equal(nrow(n_hat_df), nrow(test_env$yields))

  expect_equal(n_hat_df$date, test_env$data$date)
})

test_that("compute_n_hat rejects mismatched yields and term_premia rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  expect_error(
    n_hat_series(syn_long$yields, syn_short$term_premia, i = 60),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("compute_n_hat rejects invalid maturity values", {
  test_env <- setup_standard_test_env()
  expect_error(
    n_hat_series(test_env$yields, test_env$term_premia, i = 1.5),
    "integer"
  )
  expect_error(
    n_hat_series(test_env$yields, test_env$term_premia, i = 120),
    "between"
  )
})

test_that("n_hat imposes TP^(1):=0 at the one-period maturity (i == step)", {
  test_env <- setup_standard_test_env()
  units <- HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  pct <- HETID_CONSTANTS$PERCENT_TO_DECIMAL

  # At i == step (= 12) the step-maturity term premium (tp12) is dropped:
  # the one-period bond carries no term premium by definition
  n_hat_12 <- n_hat_series(test_env$yields, test_env$term_premia, i = 12)

  expected <- ((12 / units) * test_env$yields$y12 -
    (24 / units) * test_env$yields$y24 +
    (24 / units) * test_env$term_premia$tp24) / pct
  expect_equal(n_hat_12, expected,
    tolerance = 1e-12,
    label = "n_hat at i == step drops the step-maturity term premium"
  )

  # The retained-tp12 formula must differ (tp12 is nonzero in ACM data)
  with_tp12 <- expected - (12 / units) * test_env$term_premia$tp12 / pct
  expect_false(isTRUE(all.equal(n_hat_12, with_tp12)),
    label = "dropping tp12 must change n_hat for nonzero tp12"
  )
})

test_that("compute_n_hat warns when yields look like decimals, not percent", {
  test_env <- setup_standard_test_env()

  # Percentage-point inputs (max |yield| > 1): no units warning
  expect_no_warning(
    n_hat_series(test_env$yields, test_env$term_premia, i = 60)
  )

  # Decimal inputs (divide by 100, max |yield| < 1): warn about units
  expect_warning(
    n_hat_series(test_env$yields / 100, test_env$term_premia, i = 60),
    "decimals"
  )
})
