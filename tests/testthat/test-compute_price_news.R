test_that("compute_price_news returns time series of length n-1", {
  test_env <- setup_standard_test_env()

  # Test for maturity 60
  price_news_60 <- compute_price_news(test_env$yields, test_env$term_premia, i = 60)

  expect_type(price_news_60, "double")
  expect_length(price_news_60, nrow(test_env$yields) - 1)
  expect_true(all(is.finite(price_news_60) | is.na(price_news_60)))
})

test_that("compute_price_news works for maturity 12", {
  test_env <- setup_standard_test_env()

  # Test maturity 12 specifically
  price_news_12 <- compute_price_news(test_env$yields, test_env$term_premia, i = 12)

  expect_type(price_news_12, "double")
  expect_length(price_news_12, nrow(test_env$yields) - 1)
  expect_true(all(is.finite(price_news_12) | is.na(price_news_12)))

  # For maturity 12, the price news should have mean near zero
  mean_news <- mean(price_news_12, na.rm = TRUE)
  expect_lt(abs(mean_news), 0.01,
    label = "Price news mean should be small for maturity 12"
  )
})

test_that("price news has mean near zero", {
  test_env <- setup_standard_test_env()

  # Test for multiple maturities (including maturity 12)
  for (i in c(12, 24, 60, 96)) {
    price_news_i <- compute_price_news(test_env$yields, test_env$term_premia, i = i)

    # Mean should be close to 0
    mean_news <- mean(price_news_i, na.rm = TRUE)
    # Allow slightly larger tolerance for maturity 12
    tolerance <- if (i == 12) 0.01 else 0.001
    expect_lt(abs(mean_news), tolerance,
      label = paste("Price news mean should be near 0 for maturity", i)
    )
  }
})

test_that("yield news equals negative of price news", {
  test_env <- setup_standard_test_env()

  # Test for maturity 48
  price_news_48 <- compute_price_news(test_env$yields, test_env$term_premia,
    i = 48,
    return_yield_news = FALSE
  )
  yield_news_48 <- compute_price_news(test_env$yields, test_env$term_premia,
    i = 48,
    return_yield_news = TRUE
  )

  # Yield news should equal negative of price news
  expect_equal(yield_news_48, -price_news_48,
    tolerance = 1e-10,
    label = "Yield news should be negative of price news"
  )
})

test_that("price news is negatively correlated with yield changes", {
  test_env <- setup_standard_test_env()

  # Test for maturity 36
  i <- 36
  price_news_36 <- compute_price_news(test_env$yields, test_env$term_premia, i = i)
  yield_changes_36 <- diff(test_env$yields[[paste0("y", i)]]) / 100 # Convert to decimal

  # Align lengths
  min_len <- min(length(price_news_36), length(yield_changes_36))
  correlation <- cor(price_news_36[1:min_len], yield_changes_36[1:min_len],
    use = "complete.obs"
  )

  expect_lt(correlation, 0,
    label = "Price news should be negatively correlated with yield changes"
  )
})

test_that("price news manual verification", {
  test_env <- setup_standard_test_env()

  # Test for i=36
  i <- 36
  price_news_36 <- compute_price_news(test_env$yields, test_env$term_premia, i = i)

  # Manual calculation: price_news = n_hat(i-12,t) - n_hat(i,t-1)
  n_hat_36 <- compute_n_hat(test_env$yields, test_env$term_premia, i = i)
  n_hat_24 <- compute_n_hat(test_env$yields, test_env$term_premia, i = i - 12)

  # Lag n_hat_36 by one period
  n_hat_36_lagged <- c(NA, n_hat_36[-length(n_hat_36)])

  # Manual price news (removing first observation due to lagging)
  price_news_manual <- n_hat_24[-1] - n_hat_36_lagged[-1]

  expect_equal(price_news_36, price_news_manual,
    tolerance = 1e-10,
    label = "Price news should match manual calculation"
  )
})

test_that("compute_price_news rejects mismatched yields and term_premia rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  expect_error(
    compute_price_news(syn_long$yields, syn_short$term_premia, i = 60),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("compute_price_news rejects invalid maturity values", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_price_news(test_env$yields, test_env$term_premia, i = 1.5),
    "integer"
  )
  expect_error(
    compute_price_news(test_env$yields, test_env$term_premia, i = 120),
    "between"
  )
})
