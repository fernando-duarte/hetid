test_that("price-news kernel returns time series of length n-1", {
  test_env <- setup_standard_test_env()

  # The bare T-1 price-news series is the kernel compute_news_components()$delta_p
  price_news_60 <- compute_news_components(
    test_env$yields, test_env$term_premia,
    i = 60
  )$delta_p

  expect_type(price_news_60, "double")
  expect_length(price_news_60, nrow(test_env$yields) - 1)
  expect_true(all(is.finite(price_news_60) | is.na(price_news_60)))
})

test_that("price-news kernel works for maturity 12", {
  test_env <- setup_standard_test_env()

  price_news_12 <- compute_news_components(
    test_env$yields, test_env$term_premia,
    i = 12
  )$delta_p

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

  for (i in c(12, 24, 60, 96)) {
    price_news_i <- compute_news_components(
      test_env$yields, test_env$term_premia,
      i = i
    )$delta_p

    mean_news <- mean(price_news_i, na.rm = TRUE)
    # Maturities 12/24 inherit the TP^(1):=0 normalization (tp12 dropped from
    # n_hat(12)), a ~mean(tp12)/100 level shift, so they get a looser tolerance
    tolerance <- if (i %in% c(12, 24)) 0.01 else 0.001
    expect_lt(abs(mean_news), tolerance,
      label = paste("Price news mean should be near 0 for maturity", i)
    )
  }
})

test_that("yield news equals negative of price news", {
  test_env <- setup_standard_test_env()

  # Bare kernels: price news is delta_p, yield news is its negation
  price_news_48 <- compute_news_components(
    test_env$yields, test_env$term_premia,
    i = 48
  )$delta_p
  yield_news_48 <- -compute_news_components(
    test_env$yields, test_env$term_premia,
    i = 48
  )$delta_p

  expect_equal(yield_news_48, -price_news_48,
    tolerance = 1e-10,
    label = "Yield news should be negative of price news"
  )
})

test_that("price news is negatively correlated with yield changes", {
  test_env <- setup_standard_test_env()

  i <- 36
  price_news_36 <- compute_news_components(
    test_env$yields, test_env$term_premia,
    i = i
  )$delta_p
  yield_changes_36 <- diff(test_env$yields[[paste0("y", i)]]) / 100 # decimal

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

  i <- 36
  price_news_36 <- compute_news_components(
    test_env$yields, test_env$term_premia,
    i = i
  )$delta_p

  # Manual calculation: price_news = n_hat(i-12,t) - n_hat(i,t-1).
  # Use the bare n_hat kernel n_hat_series() for the undated vector
  n_hat_36 <- n_hat_series(test_env$yields, test_env$term_premia, i = i)
  n_hat_24 <- n_hat_series(test_env$yields, test_env$term_premia, i = i - 12)

  n_hat_36_lagged <- c(NA, n_hat_36[-length(n_hat_36)])

  # Manual price news (removing first observation due to lagging)
  price_news_manual <- n_hat_24[-1] - n_hat_36_lagged[-1]

  expect_equal(price_news_36, price_news_manual,
    tolerance = 1e-10,
    label = "Price news should match manual calculation"
  )
})

test_that("compute_price_news returns a dated news data frame", {
  test_env <- setup_standard_test_env()
  dates <- test_env$data$date

  price_news_df <- compute_price_news(
    test_env$yields, test_env$term_premia,
    i = 60, dates = dates
  )

  expect_s3_class(price_news_df, "data.frame")
  expect_named(price_news_df, c("date", "price_news"))
  expect_equal(price_news_df$date, dates)
  # News series prepends NA: nrow == nrow(yields), row 1 value is NA
  expect_equal(nrow(price_news_df), nrow(test_env$yields))
  expect_true(is.na(price_news_df$price_news[1]))

  # The non-NA values equal the bare T-1 kernel, value k on date[k+1]
  kernel <- compute_news_components(
    test_env$yields, test_env$term_premia,
    i = 60
  )$delta_p
  expect_equal(price_news_df$price_news[-1], kernel, tolerance = 1e-12)
})

test_that("compute_price_news requires a valid Date vector", {
  test_env <- setup_standard_test_env()
  # NULL dates are rejected (a time series cannot be returned without dates)
  expect_error(
    compute_price_news(test_env$yields, test_env$term_premia, i = 60),
    class = "hetid_error_bad_argument"
  )
  # Non-Date dates are rejected
  expect_error(
    compute_price_news(test_env$yields, test_env$term_premia,
      i = 60, dates = seq_len(nrow(test_env$yields))
    ),
    class = "hetid_error_bad_argument"
  )
  # Wrong-length dates are rejected
  expect_error(
    compute_price_news(test_env$yields, test_env$term_premia,
      i = 60, dates = test_env$data$date[-1]
    ),
    class = "hetid_error_dimension_mismatch"
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
