test_that("compute_price_news returns time series of length n-1", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for maturity 5
  price_news_5 <- compute_price_news(yields, term_premia, i = 5)

  expect_type(price_news_5, "double")
  expect_length(price_news_5, nrow(yields) - 1)
  expect_true(all(is.finite(price_news_5) | is.na(price_news_5)))
})

test_that("compute_price_news works for maturity 1", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test maturity 1 specifically
  price_news_1 <- compute_price_news(yields, term_premia, i = 1)

  expect_type(price_news_1, "double")
  expect_length(price_news_1, nrow(yields) - 1)
  expect_true(all(is.finite(price_news_1) | is.na(price_news_1)))

  # For maturity 1, the price news should have mean near zero
  mean_news <- mean(price_news_1, na.rm = TRUE)
  expect_lt(abs(mean_news), 0.01,
    label = "Price news mean should be small for maturity 1"
  )
})

test_that("price news has mean near zero", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for multiple maturities (including maturity 1)
  for (i in c(1, 2, 5, 8)) {
    price_news_i <- compute_price_news(yields, term_premia, i = i)

    # Mean should be close to 0
    mean_news <- mean(price_news_i, na.rm = TRUE)
    # Allow slightly larger tolerance for maturity 1
    tolerance <- if (i == 1) 0.01 else 0.001
    expect_lt(abs(mean_news), tolerance,
      label = paste("Price news mean should be near 0 for maturity", i)
    )
  }
})

test_that("yield news equals negative of price news", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for maturity 4
  price_news_4 <- compute_price_news(yields, term_premia,
    i = 4,
    return_yield_news = FALSE
  )
  yield_news_4 <- compute_price_news(yields, term_premia,
    i = 4,
    return_yield_news = TRUE
  )

  # Yield news should equal negative of price news
  expect_equal(yield_news_4, -price_news_4,
    tolerance = 1e-10,
    label = "Yield news should be negative of price news"
  )
})

test_that("price news is negatively correlated with yield changes", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for maturity 3
  i <- 3
  price_news_3 <- compute_price_news(yields, term_premia, i = i)
  yield_changes_3 <- diff(yields[[paste0("y", i)]]) / 100 # Convert to decimal

  # Align lengths
  min_len <- min(length(price_news_3), length(yield_changes_3))
  correlation <- cor(price_news_3[1:min_len], yield_changes_3[1:min_len],
    use = "complete.obs"
  )

  expect_lt(correlation, 0,
    label = "Price news should be negatively correlated with yield changes"
  )
})

test_that("price news manual verification", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for i=3
  i <- 3
  price_news_3 <- compute_price_news(yields, term_premia, i = i)

  # Manual calculation: price_news = n_hat(i-1,t) - n_hat(i,t-1)
  n_hat_3 <- compute_n_hat(yields, term_premia, i = i)
  n_hat_2 <- compute_n_hat(yields, term_premia, i = i - 1)

  # Lag n_hat_3 by one period
  n_hat_3_lagged <- c(NA, n_hat_3[-length(n_hat_3)])

  # Manual price news (removing first observation due to lagging)
  price_news_manual <- n_hat_2[-1] - n_hat_3_lagged[-1]

  expect_equal(price_news_3, price_news_manual,
    tolerance = 1e-10,
    label = "Price news should match manual calculation"
  )
})
