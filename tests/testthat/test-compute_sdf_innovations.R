test_that("compute_sdf_innovations returns time series", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for maturity 5
  sdf_innov_5 <- compute_sdf_innovations(yields, term_premia, i = 5)

  expect_type(sdf_innov_5, "double")
  expect_true(is.vector(sdf_innov_5))
  expect_true(all(is.finite(sdf_innov_5) | is.na(sdf_innov_5)))
})

test_that("SDF innovations have mean near zero", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for multiple maturities
  for (i in c(2, 5, 8)) {
    sdf_innov_i <- compute_sdf_innovations(yields, term_premia, i = i)

    # Mean should be close to 0
    mean_innov <- mean(sdf_innov_i, na.rm = TRUE)
    expect_lt(abs(mean_innov), 0.01,
      label = paste("SDF innovations mean should be near 0 for maturity", i)
    )
  }
})

test_that("SDF innovations highly correlated with price news", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for maturity 4
  i <- 4
  price_news_4 <- compute_price_news(yields, term_premia, i = i)
  sdf_innov_4 <- compute_sdf_innovations(yields, term_premia, i = i)

  # Should have same length
  expect_equal(length(sdf_innov_4), length(price_news_4))

  # Should be highly correlated
  correlation <- cor(price_news_4, sdf_innov_4, use = "complete.obs")
  expect_gt(correlation, 0.8,
    label = "SDF innovations should be highly correlated with price news"
  )
})

test_that("SDF innovations length is n-1", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test for multiple maturities
  for (i in 1:9) {
    sdf_innov_i <- compute_sdf_innovations(yields, term_premia, i = i)

    expect_equal(length(sdf_innov_i), nrow(yields) - 1,
      label = paste("SDF innovations should have length n-1 for maturity", i)
    )
  }
})
