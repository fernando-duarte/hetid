test_that("compute_sdf_innovations returns time series", {
  test_env <- setup_standard_test_env()

  # Test for maturity 60
  sdf_innov_60 <- compute_sdf_innovations(test_env$yields, test_env$term_premia, i = 60)

  expect_type(sdf_innov_60, "double")
  expect_true(is.vector(sdf_innov_60))
  expect_true(all(is.finite(sdf_innov_60) | is.na(sdf_innov_60)))
})

test_that("SDF innovations have mean near zero", {
  test_env <- setup_standard_test_env()

  # Test for multiple maturities (including maturity 12)
  for (i in c(12, 24, 60, 96)) {
    sdf_innov_i <- compute_sdf_innovations(test_env$yields, test_env$term_premia, i = i)

    # Mean should be close to 0
    mean_innov <- mean(sdf_innov_i, na.rm = TRUE)
    expect_lt(abs(mean_innov), 0.01,
      label = paste("SDF innovations mean should be near 0 for maturity", i)
    )
  }
})

test_that("SDF innovations highly correlated with price news", {
  test_env <- setup_standard_test_env()

  # Test for maturity 48
  i <- 48
  price_news_48 <- compute_price_news(test_env$yields, test_env$term_premia, i = i)
  sdf_innov_48 <- compute_sdf_innovations(test_env$yields, test_env$term_premia, i = i)

  # Should have same length
  expect_equal(length(sdf_innov_48), length(price_news_48))

  # Should be highly correlated
  correlation <- cor(price_news_48, sdf_innov_48, use = "complete.obs")
  expect_gt(correlation, 0.8,
    label = "SDF innovations should be highly correlated with price news"
  )
})

test_that("SDF innovations length is n-1", {
  test_env <- setup_standard_test_env()

  # Test across the annual nodes
  for (i in seq(12, 108, by = 12)) {
    sdf_innov_i <- compute_sdf_innovations(test_env$yields, test_env$term_premia, i = i)

    expect_equal(length(sdf_innov_i), nrow(test_env$yields) - 1,
      label = paste("SDF innovations should have length n-1 for maturity", i)
    )
  }
})

test_that("compute_sdf_innovations rejects mismatched yields and term_premia rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  expect_error(
    compute_sdf_innovations(syn_long$yields, syn_short$term_premia, i = 60),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("compute_sdf_innovations rejects invalid maturity values", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_sdf_innovations(test_env$yields, test_env$term_premia, i = 1.5),
    "integer"
  )
  expect_error(
    compute_sdf_innovations(test_env$yields, test_env$term_premia, i = 120),
    "between"
  )
})
