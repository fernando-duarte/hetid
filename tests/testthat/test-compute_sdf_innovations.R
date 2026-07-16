test_that("sdf-innovations kernel returns time series", {
  test_env <- setup_standard_test_env()

  # Bare T-1 news series is the kernel sdf_innovations_series()
  sdf_innov_60 <- sdf_innovations_series(
    test_env$yields, test_env$term_premia,
    i = 60
  )

  expect_type(sdf_innov_60, "double")
  expect_true(is.vector(sdf_innov_60))
  expect_true(all(is.finite(sdf_innov_60) | is.na(sdf_innov_60)))
})

test_that("SDF innovations have mean near zero", {
  test_env <- setup_standard_test_env()

  for (i in c(12, 24, 60, 96)) {
    sdf_innov_i <- sdf_innovations_series(
      test_env$yields, test_env$term_premia,
      i = i
    )

    mean_innov <- mean(sdf_innov_i, na.rm = TRUE)
    expect_lt(abs(mean_innov), 0.01,
      label = paste("SDF innovations mean should be near 0 for maturity", i)
    )
  }
})

test_that("SDF innovations highly correlated with price news", {
  test_env <- setup_standard_test_env()

  # Compare the two bare kernels (price-news delta_p vs sdf innovations)
  i <- 48
  price_news_48 <- compute_news_components(
    test_env$yields, test_env$term_premia,
    i = i
  )$delta_p
  sdf_innov_48 <- sdf_innovations_series(
    test_env$yields, test_env$term_premia,
    i = i
  )

  expect_equal(length(sdf_innov_48), length(price_news_48))

  correlation <- cor(price_news_48, sdf_innov_48, use = "complete.obs")
  expect_gt(correlation, 0.8,
    label = "SDF innovations should be highly correlated with price news"
  )
})

test_that("SDF innovations length is n-1", {
  test_env <- setup_standard_test_env()

  for (i in seq(12, 108, by = 12)) {
    sdf_innov_i <- sdf_innovations_series(
      test_env$yields, test_env$term_premia,
      i = i
    )

    expect_equal(length(sdf_innov_i), nrow(test_env$yields) - 1,
      label = paste("SDF innovations should have length n-1 for maturity", i)
    )
  }
})

test_that("compute_sdf_innovations returns a dated news data frame", {
  test_env <- setup_standard_test_env()
  dates <- test_env$data$date

  sdf_df <- compute_sdf_innovations(
    test_env$yields, test_env$term_premia,
    i = 60, dates = dates
  )

  expect_s3_class(sdf_df, "data.frame")
  expect_named(sdf_df, c("date", "sdf_innovations"))
  expect_equal(sdf_df$date, dates)
  # News series prepends NA: nrow == nrow(yields), row 1 value is NA
  expect_equal(nrow(sdf_df), nrow(test_env$yields))
  expect_true(is.na(sdf_df$sdf_innovations[1]))

  # The non-NA values equal the bare T-1 kernel, value k on date[k+1]
  kernel <- sdf_innovations_series(test_env$yields, test_env$term_premia, i = 60)
  expect_equal(sdf_df$sdf_innovations[-1], kernel, tolerance = 1e-12)
})

test_that("compute_sdf_innovations requires a valid Date vector", {
  test_env <- setup_standard_test_env()
  # NULL dates are rejected
  expect_error(
    compute_sdf_innovations(test_env$yields, test_env$term_premia, i = 60),
    class = "hetid_error_bad_argument"
  )
  # Non-Date dates are rejected
  expect_error(
    compute_sdf_innovations(test_env$yields, test_env$term_premia,
      i = 60, dates = seq_len(nrow(test_env$yields))
    ),
    class = "hetid_error_bad_argument"
  )
  # Wrong-length dates are rejected
  expect_error(
    compute_sdf_innovations(test_env$yields, test_env$term_premia,
      i = 60, dates = test_env$data$date[-1]
    ),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("compute_sdf_innovations rejects mismatched yields and term_premia rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  # Supply valid dates (length nrow(yields)) so execution reaches the
  # row-alignment check that this test targets
  dts <- seq(as.Date("1990-03-31"), by = "quarter", length.out = 30)
  expect_error(
    compute_sdf_innovations(syn_long$yields, syn_short$term_premia,
      i = 60, dates = dts
    ),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("compute_sdf_innovations rejects invalid maturity values", {
  test_env <- setup_standard_test_env()
  dates <- test_env$data$date
  expect_error(
    compute_sdf_innovations(test_env$yields, test_env$term_premia,
      i = 1.5, dates = dates
    ),
    "integer"
  )
  expect_error(
    compute_sdf_innovations(test_env$yields, test_env$term_premia,
      i = 120, dates = dates
    ),
    "between"
  )
})

test_that("SDF innovations use the constant centering B_i (subtracted outside e^mu)", {
  test_env <- setup_standard_test_env()
  i <- 48

  # Bare kernels: sdf innovations, n_hat level, and price-news delta_p
  sdf <- sdf_innovations_series(test_env$yields, test_env$term_premia, i = i)
  n_hat_i <- n_hat_series(test_env$yields, test_env$term_premia, i = i)
  delta_p <- compute_news_components(
    test_env$yields, test_env$term_premia,
    i = i
  )$delta_p

  # Spec centering: B_i = 0.5 * mean(e^{mu} * delta_p^2) subtracted
  # outside the e^{mu} factor (a constant, not a time-varying term)
  exp_mu <- exp(n_hat_i[seq_along(delta_p)])
  valid <- !is.na(exp_mu) & !is.na(delta_p)
  b_hat <- 0.5 * mean(exp_mu[valid] * delta_p[valid]^2)
  expected <- exp_mu * (delta_p + 0.5 * delta_p^2) - b_hat

  expect_equal(sdf, expected,
    tolerance = 1e-12,
    label = "SDF innovation = e^mu(dp + 0.5 dp^2) - 0.5 mean(e^mu dp^2)"
  )
})
