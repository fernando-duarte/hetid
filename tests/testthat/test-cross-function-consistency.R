test_that("all functions handle same ACM data format", {
  test_env <- setup_standard_test_env()
  dates <- test_env$data$date

  # Test that all functions work with the same data
  i <- 60

  # Scalar functions return doubles directly
  expect_type(compute_c_hat(test_env$yields, test_env$term_premia, i = i), "double")
  expect_type(compute_k_hat(test_env$yields, test_env$term_premia, i = i), "double")
  expect_type(compute_variance_bound(test_env$yields, test_env$term_premia, i = i), "double")
  expect_type(
    compute_expected_sdf_variance_bound(test_env$yields, test_env$term_premia, i = i),
    "double"
  )

  # The series functions now return dated data frames; their bare numeric
  # cores are the kernels (n_hat_series, compute_news_components()$delta_p,
  # sdf_innovations_series). compute_expected_sdf has no kernel, so take the
  # value column of its dated data frame.
  expect_type(n_hat_series(test_env$yields, test_env$term_premia, i = i), "double")
  expect_type(
    compute_news_components(test_env$yields, test_env$term_premia, i = i)$delta_p,
    "double"
  )
  expect_type(
    sdf_innovations_series(test_env$yields, test_env$term_premia, i = i),
    "double"
  )
  expect_type(
    compute_expected_sdf(
      test_env$yields, test_env$term_premia,
      i = i, dates = dates
    )$expected_sdf,
    "double"
  )
})

test_that("functions with date parameters align correctly", {
  test_env <- setup_standard_test_env()
  dates <- test_env$data$date

  # Test n_hat with dates (level series: one row per date, no NA prepend)
  n_hat_df <- compute_n_hat(test_env$yields, test_env$term_premia,
    i = 60, dates = dates
  )

  expect_s3_class(n_hat_df, "data.frame")
  expect_equal(n_hat_df$date, dates)
  expect_equal(nrow(n_hat_df), length(dates))

  # Test price news with dates (news series: NA-prepended to length nrow)
  price_news_df <- compute_price_news(test_env$yields, test_env$term_premia,
    i = 60, dates = dates
  )

  expect_s3_class(price_news_df, "data.frame")
  expect_equal(price_news_df$date, dates)
  # Price news returns a data frame with same number of rows as input
  # The first row has NA because price news can't be computed for t=0
  expect_equal(nrow(price_news_df), length(dates))
  expect_true(is.na(price_news_df$price_news[1]))
})

test_that("consistent NA handling across functions", {
  test_env <- setup_standard_test_env()
  yields <- test_env$yields
  term_premia <- test_env$term_premia
  dates <- test_env$data$date

  # Introduce NAs
  yields[c(10, 20, 30), "y60"] <- NA
  term_premia[c(15, 25), "tp60"] <- NA

  # All functions should handle NAs gracefully. Scalars return doubles;
  # series functions are exercised via their bare kernels (and the dated
  # expected-SDF data frame, which has no kernel).
  expect_type(compute_c_hat(yields, term_premia, i = 60), "double")
  expect_type(compute_k_hat(yields, term_premia, i = 60), "double")
  expect_type(n_hat_series(yields, term_premia, i = 60), "double")
  expect_type(
    compute_news_components(yields, term_premia, i = 60)$delta_p,
    "double"
  )
  expect_type(sdf_innovations_series(yields, term_premia, i = 60), "double")
  expect_type(compute_variance_bound(yields, term_premia, i = 60), "double")
  expect_type(
    compute_expected_sdf(yields, term_premia, i = 60, dates = dates)$expected_sdf,
    "double"
  )
  expect_type(
    compute_expected_sdf_variance_bound(yields, term_premia, i = 60),
    "double"
  )
})

test_that("invalid inputs produce appropriate errors", {
  test_env <- setup_standard_test_env()

  # Test invalid maturity
  expect_error(
    compute_c_hat(test_env$yields, test_env$term_premia, i = 0),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_k_hat(test_env$yields, test_env$term_premia, i = -1),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_n_hat(test_env$yields, test_env$term_premia, i = 132),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_price_news(test_env$yields, test_env$term_premia, i = 180),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_sdf_innovations(test_env$yields, test_env$term_premia, i = 0),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_variance_bound(test_env$yields, test_env$term_premia, i = -5),
    class = "hetid_error_bad_argument"
  )
})

test_that("all functions complete without error on larger stacked data", {
  test_env <- setup_standard_test_env()

  # Repeat data to make it larger (but not too large for testing)
  large_data <- rbind(test_env$data, test_env$data, test_env$data)
  large_extracted <- extract_yields_and_tp(large_data)
  yields <- large_extracted$yields
  term_premia <- large_extracted$term_premia
  dates <- large_data$date

  # All functions should complete without error. Series functions via the
  # bare kernels; expected_sdf via its dated data frame.
  i <- 60
  expect_type(compute_c_hat(yields, term_premia, i = i), "double")
  expect_type(compute_k_hat(yields, term_premia, i = i), "double")
  expect_type(n_hat_series(yields, term_premia, i = i), "double")
  expect_type(
    compute_news_components(yields, term_premia, i = i)$delta_p,
    "double"
  )
  expect_type(sdf_innovations_series(yields, term_premia, i = i), "double")
  expect_type(compute_variance_bound(yields, term_premia, i = i), "double")
  expect_type(
    compute_expected_sdf(yields, term_premia, i = i, dates = dates)$expected_sdf,
    "double"
  )
  expect_type(
    compute_expected_sdf_variance_bound(yields, term_premia, i = i),
    "double"
  )
})

test_that("similar functions have consistent return structures", {
  test_env <- setup_standard_test_env()
  dates <- test_env$data$date

  # Scalar returns: c_hat, k_hat, variance_bound
  c_hat <- compute_c_hat(test_env$yields, test_env$term_premia, i = 60)
  k_hat <- compute_k_hat(test_env$yields, test_env$term_premia, i = 60)
  var_bound <- compute_variance_bound(test_env$yields, test_env$term_premia, i = 60)
  esdf_bound <- compute_expected_sdf_variance_bound(test_env$yields, test_env$term_premia, i = 60)

  expect_length(c_hat, 1)
  expect_length(k_hat, 1)
  expect_length(var_bound, 1)
  expect_length(esdf_bound, 1)

  # Series returns: n_hat (level), price_news / sdf_innovations (news).
  # Bare kernels carry the undated numeric series.
  n_hat <- n_hat_series(test_env$yields, test_env$term_premia, i = 60)
  price_news <- compute_news_components(
    test_env$yields, test_env$term_premia,
    i = 60
  )$delta_p
  sdf_innov <- sdf_innovations_series(test_env$yields, test_env$term_premia, i = 60)

  expect_true(length(n_hat) > 1)
  expect_true(length(price_news) > 1)
  expect_true(length(sdf_innov) > 1)

  # n_hat kernel has same length as input, news kernels have n-1
  expect_length(n_hat, nrow(test_env$yields))
  expect_length(price_news, nrow(test_env$yields) - 1)
  expect_length(sdf_innov, nrow(test_env$yields) - 1)

  # expected_sdf has no kernel; its dated data frame is a level series with
  # one row per date (value column the same length as the input rows)
  expected_sdf_df <- compute_expected_sdf(
    test_env$yields, test_env$term_premia,
    i = 60, dates = dates
  )
  expect_s3_class(expected_sdf_df, "data.frame")
  expect_true(length(expected_sdf_df$expected_sdf) > 1)
  expect_length(expected_sdf_df$expected_sdf, nrow(test_env$yields))
})

test_that("ACM data is quarterly when requested", {
  # Test monthly vs quarterly
  monthly_data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "monthly"
  )

  quarterly_data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly",
    use_incomplete_quarters = FALSE
  )

  # Quarterly should have fewer observations
  expect_lt(nrow(quarterly_data), nrow(monthly_data))

  # Ratio should be approximately 3:1
  ratio <- nrow(monthly_data) / nrow(quarterly_data)
  expect_gt(ratio, 2.8)
  expect_lt(ratio, 3.2)
})

test_that("quarterly data uses last month of quarter", {
  # Load quarterly data
  quarterly_data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly",
    use_incomplete_quarters = FALSE
  )

  # Check all dates are end of quarter months
  months <- as.numeric(format(quarterly_data$date, "%m"))
  expect_true(all(months %in% c(3, 6, 9, 12)))

  # Load monthly data to verify
  monthly_data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "monthly"
  )

  # Find matching dates
  matching_dates <- quarterly_data$date %in% monthly_data$date
  expect_true(all(matching_dates))

  # For a sample of quarterly dates, verify they match the monthly data
  sample_dates <- quarterly_data$date[seq_len(min(10, nrow(quarterly_data)))]
  for (date in sample_dates) {
    # Find the quarterly value
    q_row <- quarterly_data[quarterly_data$date == date, ]
    # Find the monthly value
    m_row <- monthly_data[monthly_data$date == date, ]

    if (nrow(m_row) > 0 && nrow(q_row) > 0) {
      # Values should match exactly
      expect_equal(q_row$y12, m_row$y12,
        label = paste("Quarterly and monthly values should match for", date)
      )
    }
  }
})
