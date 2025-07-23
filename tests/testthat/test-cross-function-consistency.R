test_that("all functions handle same ACM data format", {
  # Load test data once
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test that all functions work with the same data
  i <- 5

  # All these should work without error
  expect_type(compute_c_hat(yields, term_premia, i = i), "double")
  expect_type(compute_k_hat(yields, term_premia, i = i), "double")
  expect_type(compute_n_hat(yields, term_premia, i = i), "double")
  expect_type(compute_price_news(yields, term_premia, i = i), "double")
  expect_type(compute_sdf_innovations(yields, term_premia, i = i), "double")
  expect_type(compute_variance_bound(yields, term_premia, i = i), "double")
})

test_that("functions with date parameters align correctly", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]
  dates <- data$date

  # Test n_hat with dates
  n_hat_df <- compute_n_hat(yields, term_premia,
    i = 5,
    return_df = TRUE, dates = dates
  )

  expect_s3_class(n_hat_df, "data.frame")
  expect_equal(n_hat_df$date, dates)

  # Test price news with dates
  price_news_df <- compute_price_news(yields, term_premia,
    i = 5,
    return_df = TRUE, dates = dates
  )

  expect_s3_class(price_news_df, "data.frame")
  # Price news returns a data frame with same number of rows as input
  # The first row has NA because price news can't be computed for t=0
  expect_equal(nrow(price_news_df), length(dates))
  expect_true(is.na(price_news_df$price_news[1]))
})

test_that("consistent NA handling across functions", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Introduce NAs
  yields[c(10, 20, 30), "y5"] <- NA
  term_premia[c(15, 25), "tp5"] <- NA

  # All functions should handle NAs gracefully
  expect_type(compute_c_hat(yields, term_premia, i = 5), "double")
  expect_type(compute_k_hat(yields, term_premia, i = 5), "double")
  expect_type(compute_n_hat(yields, term_premia, i = 5), "double")
  expect_type(compute_price_news(yields, term_premia, i = 5), "double")
  expect_type(compute_sdf_innovations(yields, term_premia, i = 5), "double")
  expect_type(compute_variance_bound(yields, term_premia, i = 5), "double")
})

test_that("invalid inputs produce appropriate errors", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Test invalid maturity
  expect_error(compute_c_hat(yields, term_premia, i = 0))
  expect_error(compute_k_hat(yields, term_premia, i = -1))
  expect_error(compute_n_hat(yields, term_premia, i = 11))
  expect_error(compute_price_news(yields, term_premia, i = 15))
  expect_error(compute_sdf_innovations(yields, term_premia, i = 0))
  expect_error(compute_variance_bound(yields, term_premia, i = -5))
})

test_that("functions handle large datasets efficiently", {
  # Create a larger dataset by repeating data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )

  # Repeat data to make it larger (but not too large for testing)
  large_data <- rbind(data, data, data)
  yields <- large_data[, grep("^y", names(large_data))]
  term_premia <- large_data[, grep("^tp", names(large_data))]

  # All functions should complete without error
  i <- 5
  expect_type(compute_c_hat(yields, term_premia, i = i), "double")
  expect_type(compute_k_hat(yields, term_premia, i = i), "double")
  expect_type(compute_n_hat(yields, term_premia, i = i), "double")
  expect_type(compute_price_news(yields, term_premia, i = i), "double")
  expect_type(compute_sdf_innovations(yields, term_premia, i = i), "double")
  expect_type(compute_variance_bound(yields, term_premia, i = i), "double")
})

test_that("similar functions have consistent return structures", {
  # Load test data
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
  )
  yields <- data[, grep("^y", names(data))]
  term_premia <- data[, grep("^tp", names(data))]

  # Scalar returns: c_hat, k_hat, variance_bound
  c_hat <- compute_c_hat(yields, term_premia, i = 5)
  k_hat <- compute_k_hat(yields, term_premia, i = 5)
  var_bound <- compute_variance_bound(yields, term_premia, i = 5)

  expect_length(c_hat, 1)
  expect_length(k_hat, 1)
  expect_length(var_bound, 1)

  # Vector returns: n_hat, price_news, sdf_innovations
  n_hat <- compute_n_hat(yields, term_premia, i = 5)
  price_news <- compute_price_news(yields, term_premia, i = 5)
  sdf_innov <- compute_sdf_innovations(yields, term_premia, i = 5)

  expect_true(length(n_hat) > 1)
  expect_true(length(price_news) > 1)
  expect_true(length(sdf_innov) > 1)

  # n_hat has same length as input, others have n-1
  expect_length(n_hat, nrow(yields))
  expect_length(price_news, nrow(yields) - 1)
  expect_length(sdf_innov, nrow(yields) - 1)
})

test_that("ACM data is quarterly when requested", {
  # Test monthly vs quarterly
  monthly_data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "monthly"
  )

  quarterly_data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly"
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
    frequency = "quarterly"
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
  sample_dates <- quarterly_data$date[1:min(10, nrow(quarterly_data))]
  for (date in sample_dates) {
    # Find the quarterly value
    q_row <- quarterly_data[quarterly_data$date == date, ]
    # Find the monthly value
    m_row <- monthly_data[monthly_data$date == date, ]

    if (nrow(m_row) > 0 && nrow(q_row) > 0) {
      # Values should match exactly
      expect_equal(q_row$y1, m_row$y1,
        label = paste("Quarterly and monthly values should match for", date)
      )
    }
  }
})
