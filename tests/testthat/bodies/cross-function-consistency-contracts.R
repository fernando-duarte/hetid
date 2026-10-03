{
  test_env <- setup_standard_test_env()
  dates <- test_env$data$date

  i <- 60

  expect_type(compute_c_hat(test_env$yields, test_env$term_premia, i = i), "double")
  expect_type(compute_k_hat(test_env$yields, test_env$term_premia, i = i), "double")
  expect_type(compute_variance_bound(test_env$yields, test_env$term_premia, i = i), "double")
  expect_type(compute_news_q_bound(test_env$yields, test_env$term_premia, i = i), "double")
  expect_type(
    compute_expected_sdf_variance_bound(test_env$yields, test_env$term_premia, i = i),
    "double"
  )

  # Series functions return dated frames; use their bare kernels (n_hat_series,
  # news delta_p, sdf_innovations_series). expected_sdf has none: take its column
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
}

{
  test_env <- setup_standard_test_env()
  dates <- test_env$data$date

  c_hat <- compute_c_hat(test_env$yields, test_env$term_premia, i = 60)
  k_hat <- compute_k_hat(test_env$yields, test_env$term_premia, i = 60)
  var_bound <- compute_variance_bound(test_env$yields, test_env$term_premia, i = 60)
  esdf_bound <- compute_expected_sdf_variance_bound(test_env$yields, test_env$term_premia, i = 60)

  expect_length(c_hat, 1)
  expect_length(k_hat, 1)
  expect_length(var_bound, 1)
  expect_length(esdf_bound, 1)

  # Series returns: n_hat (level), price_news / sdf_innovations (news).
  # Bare kernels carry the undated numeric series
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
}
