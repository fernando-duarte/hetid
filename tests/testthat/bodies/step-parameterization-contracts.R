{
  test_env <- setup_standard_test_env()
  yields <- test_env$yields
  tp <- test_env$term_premia
  dates <- test_env$data$date
  s <- HETID_CONSTANTS$DEFAULT_STEP

  # Dated wrappers require dates on both sides; explicit step must still
  # reproduce the omitted-step result exactly
  expect_identical(
    compute_n_hat(yields, tp, i = 60, step = s, dates = dates),
    compute_n_hat(yields, tp, i = 60, dates = dates)
  )
  expect_identical(
    compute_price_news(yields, tp, i = 60, step = s, dates = dates),
    compute_price_news(yields, tp, i = 60, dates = dates)
  )
  expect_identical(
    compute_sdf_innovations(yields, tp, i = 60, step = s, dates = dates),
    compute_sdf_innovations(yields, tp, i = 60, dates = dates)
  )
  expect_identical(
    compute_c_hat(yields, tp, i = 60, step = s),
    compute_c_hat(yields, tp, i = 60)
  )
  expect_identical(
    compute_k_hat(yields, tp, i = 60, step = s),
    compute_k_hat(yields, tp, i = 60)
  )
  expect_identical(
    compute_variance_bound(yields, tp, i = 60, step = s),
    compute_variance_bound(yields, tp, i = 60)
  )
  expect_identical(
    compute_expected_sdf(yields, tp, i = 60, step = s, dates = dates),
    compute_expected_sdf(yields, tp, i = 60, dates = dates)
  )
  expect_identical(
    compute_expected_sdf_variance_bound(yields, tp, i = 60, step = s),
    compute_expected_sdf_variance_bound(yields, tp, i = 60)
  )
}
