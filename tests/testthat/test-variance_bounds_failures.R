test_that("positive infinite q arms lose to finite envelopes without being discarded", {
  local_mocked_bindings(
    compute_variance_bound = function(...) 2,
    compute_news_q_bound = function(...) Inf,
    compute_expected_sdf_variance_bound = function(...) 3
  )
  result <- compute_variance_bounds(matrix(0, 3, 1), matrix(0, 3, 1), 1L, 1L)
  expect_identical(result$per_maturity$News_Q_Bound, Inf)
  expect_identical(result$per_maturity$Variance_Bound, 2)
})

test_that("invalid bound arms identify their series and offending maturities", {
  for (bad in c(NA_real_, NaN, 0, -1, -Inf)) {
    local_mocked_bindings(
      compute_variance_bound = function(...) 2,
      compute_news_q_bound = function(...) bad,
      compute_expected_sdf_variance_bound = function(...) 3
    )
    expect_error(compute_variance_bounds(matrix(0, 3, 1), matrix(0, 3, 1), 1L, 2L),
      "News_Q_Bound at maturities: 2",
      class = "hetid_error"
    )
  }
  for (bad in c(NA_real_, NaN, 0, -1, Inf, -Inf)) {
    local_mocked_bindings(
      compute_variance_bound = function(...) bad,
      compute_news_q_bound = function(...) 2,
      compute_expected_sdf_variance_bound = function(...) 3
    )
    expect_error(compute_variance_bounds(matrix(0, 3, 1), matrix(0, 3, 1), 1L, 2L),
      "News_Envelope_Bound at maturities: 2",
      class = "hetid_error"
    )
    local_mocked_bindings(
      compute_variance_bound = function(...) 2,
      compute_expected_sdf_variance_bound = function(...) bad
    )
    expect_error(compute_variance_bounds(matrix(0, 3, 1), matrix(0, 3, 1), 1L, 2L),
      "Expected_SDF_Bound at maturities: 2",
      class = "hetid_error"
    )
  }
})

test_that("real scalar overflow and degenerate zero bounds retain their distinct contracts", {
  y12 <- c(1, 4, 9, -1e5, 7, 5, 3, 8)
  yields <- data.frame(y12 = y12, y24 = numeric(8), y36 = numeric(8))
  tp <- data.frame(tp12 = numeric(8), tp24 = numeric(8), tp36 = numeric(8))
  expect_identical(compute_news_q_bound(yields, tp, 24L, step = 12L), Inf)
  result <- compute_variance_bounds(yields, tp, 12L, 24L)$per_maturity
  expect_identical(result$News_Q_Bound, Inf)
  expect_true(is.finite(result$News_Envelope_Bound))
  expect_identical(result$Variance_Bound, result$News_Envelope_Bound)
  expect_true(is.finite(result$Expected_SDF_Bound))
  yields$y12 <- rep(2, 8)
  yields$y24 <- rep(4, 8)
  yields$y36 <- rep(6, 8)
  expect_error(compute_variance_bounds(yields, tp, 12L, 24L), class = "hetid_error")
})

test_that("invalid maturity grids and primitive input failures are structured", {
  y <- data.frame(y12 = 1:5, y24 = 2:6, y36 = 3:7)
  tp <- data.frame(tp12 = numeric(5), tp24 = numeric(5), tp36 = numeric(5))
  for (m in list(numeric(), c(12, 12), 0, 18, 120, NA_real_, Inf, matrix(12), "12")) {
    expect_error(compute_variance_bounds(y, tp, step = 12L, maturities = m),
      class = "hetid_error_bad_argument"
    )
  }
  expect_error(compute_variance_bounds(y, tp, step = 0), class = "hetid_error_bad_argument")
  expect_error(compute_variance_bounds(y, tp[-1, ], 12L, 24L),
    class = "hetid_error_dimension_mismatch"
  )
  expect_error(compute_variance_bounds(y["y12"], tp, 12L, 24L),
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_variance_bounds(y[1:2, ], tp[1:2, ], 12L, 24L),
    class = "hetid_error_insufficient_data"
  )
})
