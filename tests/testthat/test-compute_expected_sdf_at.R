test_that("j = 0 reduces exactly to compute_expected_sdf(i = (m-1)*step)", {
  test_env <- setup_standard_test_env()

  # m = 6 -> s = 5 -> i = 60 (valid 5-year node)
  got <- compute_expected_sdf_at(
    test_env$yields, test_env$term_premia,
    j = 0, m = 6
  )
  want <- compute_expected_sdf(test_env$yields, test_env$term_premia, i = 60)

  expect_equal(got, want)
})

test_that("a positive info-lag j is exactly the base series lagged j rows", {
  test_env <- setup_standard_test_env()

  # j = 1, m = 5 -> s = 5 -> i = 60, same s as the j = 0 case above
  got <- compute_expected_sdf_at(
    test_env$yields, test_env$term_premia,
    j = 1, m = 5
  )
  base <- compute_expected_sdf(test_env$yields, test_env$term_premia, i = 60)

  n <- length(base)
  want <- rep(NA_real_, n)
  want[2:n] <- base[seq_len(n - 1L)]

  expect_equal(got, want)
  expect_true(is.na(got[1])) # first j rows are NA
})

test_that("series with equal j + m share the same horizon, differ only by lag", {
  test_env <- setup_standard_test_env()

  a <- compute_expected_sdf_at(test_env$yields, test_env$term_premia, j = 0, m = 6)
  b <- compute_expected_sdf_at(test_env$yields, test_env$term_premia, j = 2, m = 4)

  n <- length(a)
  a_lag2 <- rep(NA_real_, n)
  a_lag2[3:n] <- a[seq_len(n - 2L)]

  expect_equal(b, a_lag2)
})

test_that("s = 1 lower boundary (m + j = 2) is supported", {
  test_env <- setup_standard_test_env()
  # j = 1, m = 1 -> s = 1 -> i = 12 (one-period horizon; exercises the
  # i == step n_hat normalization through the wrapper)
  got <- compute_expected_sdf_at(test_env$yields, test_env$term_premia, j = 1, m = 1)
  base <- compute_expected_sdf(test_env$yields, test_env$term_premia, i = 12)
  n <- length(base)
  want <- rep(NA_real_, n)
  want[2:n] <- base[seq_len(n - 1L)]
  expect_equal(got, want)
})

test_that("step is plumbed through (non-default step = 6)", {
  # Synthetic 6-month grid so step = 6 has the y6/y12 + tp columns it needs.
  n <- 10L
  base_yield <- seq(1, 4, length.out = n)
  mats <- seq(6L, 36L, by = 6L)
  yields <- as.data.frame(stats::setNames(
    lapply(mats, function(k) base_yield + k / 12), paste0("y", mats)
  ))
  term_premia <- as.data.frame(stats::setNames(
    lapply(mats, function(k) rep(0.1, n)), paste0("tp", mats)
  ))

  # j = 0, m = 2 -> s = 1 -> i = 6 with step = 6
  got <- compute_expected_sdf_at(yields, term_premia, j = 0, m = 2, step = 6L)
  want <- compute_expected_sdf(yields, term_premia, i = 6L, step = 6L)
  expect_equal(got, want)
})

test_that("return_df = TRUE keeps dates unchanged and leads with j NAs", {
  test_env <- setup_standard_test_env()
  got <- compute_expected_sdf_at(
    test_env$yields, test_env$term_premia,
    j = 2, m = 4, return_df = TRUE, dates = test_env$data$date
  )
  expect_s3_class(got, "data.frame")
  expect_named(got, c("date", "expected_sdf"))
  expect_equal(nrow(got), nrow(test_env$yields))
  expect_equal(got$date, test_env$data$date) # dates index t, not the base t-j
  expect_true(all(is.na(got$expected_sdf[1:2]))) # first j rows NA
})

test_that("max in-sample lag (j = T - 1) leaves exactly one valid value", {
  test_env <- setup_standard_test_env()
  n <- nrow(test_env$yields)
  j <- n - 1L
  m <- 2L - j # s = m + j - 1 = 1 stays on the grid (i = 12); exercises m <= 0
  got <- compute_expected_sdf_at(test_env$yields, test_env$term_premia, j = j, m = m)
  base <- compute_expected_sdf(test_env$yields, test_env$term_premia, i = 12)
  want <- rep(NA_real_, n)
  want[n] <- base[1] # the one survivor sits at the last row, = G_1(row 1)
  expect_equal(got, want)
})

test_that("a lag that consumes the whole sample (j = T) errors via the j < n guard", {
  test_env <- setup_standard_test_env()
  n <- nrow(test_env$yields)
  j <- n
  m <- 2L - j # keep s = 1 valid, so the error is the lag guard, NOT the s-range
  expect_error(
    compute_expected_sdf_at(test_env$yields, test_env$term_premia, j = j, m = m),
    regexp = "leaves no in-sample base rows",
    class = "hetid_error_insufficient_data"
  )
})

test_that("integer-valued doubles for j, m, step flow through", {
  test_env <- setup_standard_test_env()
  got <- compute_expected_sdf_at(
    test_env$yields, test_env$term_premia,
    j = 1.0, m = 5.0, step = 12.0
  )
  base <- compute_expected_sdf(test_env$yields, test_env$term_premia, i = 60)
  n <- length(base)
  want <- rep(NA_real_, n)
  want[2:n] <- base[seq_len(n - 1L)]
  expect_equal(got, want)
})

test_that("degenerate horizon (s < 1) errors with the (j, m)-aware message", {
  test_env <- setup_standard_test_env()
  # j = 0, m = 1 -> s = 0; regexp pins the s-range guard, not some other error
  expect_error(
    compute_expected_sdf_at(test_env$yields, test_env$term_premia, j = 0, m = 1),
    regexp = "news horizon s",
    class = "hetid_error_bad_argument"
  )
})

test_that("horizon past the n_hat grid errors with the (j, m)-aware message", {
  test_env <- setup_standard_test_env()
  # j = 5, m = 6 -> s = 10 -> i = 120 > effective_max (108)
  expect_error(
    compute_expected_sdf_at(test_env$yields, test_env$term_premia, j = 5, m = 6),
    regexp = "news horizon s",
    class = "hetid_error_bad_argument"
  )
})

test_that("non-integer or negative j errors", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_expected_sdf_at(test_env$yields, test_env$term_premia, j = -1, m = 6),
    class = "hetid_error"
  )
  expect_error(
    compute_expected_sdf_at(test_env$yields, test_env$term_premia, j = 1.5, m = 5),
    class = "hetid_error"
  )
})

test_that("non-integer or NA m errors", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_expected_sdf_at(test_env$yields, test_env$term_premia, j = 1, m = 2.5),
    class = "hetid_error"
  )
  expect_error(
    compute_expected_sdf_at(test_env$yields, test_env$term_premia, j = 1, m = NA_real_),
    class = "hetid_error"
  )
})

test_that("mismatched yields/term_premia rows error", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_expected_sdf_at(
      test_env$yields, test_env$term_premia[-1, ],
      j = 1, m = 5
    ),
    class = "hetid_error"
  )
})
