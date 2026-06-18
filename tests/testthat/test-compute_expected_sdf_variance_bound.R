# Reconstruct the gap series e^{-y^(1)_{t+s}} - e^{n_hat(i,t)} over T_i,
# matching compute_expected_sdf's construction, for manual checks.
gap_series <- function(yields, term_premia, i,
                       step = HETID_CONSTANTS$DEFAULT_STEP) {
  n_hat <- compute_n_hat(yields, term_premia, i, step = step)
  y_step <- yields[[acm_column_name("yields", step)]]
  s <- i %/% step
  n_obs <- length(n_hat)
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  exp_n_hat_paired <- exp(n_hat[seq_len(n_obs - s)])
  realized <- exp(-m_step * y_step[seq.int(s + 1L, n_obs)] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL)
  g <- realized - exp_n_hat_paired
  g[is.finite(g)]
}

test_that("compute_expected_sdf_variance_bound returns a single non-negative value", {
  test_env <- setup_standard_test_env()

  bound <- compute_expected_sdf_variance_bound(
    test_env$yields, test_env$term_premia,
    i = 60
  )

  expect_type(bound, "double")
  expect_length(bound, 1)
  expect_true(is.finite(bound))
  expect_gte(bound, 0)
})

test_that("the bound equals the 1/N centered variance of the gap series", {
  test_env <- setup_standard_test_env()
  i <- 60

  bound <- compute_expected_sdf_variance_bound(
    test_env$yields, test_env$term_premia,
    i = i
  )

  g <- gap_series(test_env$yields, test_env$term_premia, i = i)
  expected <- sum((g - mean(g))^2) / length(g)

  expect_equal(bound, expected, tolerance = 1e-12)
})

test_that("the bound uses divisor N, not N - 1", {
  test_env <- setup_standard_test_env()
  i <- 48

  bound <- compute_expected_sdf_variance_bound(
    test_env$yields, test_env$term_premia,
    i = i
  )

  g <- gap_series(test_env$yields, test_env$term_premia, i = i)
  # var() uses divisor N-1; the projection bound uses N
  expect_equal(bound, stats::var(g) * (length(g) - 1) / length(g),
    tolerance = 1e-12
  )
  # The N vs N-1 difference is observable only when var(g) != 0 (true for
  # this real-data fixture); on a constant gap both would be 0.
  expect_false(isTRUE(all.equal(bound, stats::var(g))))
})

test_that("the bound centers on the same correction compute_expected_sdf adds", {
  # The spec ties the two: mean(gap) is compute_expected_sdf's additive
  # correction, and the bound is the centered variance of that same gap.
  test_env <- setup_standard_test_env()
  i <- 60

  esdf <- compute_expected_sdf(test_env$yields, test_env$term_premia, i = i)
  n_hat <- compute_n_hat(test_env$yields, test_env$term_premia, i = i)
  # esdf - exp(n_hat) is the constant correction at every finite t; take its
  # NA-robust value rather than index [1] (which could be NA on some data)
  correction <- mean(esdf - exp(n_hat), na.rm = TRUE)

  g <- gap_series(test_env$yields, test_env$term_premia, i = i)
  expect_equal(mean(g), correction, tolerance = 1e-12)

  bound <- compute_expected_sdf_variance_bound(
    test_env$yields, test_env$term_premia,
    i = i
  )
  expect_equal(bound, mean((g - correction)^2), tolerance = 1e-12)
})

test_that("compute_expected_sdf_variance_bound honors a non-default step", {
  # step = 6: one-period bond is y6, lead s = i/step = 2; y12=y18=tp12=tp18=0
  # => n_hat(12, step=6) = 0 => e^{n_hat} = 1, m_step = 0.5.
  step <- 6L
  y6_pct <- c(0, 2, 5, 9, 14, 20, 27)
  n <- length(y6_pct)
  zeros <- numeric(n)
  yields <- data.frame(y6 = y6_pct, y12 = zeros, y18 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp18 = zeros)

  bound <- compute_expected_sdf_variance_bound(
    yields, term_premia,
    i = 12, step = step
  )

  s <- 12L %/% step
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  realized <- exp(-m_step * y6_pct[(s + 1):n] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL)
  g <- realized - 1 # e^{n_hat} = 1 everywhere
  expect_equal(bound, sum((g - mean(g))^2) / length(g), tolerance = 1e-12)
})

test_that("the bound averages over finite pairs only (interior NA)", {
  # Interior NA in the realized one-period (y12) leg drops one pair; the
  # bound is the 1/N centered variance over the SURVIVING finite gaps.
  test_env <- setup_standard_test_env()
  i <- 60
  yields_na <- test_env$yields
  yields_na$y12[40] <- NA_real_

  bound <- compute_expected_sdf_variance_bound(
    yields_na, test_env$term_premia,
    i = i
  )

  g <- gap_series(yields_na, test_env$term_premia, i = i) # already finite-filtered
  expect_true(is.finite(bound))
  expect_equal(bound, sum((g - mean(g))^2) / length(g), tolerance = 1e-12)
})

test_that("the bound drops a non-finite exp(n_hat) leg too (interior NA in y60)", {
  # n_hat(60) uses y60/y72/tp60/tp72, so an NA in y60 makes the exp(n_hat)
  # leg of a pair non-finite. The gap filter must drop it (both legs masked,
  # not just the realized one-period y12 leg).
  test_env <- setup_standard_test_env()
  i <- 60
  yields_na <- test_env$yields
  yields_na$y60[40] <- NA_real_

  bound <- compute_expected_sdf_variance_bound(
    yields_na, test_env$term_premia,
    i = i
  )

  g <- gap_series(yields_na, test_env$term_premia, i = i)
  expect_true(is.finite(bound))
  expect_equal(bound, sum((g - mean(g))^2) / length(g), tolerance = 1e-12)
})

test_that("a constant gap series gives a bound of exactly 0", {
  # y24 = y36 = tp24 = tp36 = 0 => n_hat(24) = 0 => e^{n_hat} = 1; a
  # constant one-period yield makes the realized leg constant too, so the
  # gap is constant and its 1/N variance is exactly 0 (not NA, not an error).
  k <- 8
  y12_pct <- rep(2, k)
  zeros <- numeric(k)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  bound <- compute_expected_sdf_variance_bound(yields, term_premia, i = 24)

  expect_equal(bound, 0, tolerance = 1e-12)
})

test_that("a single valid pair gives a bound of exactly 0 (divisor N)", {
  # T = 3, i = 24, step = 12 => s = 2 => exactly one paired date (T - s = 1).
  # The 1/N centered variance of one point is 0; stats::var would give NA.
  y12_pct <- c(1, 2, 3)
  zeros <- numeric(3)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  bound <- compute_expected_sdf_variance_bound(yields, term_premia, i = 24)

  expect_equal(bound, 0, tolerance = 1e-12)
})

test_that("compute_expected_sdf_variance_bound handles the i = step (s = 1) horizon", {
  # Smallest horizon: i = step => s = 1, the n_hat boundary where TP^(1):=0.
  test_env <- setup_standard_test_env()
  i <- HETID_CONSTANTS$DEFAULT_STEP

  bound <- compute_expected_sdf_variance_bound(
    test_env$yields, test_env$term_premia,
    i = i
  )

  g <- gap_series(test_env$yields, test_env$term_premia, i = i)
  expect_true(is.finite(bound))
  expect_gte(bound, 0)
  expect_equal(bound, sum((g - mean(g))^2) / length(g), tolerance = 1e-12)
})

test_that("the bound drops an Inf realized leg (is.finite, not just NA)", {
  # A wildly negative one-period yield overflows exp() to Inf; that pair
  # must be dropped, leaving a finite bound over the remaining gaps.
  y12_pct <- c(0, 0, -1e5, 2, 5) # -1e5% -> exp(+1000) = Inf
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  bound <- compute_expected_sdf_variance_bound(yields, term_premia, i = 24)

  expect_true(is.finite(bound))
  expect_gte(bound, 0)
})

test_that("compute_expected_sdf_variance_bound returns NA when no finite pairs", {
  # Unlike compute_expected_sdf (which errors), the scalar bound returns
  # NA_real_ on degenerate data, matching compute_c_hat / compute_k_hat.
  test_env <- setup_standard_test_env()
  yields_na <- test_env$yields
  yields_na$y12 <- NA_real_ # realized one-period leg all NA

  bound <- compute_expected_sdf_variance_bound(
    yields_na, test_env$term_premia,
    i = 60
  )

  expect_type(bound, "double")
  expect_true(is.na(bound))
  expect_identical(
    compute_expected_sdf_variance_bound(yields_na, test_env$term_premia, i = 60),
    NA_real_
  )
})

test_that("compute_expected_sdf_variance_bound raises on a short series", {
  syn <- create_synthetic_test_data(n = 5)
  expect_error(
    compute_expected_sdf_variance_bound(syn$yields, syn$term_premia, i = 108),
    "Not enough observations",
    class = "hetid_error_insufficient_data"
  )
})

test_that("compute_expected_sdf_variance_bound rejects invalid maturities", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_expected_sdf_variance_bound(test_env$yields, test_env$term_premia, i = 1.5),
    "integer"
  )
  expect_error(
    compute_expected_sdf_variance_bound(test_env$yields, test_env$term_premia, i = 120),
    "between"
  )
  expect_error(
    compute_expected_sdf_variance_bound(test_env$yields, test_env$term_premia, i = 18),
    "multiple of step"
  )
})

test_that("compute_expected_sdf_variance_bound rejects mismatched rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  expect_error(
    compute_expected_sdf_variance_bound(syn_long$yields, syn_short$term_premia, i = 60),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
})
