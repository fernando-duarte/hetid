# Edge and boundary tests for compute_expected_sdf_variance_bound: horizon
# zero, masking, overflow, degenerate data, and validation errors. The
# contract/formula tests live in the non-_edges sibling; shared manual
# reconstructions come from helper-expected-sdf-bounds.R.

test_that("compute_expected_sdf_variance_bound is identically 0 at i = 0", {
  # Horizon 0 is exact (no approximation), so the error-variance bound is 0,
  # mirroring compute_expected_sdf(i = 0)
  test_env <- setup_standard_test_env()

  expect_warning(
    bound <- compute_expected_sdf_variance_bound(
      test_env$yields, test_env$term_premia,
      i = 0
    ),
    class = "hetid_warning_horizon_zero"
  )

  expect_identical(bound, 0)
})

test_that("the bound averages over finite pairs only (interior NA)", {
  # Interior NA in the realized one-period (y12) leg drops one pair; both
  # arms are computed over the surviving finite gaps
  test_env <- setup_standard_test_env()
  i <- 60
  yields_na <- test_env$yields
  yields_na$y12[40] <- NA_real_

  bound <- compute_expected_sdf_variance_bound(
    yields_na, test_env$term_premia,
    i = i
  )

  expect_true(is.finite(bound))
  expect_equal(
    bound,
    esdf_bound_manual(yields_na, test_env$term_premia, i = i),
    tolerance = 1e-12
  )
})

test_that("the bound drops a non-finite exp(n_hat) leg too (interior NA in y60)", {
  # an NA in y60 makes the exp(n_hat) leg of a pair non-finite; the gap filter
  # must drop it, masking both legs, not just the realized one-period y12 leg
  test_env <- setup_standard_test_env()
  i <- 60
  yields_na <- test_env$yields
  yields_na$y60[40] <- NA_real_

  bound <- compute_expected_sdf_variance_bound(
    yields_na, test_env$term_premia,
    i = i
  )

  expect_true(is.finite(bound))
  expect_equal(
    bound,
    esdf_bound_manual(yields_na, test_env$term_premia, i = i),
    tolerance = 1e-12
  )
})

test_that("a constant gap series gives a bound of exactly 0", {
  # y24 = y36 = tp24 = tp36 = 0 => n_hat(24) = 0 => e^{n_hat} = 1; a constant
  # one-period yield makes q constant, so the q arm's 1/N variance is exactly
  # 0 and wins the min (the component arm is strictly positive here)
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
  # The 1/N centered variance of one point is 0; stats::var would give NA
  y12_pct <- c(1, 2, 3)
  zeros <- numeric(3)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  bound <- compute_expected_sdf_variance_bound(yields, term_premia, i = 24)

  expect_equal(bound, 0, tolerance = 1e-12)
})

test_that("compute_expected_sdf_variance_bound handles the i = step (s = 1) horizon", {
  # Smallest horizon: i = step => s = 1, the n_hat boundary where TP^(1):=0
  test_env <- setup_standard_test_env()
  i <- HETID_CONSTANTS$DEFAULT_STEP

  bound <- compute_expected_sdf_variance_bound(
    test_env$yields, test_env$term_premia,
    i = i
  )

  expect_true(is.finite(bound))
  expect_gte(bound, 0)
  expect_equal(
    bound,
    esdf_bound_manual(test_env$yields, test_env$term_premia, i = i),
    tolerance = 1e-12
  )
})

test_that("the bound drops an Inf realized leg (is.finite, not just NA)", {
  # A wildly negative one-period yield overflows exp() to Inf; that pair
  # must be dropped, leaving a finite bound over the remaining gaps
  y12_pct <- c(0, 0, -1e5, 2, 5) # -1e5% -> exp(+1000) = Inf
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  bound <- compute_expected_sdf_variance_bound(yields, term_premia, i = 24)

  expect_true(is.finite(bound))
  expect_gte(bound, 0)
})

test_that("both arms overflowing on a kept row gives Inf, not NA", {
  # y12 = +Inf at one interior date leaves gap = -e^{n_hat} finite, so the row
  # is KEPT, but u = -Inf makes q = +Inf (q arm -> Inf) and mean(u^4) = Inf
  # makes the component arm Inf too: the documented contract is Inf on a
  # nonempty sample, never NA and never a silent row drop
  y12_pct <- c(1, 2, 3, 4, 5, 6)
  y12_pct[3] <- Inf
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  bound <- compute_expected_sdf_variance_bound(yields, term_premia, i = 24)

  expect_identical(bound, Inf)
})

test_that("compute_expected_sdf_variance_bound returns NA when no finite pairs", {
  # Unlike compute_expected_sdf (which errors), the scalar bound returns
  # NA_real_ on degenerate data, matching compute_c_hat / compute_k_hat
  test_env <- setup_standard_test_env()
  yields_na <- test_env$yields
  yields_na$y12 <- NA_real_ # realized one-period leg all NA

  bound <- compute_expected_sdf_variance_bound(
    yields_na, test_env$term_premia,
    i = 60
  )

  expect_true(is.na(bound))
  # Typed NA contract (double, not logical NA), checked against the value
  # already computed above rather than re-running the function
  expect_identical(bound, NA_real_)
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
