# Contract, alignment, and edge tests for compute_news_q_bound, the two-leg
# first-order-cancelled (Minkowski) SDF-news variance bound. The manual spec
# reconstruction news_q_manual lives in helper-expected-sdf-bounds.R.
#
# Fixture convention: with y24 = y36 = tp* = 0 and step = 12, n_hat(24) = 0,
# n_hat(12) = y12/100, and the realized log price is x_t = -y12[t+s]/100, so
# every leg is hand-computable from the y12 vector alone.

test_that("compute_news_q_bound matches the manual Minkowski formula at s = 2", {
  # i = 2 * step exercises a multi-period lead with nonlinear row values, so a
  # wrong slice in any of the three legs moves the answer
  y12_pct <- c(1, 4, 9, 2, 7, 5, 3, 8)
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  bound <- compute_news_q_bound(yields, term_premia, i = 24)

  expect_single_finite_value(bound, should_be_positive = FALSE)
  expect_gte(bound, 0)
  expect_equal(
    bound,
    news_q_manual(yields, term_premia, i = 24),
    tolerance = 1e-12
  )
})

test_that("the i = step boundary uses the realized step-bond leg (q1 leg exact)", {
  # At s = 1 the led forecast IS the realized log step-bond price
  # (compute_n_hat_previous with TP^(step) := 0), so u1 = 0 identically and
  # the q1 sigma contributes exactly nothing
  y12_pct <- c(2, 5, 1, 7, 4, 6)
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)
  i <- HETID_CONSTANTS$DEFAULT_STEP

  bound <- compute_news_q_bound(yields, term_premia, i = i)

  expect_equal(
    bound,
    news_q_manual(yields, term_premia, i = i),
    tolerance = 1e-12
  )
  # reconstruct the boundary property directly: n1 leg equals x, so q1 = 0
  s <- 1L
  n0 <- (y12_pct / 100)[seq_len(n - s)]
  x <- (-y12_pct / 100)[seq.int(s + 1L, n)]
  q0 <- exp(n0) * (expm1(x - n0) - (x - n0))
  d <- x - n0
  g <- exp(n0) * (expm1(d) - d - d^2 / 2)
  expect_equal(bound, (sd_n(q0) + sd_n(g))^2, tolerance = 1e-12)
})

test_that("interior primitive NAs drop rows; the survivors match the manual formula", {
  y12_pct <- c(1, 4, 9, 2, 7, 5, 3, 8)
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  yields$y12[4] <- NA_real_ # hits n_hat(12) at t = 4 and the realized leg
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  bound <- compute_news_q_bound(yields, term_premia, i = 24)

  expect_true(is.finite(bound))
  expect_equal(
    bound,
    news_q_manual(yields, term_premia, i = 24),
    tolerance = 1e-12
  )
})

test_that("an overflowing leg on finite primitives gives Inf, not NA or a drop", {
  # y12 = -1e5 at a realized position keeps every primitive finite
  # (x = +1000), but q0 = expm1(1000) overflows: the arm turns Inf and the
  # bound is Inf -- conservative, so the envelope arm wins the caller's min
  y12_pct <- c(1, 4, 9, -1e5, 7, 5, 3, 8)
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  bound <- compute_news_q_bound(yields, term_premia, i = 24)

  expect_identical(bound, Inf)
})

test_that("compute_news_q_bound returns NA_real_ when no primitive rows survive", {
  # An all-NA y24 kills both forecast legs (n_hat(24) and n_hat(12) each read
  # y24) at every date while the realized y12 leg stays finite, so the
  # primitive mask is empty; y12 keeps percent-scale values so the units
  # heuristic stays quiet
  y12_pct <- c(1, 4, 9, 2, 7, 5, 3, 8)
  zeros <- numeric(8)
  yields <- data.frame(y12 = y12_pct, y24 = rep(NA_real_, 8), y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  bound <- compute_news_q_bound(yields, term_premia, i = 24)

  expect_identical(bound, NA_real_)
})

test_that("compute_news_q_bound raises on a short series", {
  syn <- create_synthetic_test_data(n = 5)
  expect_error(
    compute_news_q_bound(syn$yields, syn$term_premia, i = 108),
    "Not enough observations",
    class = "hetid_error_insufficient_data"
  )
})

test_that("compute_news_q_bound rejects invalid maturities with structured errors", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_news_q_bound(test_env$yields, test_env$term_premia, i = 0),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_news_q_bound(test_env$yields, test_env$term_premia, i = 18),
    "multiple of step"
  )
  expect_error(
    compute_news_q_bound(test_env$yields, test_env$term_premia, i = 120),
    "between"
  )
})

test_that("compute_news_q_bound rejects mismatched rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  expect_error(
    compute_news_q_bound(syn_long$yields, syn_short$term_premia, i = 60),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("compute_news_q_bound is finite and positive on the ACM extract", {
  test_env <- setup_standard_test_env()
  for (i in c(12, 60, 108)) {
    news_q <- compute_news_q_bound(test_env$yields, test_env$term_premia, i = i)
    envelope <- compute_variance_bound(test_env$yields, test_env$term_premia, i = i)
    expect_true(is.finite(news_q))
    expect_gt(news_q, 0)
    expect_true(is.finite(envelope))
    # the reported news bound downstream is the pointwise min of the two
    expect_true(is.finite(min(envelope, news_q)))
  }
})
