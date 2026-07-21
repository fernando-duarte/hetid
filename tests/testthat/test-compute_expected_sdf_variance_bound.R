# Contract and formula tests for compute_expected_sdf_variance_bound: the
# reported bound is min{(1/4)*C*K, var_N(q)} on the estimator's paired mask.
# Edge and boundary cases live in the _edges sibling; the manual
# reconstructions (gap_series, q_series, component_arm, var_n,
# esdf_bound_manual) are in helper-expected-sdf-bounds.R.

test_that("compute_expected_sdf_variance_bound returns a single non-negative value", {
  test_env <- setup_standard_test_env()

  bound <- compute_expected_sdf_variance_bound(
    test_env$yields, test_env$term_premia,
    i = 60
  )

  expect_single_finite_value(bound, should_be_positive = FALSE)
  expect_gte(bound, 0)
})

test_that("the bound is the smaller of the component and q arms", {
  test_env <- setup_standard_test_env()
  i <- 60
  bound <- compute_expected_sdf_variance_bound(
    test_env$yields, test_env$term_premia,
    i = i
  )
  comp <- component_arm(test_env$yields, test_env$term_premia, i = i)
  vq <- var_n(q_series(test_env$yields, test_env$term_premia, i = i))
  expect_equal(bound, min(comp, vq), tolerance = 1e-12)
  # the load-bearing empirical claim on real data: the q arm binds, so the
  # reported value is the exact first-order-cancelled bound estimate
  expect_lt(vq, comp)
})

test_that("the q arm wins on real data at every tested horizon", {
  test_env <- setup_standard_test_env()
  for (i in c(12, 60, 108)) {
    comp <- component_arm(test_env$yields, test_env$term_premia, i = i)
    vq <- var_n(q_series(test_env$yields, test_env$term_premia, i = i))
    bound <- compute_expected_sdf_variance_bound(
      test_env$yields, test_env$term_premia,
      i = i
    )
    expect_lt(vq, comp)
    expect_equal(bound, vq, tolerance = 1e-12)
  }
})

test_that("the component arm wins on a fat-tailed symmetric fixture", {
  # y24 = y36 = tp = 0 => n_hat(24) = 0, envelope C = 1, m_step = 1; the
  # alternating y12 = -/+300 pattern makes u = +/-3 over the paired set, where
  # var(q) = var(e^u - 1 - u) ~ 49.3 exceeds the component (1/4)*mean(u^4) =
  # 20.25, so the min must select the component arm
  y12_pct <- c(0, 0, -300, 300, -300, 300)
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  bound <- compute_expected_sdf_variance_bound(yields, term_premia, i = 24)

  comp <- component_arm(yields, term_premia, i = 24)
  vq <- var_n(q_series(yields, term_premia, i = 24))
  expect_lt(comp, vq)
  expect_equal(bound, comp, tolerance = 1e-12)
  expect_equal(comp, 20.25, tolerance = 1e-12)
})

test_that("the q arm uses divisor N, not N - 1", {
  test_env <- setup_standard_test_env()
  i <- 48

  bound <- compute_expected_sdf_variance_bound(
    test_env$yields, test_env$term_premia,
    i = i
  )

  q <- q_series(test_env$yields, test_env$term_premia, i = i)
  # the q arm binds on this data; its variance uses divisor N, not stats::var's
  # N - 1
  expect_equal(bound, var_n(q), tolerance = 1e-12)
  expect_equal(
    bound,
    stats::var(q) * (length(q) - 1) / length(q),
    tolerance = 1e-12
  )
  expect_false(isTRUE(all.equal(bound, stats::var(q))))
})

test_that("the bound centers on the same correction compute_expected_sdf adds", {
  # mean(gap) is compute_expected_sdf's paired additive correction; the bound
  # arms are computed on that same masked sample
  test_env <- setup_standard_test_env()
  i <- 60

  esdf <- compute_expected_sdf(
    test_env$yields, test_env$term_premia,
    i = i, paired = TRUE, dates = test_env$data$date
  )$expected_sdf
  n_hat <- n_hat_series(test_env$yields, test_env$term_premia, i = i)
  # esdf - exp(n_hat) is the constant correction at every finite t; take its
  # NA-robust value rather than index [1] (which could be NA on some data)
  correction <- mean(esdf - exp(n_hat), na.rm = TRUE)

  g <- gap_series(test_env$yields, test_env$term_premia, i = i)
  expect_equal(mean(g), correction, tolerance = 1e-12)

  bound <- compute_expected_sdf_variance_bound(
    test_env$yields, test_env$term_premia,
    i = i
  )
  # min <= q arm structurally
  expect_lte(bound, var_n(q_series(test_env$yields, test_env$term_premia, i = i)))
})

test_that("compute_expected_sdf_variance_bound honors a non-default step", {
  # step = 6: one-period bond is y6, lead s = i/step = 2; y12=y18=tp12=tp18=0
  # => n_hat(12, step=6) = 0 => e^{n_hat} = 1, m_step = 0.5
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

  expect_equal(
    bound,
    esdf_bound_manual(yields, term_premia, i = 12, step = step),
    tolerance = 1e-12
  )
})

test_that("q removes the first-order term: var(q) is far below var(g)", {
  test_env <- setup_standard_test_env()
  # the projection arm is gone from the min, but the property that motivated
  # the q arm still holds and guards the construction: the linear term is
  # cancelled, so var(q) is orders of magnitude below var(g)
  for (i in c(12, 60, 108)) {
    vg <- var_n(gap_series(test_env$yields, test_env$term_premia, i = i))
    vq <- var_n(q_series(test_env$yields, test_env$term_premia, i = i))
    expect_lt(vq, vg)
    expect_lt(vq / vg, 1e-2)
  }
})
