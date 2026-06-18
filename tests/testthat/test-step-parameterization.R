# Step parameterization: explicit step = DEFAULT_STEP must reproduce the
# omitted-step results exactly, and non-default steps must follow the
# hand-computed formulas on synthetic data.

# Synthetic frame with columns only at the maturities a step = 6 chain
# touches; deterministic values so expectations are hand-computable
make_step_six_frame <- function() {
  y6 <- c(2.0, 2.1, 2.3, 2.2, 2.4, 2.6, 2.5, 2.7)
  y12 <- c(3.0, 3.2, 3.1, 3.3, 3.5, 3.4, 3.6, 3.8)
  y18 <- c(4.1, 4.0, 4.2, 4.4, 4.3, 4.5, 4.7, 4.6)
  tp6 <- c(0.20, 0.22, 0.21, 0.23, 0.25, 0.24, 0.26, 0.28)
  tp12 <- c(0.40, 0.41, 0.43, 0.42, 0.44, 0.46, 0.45, 0.47)
  tp18 <- c(0.60, 0.62, 0.61, 0.63, 0.65, 0.64, 0.66, 0.68)
  list(
    yields = data.frame(y6 = y6, y12 = y12, y18 = y18),
    term_premia = data.frame(tp6 = tp6, tp12 = tp12, tp18 = tp18)
  )
}

test_that("explicit default step reproduces omitted-step results on bundled data", {
  test_env <- setup_standard_test_env()
  yields <- test_env$yields
  tp <- test_env$term_premia
  s <- HETID_CONSTANTS$DEFAULT_STEP

  expect_identical(
    compute_n_hat(yields, tp, i = 60, step = s),
    compute_n_hat(yields, tp, i = 60)
  )
  expect_identical(
    compute_price_news(yields, tp, i = 60, step = s),
    compute_price_news(yields, tp, i = 60)
  )
  expect_identical(
    compute_sdf_innovations(yields, tp, i = 60, step = s),
    compute_sdf_innovations(yields, tp, i = 60)
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
    compute_expected_sdf(yields, tp, i = 60, step = s),
    compute_expected_sdf(yields, tp, i = 60)
  )
})

test_that("explicit default step reproduces omitted-step w2 residuals", {
  test_env <- setup_standard_test_env()
  set.seed(42)
  pcs <- matrix(rnorm(nrow(test_env$yields) * 4), ncol = 4)

  res_omitted <- compute_w2_residuals(
    test_env$yields, test_env$term_premia,
    maturities = c(24, 36), n_pcs = 4, pcs = pcs
  )
  res_explicit <- compute_w2_residuals(
    test_env$yields, test_env$term_premia,
    maturities = c(24, 36), n_pcs = 4, pcs = pcs,
    step = HETID_CONSTANTS$DEFAULT_STEP
  )
  expect_identical(res_omitted, res_explicit)
})

test_that("n_hat with step = 6 matches the hand formula", {
  frame <- make_step_six_frame()
  pct <- HETID_CONSTANTS$PERCENT_TO_DECIMAL
  units <- HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR

  # At i == step the step-maturity term premium is normalized to zero
  # (TP^(1):=0), so the (6 / units) * tp6 term is dropped
  n_hat_6 <- compute_n_hat(frame$yields, frame$term_premia, i = 6, step = 6)
  expected_6 <- ((6 / units) * frame$yields$y6 - (12 / units) * frame$yields$y12 +
    (12 / units) * frame$term_premia$tp12) / pct
  expect_equal(n_hat_6, expected_6)

  n_hat_12 <- compute_n_hat(frame$yields, frame$term_premia, i = 12, step = 6)
  expected_12 <- ((12 / units) * frame$yields$y12 - (18 / units) * frame$yields$y18 +
    (18 / units) * frame$term_premia$tp18 - (12 / units) * frame$term_premia$tp12) / pct
  expect_equal(n_hat_12, expected_12)
})

test_that("n_hat previous boundary at i == step uses the step-maturity yield", {
  frame <- make_step_six_frame()
  pct <- HETID_CONSTANTS$PERCENT_TO_DECIMAL
  units <- HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR

  boundary <- compute_n_hat_previous(frame$yields, frame$term_premia, i = 6, step = 6)
  expect_equal(boundary, -(6 / units) * frame$yields$y6 / pct)
})

test_that("k_hat with step = 6 shifts by whole news periods", {
  frame <- make_step_six_frame()
  pct <- HETID_CONSTANTS$PERCENT_TO_DECIMAL
  units <- HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  n_obs <- nrow(frame$yields)

  k_hat <- compute_k_hat(frame$yields, frame$term_premia, i = 12, step = 6)

  # n_hat_prev = compute_n_hat(6, step = 6) at the one-period maturity,
  # where TP^(1):=0 drops the (6 / units) * tp6 term
  n_hat_prev <- ((6 / units) * frame$yields$y6 - (12 / units) * frame$yields$y12 +
    (12 / units) * frame$term_premia$tp12) / pct
  horizon_periods <- 2
  realized <- -(6 / units) * frame$yields$y6[(horizon_periods + 1):n_obs] / pct
  forecast <- n_hat_prev[2:(n_obs - horizon_periods + 1)]
  expect_equal(k_hat, mean((realized - forecast)^4))
})

test_that("k_hat rejects a maturity that is not a multiple of step", {
  frame <- make_step_six_frame()
  expect_error(
    compute_k_hat(frame$yields, frame$term_premia, i = 9, step = 6),
    class = "hetid_error_bad_argument"
  )
})

test_that("price news with step = 6 differences the step-spaced n_hat series", {
  frame <- make_step_six_frame()
  n_obs <- nrow(frame$yields)

  news <- compute_price_news(frame$yields, frame$term_premia, i = 12, step = 6)
  n_hat_12 <- compute_n_hat(frame$yields, frame$term_premia, i = 12, step = 6)
  n_hat_6 <- compute_n_hat(frame$yields, frame$term_premia, i = 6, step = 6)
  expect_equal(news, n_hat_6[2:n_obs] - n_hat_12[1:(n_obs - 1)])
})

test_that("the quarterly clock is valid from the boundary; below-floor horizons fail", {
  # With the 3-month maturity floor, step = 3 news works from the
  # boundary horizon i = 3 upward; a horizon whose previous-period
  # maturity falls below the floor still fails informatively
  expect_true(validate_news_maturity_index(3, step = 3))
  expect_true(validate_news_maturity_index(6, step = 3))
  expect_true(validate_news_maturity_index(9, step = 3))

  frame <- make_step_six_frame()
  expect_error(
    compute_price_news(frame$yields, frame$term_premia, i = 4, step = 3),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_price_news(frame$yields, frame$term_premia, i = 2, step = 1),
    class = "hetid_error_bad_argument"
  )
})

test_that("the quarterly boundary horizon computes on the bundled 3-month grid", {
  acm <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    maturities = c(3, 6),
    frequency = "quarterly",
    use_incomplete_quarters = FALSE
  )
  yields <- acm[, c("y3", "y6"), drop = FALSE]
  tp <- acm[, c("tp3", "tp6"), drop = FALSE]

  news <- compute_price_news(yields, tp, i = 3, step = 3)
  expect_length(news, nrow(yields) - 1)
  expect_true(all(is.finite(news)))

  k_boundary <- compute_k_hat(yields, tp, i = 3, step = 3)
  expect_true(is.finite(k_boundary))
})

test_that("validate_step rejects non-positive, fractional, and oversized steps", {
  expect_error(validate_step(0L), class = "hetid_error_bad_argument")
  expect_error(validate_step(-1L), class = "hetid_error_bad_argument")
  expect_error(validate_step(1.5), class = "hetid_error_bad_argument")
  expect_error(
    validate_step(HETID_CONSTANTS$MAX_MATURITY %/% 2 + 1L),
    class = "hetid_error_bad_argument"
  )
  expect_true(validate_step(1L))
  expect_true(validate_step(6L))
})

test_that("effective_max_maturity subtracts the step from the maximum", {
  expect_identical(
    effective_max_maturity(1L),
    HETID_CONSTANTS$MAX_MATURITY - 1L
  )
  expect_identical(
    effective_max_maturity(6L),
    HETID_CONSTANTS$MAX_MATURITY - 6L
  )
  expect_identical(
    effective_max_maturity(),
    HETID_CONSTANTS$MAX_MATURITY - HETID_CONSTANTS$DEFAULT_STEP
  )
})

test_that("news maturity validator enforces the step-aware ceiling", {
  expect_true(
    validate_news_maturity_index(HETID_CONSTANTS$MAX_MATURITY - 6L, step = 6L)
  )
  expect_error(
    validate_news_maturity_index(HETID_CONSTANTS$MAX_MATURITY - 1L, step = 6L),
    class = "hetid_error_bad_argument"
  )
})

test_that("n_hat keeps its preserved bounds under the default step", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_n_hat(test_env$yields, test_env$term_premia, i = HETID_CONSTANTS$MAX_MATURITY),
    class = "hetid_error_bad_argument"
  )
  k_max <- compute_k_hat(
    test_env$yields, test_env$term_premia,
    i = HETID_CONSTANTS$MAX_MATURITY
  )
  expect_true(is.finite(k_max))
})
