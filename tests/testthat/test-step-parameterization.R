# Step parameterization: explicit step = DEFAULT_STEP must reproduce the
# omitted-step results exactly, and non-default steps must follow the
# hand-computed formulas on synthetic data.

# Synthetic frame with columns only at the maturities a step = 2 chain
# touches; deterministic values so expectations are hand-computable
make_step_two_frame <- function() {
  y2 <- c(2.0, 2.1, 2.3, 2.2, 2.4, 2.6, 2.5, 2.7)
  y4 <- c(3.0, 3.2, 3.1, 3.3, 3.5, 3.4, 3.6, 3.8)
  y6 <- c(4.1, 4.0, 4.2, 4.4, 4.3, 4.5, 4.7, 4.6)
  tp2 <- c(0.20, 0.22, 0.21, 0.23, 0.25, 0.24, 0.26, 0.28)
  tp4 <- c(0.40, 0.41, 0.43, 0.42, 0.44, 0.46, 0.45, 0.47)
  tp6 <- c(0.60, 0.62, 0.61, 0.63, 0.65, 0.64, 0.66, 0.68)
  list(
    yields = data.frame(y2 = y2, y4 = y4, y6 = y6),
    term_premia = data.frame(tp2 = tp2, tp4 = tp4, tp6 = tp6)
  )
}

test_that("explicit default step reproduces omitted-step results on bundled data", {
  test_env <- setup_standard_test_env()
  yields <- test_env$yields
  tp <- test_env$term_premia
  s <- HETID_CONSTANTS$DEFAULT_STEP

  expect_identical(
    compute_n_hat(yields, tp, i = 5, step = s),
    compute_n_hat(yields, tp, i = 5)
  )
  expect_identical(
    compute_price_news(yields, tp, i = 5, step = s),
    compute_price_news(yields, tp, i = 5)
  )
  expect_identical(
    compute_sdf_innovations(yields, tp, i = 5, step = s),
    compute_sdf_innovations(yields, tp, i = 5)
  )
  expect_identical(
    compute_c_hat(yields, tp, i = 5, step = s),
    compute_c_hat(yields, tp, i = 5)
  )
  expect_identical(
    compute_k_hat(yields, tp, i = 5, step = s),
    compute_k_hat(yields, tp, i = 5)
  )
  expect_identical(
    compute_variance_bound(yields, tp, i = 5, step = s),
    compute_variance_bound(yields, tp, i = 5)
  )
})

test_that("explicit default step reproduces omitted-step w2 residuals", {
  test_env <- setup_standard_test_env()
  set.seed(42)
  pcs <- matrix(rnorm(nrow(test_env$yields) * 4), ncol = 4)

  res_omitted <- compute_w2_residuals(
    test_env$yields, test_env$term_premia,
    maturities = c(2, 3), n_pcs = 4, pcs = pcs
  )
  res_explicit <- compute_w2_residuals(
    test_env$yields, test_env$term_premia,
    maturities = c(2, 3), n_pcs = 4, pcs = pcs,
    step = HETID_CONSTANTS$DEFAULT_STEP
  )
  expect_identical(res_omitted, res_explicit)
})

test_that("n_hat with step = 2 matches the hand formula", {
  frame <- make_step_two_frame()
  pct <- HETID_CONSTANTS$PERCENT_TO_DECIMAL
  units <- HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR

  n_hat_2 <- compute_n_hat(frame$yields, frame$term_premia, i = 2, step = 2)
  expected_2 <- ((2 / units) * frame$yields$y2 - (4 / units) * frame$yields$y4 +
    (4 / units) * frame$term_premia$tp4 - (2 / units) * frame$term_premia$tp2) / pct
  expect_equal(n_hat_2, expected_2)

  n_hat_4 <- compute_n_hat(frame$yields, frame$term_premia, i = 4, step = 2)
  expected_4 <- ((4 / units) * frame$yields$y4 - (6 / units) * frame$yields$y6 +
    (6 / units) * frame$term_premia$tp6 - (4 / units) * frame$term_premia$tp4) / pct
  expect_equal(n_hat_4, expected_4)
})

test_that("n_hat previous boundary at i == step uses the step-maturity yield", {
  frame <- make_step_two_frame()
  pct <- HETID_CONSTANTS$PERCENT_TO_DECIMAL
  units <- HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR

  boundary <- compute_n_hat_previous(frame$yields, frame$term_premia, i = 2, step = 2)
  expect_equal(boundary, -(2 / units) * frame$yields$y2 / pct)
})

test_that("k_hat with step = 2 shifts by whole news periods", {
  frame <- make_step_two_frame()
  pct <- HETID_CONSTANTS$PERCENT_TO_DECIMAL
  units <- HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  n_obs <- nrow(frame$yields)

  k_hat <- compute_k_hat(frame$yields, frame$term_premia, i = 4, step = 2)

  n_hat_prev <- ((2 / units) * frame$yields$y2 - (4 / units) * frame$yields$y4 +
    (4 / units) * frame$term_premia$tp4 - (2 / units) * frame$term_premia$tp2) / pct
  horizon_periods <- 2
  realized <- -(2 / units) * frame$yields$y2[(horizon_periods + 1):n_obs] / pct
  forecast <- n_hat_prev[2:(n_obs - horizon_periods + 1)]
  expect_equal(k_hat, mean((realized - forecast)^4))
})

test_that("k_hat rejects a maturity that is not a multiple of step", {
  frame <- make_step_two_frame()
  expect_error(
    compute_k_hat(frame$yields, frame$term_premia, i = 3, step = 2),
    class = "hetid_error_bad_argument"
  )
})

test_that("price news with step = 2 differences the step-spaced n_hat series", {
  frame <- make_step_two_frame()
  n_obs <- nrow(frame$yields)

  news <- compute_price_news(frame$yields, frame$term_premia, i = 4, step = 2)
  n_hat_4 <- compute_n_hat(frame$yields, frame$term_premia, i = 4, step = 2)
  n_hat_2 <- compute_n_hat(frame$yields, frame$term_premia, i = 2, step = 2)
  expect_equal(news, n_hat_2[2:n_obs] - n_hat_4[1:(n_obs - 1)])
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
  expect_true(validate_step(2L))
})

test_that("effective_max_maturity subtracts the step from the maximum", {
  expect_identical(
    effective_max_maturity(1L),
    HETID_CONSTANTS$MAX_MATURITY - 1L
  )
  expect_identical(
    effective_max_maturity(2L),
    HETID_CONSTANTS$MAX_MATURITY - 2L
  )
  expect_identical(
    effective_max_maturity(),
    HETID_CONSTANTS$MAX_MATURITY - HETID_CONSTANTS$DEFAULT_STEP
  )
})

test_that("news maturity validator enforces the step-aware ceiling", {
  expect_true(
    validate_news_maturity_index(HETID_CONSTANTS$MAX_MATURITY - 2L, step = 2L)
  )
  expect_error(
    validate_news_maturity_index(HETID_CONSTANTS$MAX_MATURITY - 1L, step = 2L),
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
