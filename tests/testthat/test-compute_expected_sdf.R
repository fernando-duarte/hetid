test_that("compute_expected_sdf returns a numeric level series of length T", {
  test_env <- setup_standard_test_env()

  result <- compute_expected_sdf(test_env$yields, test_env$term_premia, i = 60)

  expect_type(result, "double")
  expect_length(result, nrow(test_env$yields))
  expect_true(all(is.finite(result)))
})

test_that("compute_expected_sdf returns a dated data frame when requested", {
  test_env <- setup_standard_test_env()

  result <- compute_expected_sdf(
    test_env$yields, test_env$term_premia,
    i = 60,
    return_df = TRUE,
    dates = test_env$data$date
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("date", "expected_sdf"))
  expect_equal(nrow(result), nrow(test_env$yields))
  expect_equal(result$date, test_env$data$date)
})

test_that("compute_expected_sdf drops an Inf realized leg from the correction", {
  # A wildly negative one-period yield overflows exp() to Inf. is.finite()
  # masking must drop that pair; a !is.na() implementation would keep the
  # Inf and poison the scalar correction (and the whole output series).
  y12_pct <- c(0, 0, -1e5, 2) # -1e5% -> exp(+1000) = Inf in the realized leg
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  result <- compute_expected_sdf(yields, term_premia, i = 24)

  expect_length(result, n)
  expect_true(all(is.finite(result)))
})

test_that("compute_expected_sdf matches manual exp(n_hat) + correction", {
  test_env <- setup_standard_test_env()
  step <- HETID_CONSTANTS$DEFAULT_STEP
  i <- 60

  result <- compute_expected_sdf(test_env$yields, test_env$term_premia, i = i)

  n_hat <- compute_n_hat(test_env$yields, test_env$term_premia, i = i)
  y_step <- test_env$yields$y12
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  s <- i %/% step
  n_obs <- length(n_hat)

  exp_n_hat <- exp(n_hat)
  paired <- seq_len(n_obs - s)
  realized <- exp(-m_step * y_step[(s + 1):n_obs] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL)
  valid <- is.finite(realized) & is.finite(exp_n_hat[paired])
  correction <- mean(realized[valid] - exp_n_hat[paired][valid])
  expected <- exp_n_hat + correction

  expect_equal(result, expected, tolerance = 1e-12)
})

test_that("compute_expected_sdf ignores the one-period term premium at i = step", {
  # At i = step the n_hat normalization TP^(1) := 0 drops tp{step}; the
  # realized leg uses the y{step} yield, not tp{step}, so perturbing tp12
  # must leave the result unchanged.
  test_env <- setup_standard_test_env()
  tp_perturbed <- test_env$term_premia
  tp_perturbed$tp12 <- tp_perturbed$tp12 + 1

  base <- compute_expected_sdf(
    test_env$yields, test_env$term_premia,
    i = HETID_CONSTANTS$DEFAULT_STEP
  )
  perturbed <- compute_expected_sdf(
    test_env$yields, tp_perturbed,
    i = HETID_CONSTANTS$DEFAULT_STEP
  )

  expect_equal(base, perturbed, tolerance = 1e-12)
})

test_that("compute_expected_sdf honors a non-default step", {
  # step = 6: the one-period bond is y6, and the lead is s = i/step = 2
  # rows. y12 = y18 = tp12 = tp18 = 0 => n_hat(12, step = 6) = 0 =>
  # exp(n_hat) = 1, so the correction isolates the y6 lead with a
  # non-unit m_step = step / MATURITY_UNITS_PER_YEAR = 0.5. A y12- or
  # step=12-hardcoded implementation cannot pass this.
  step <- 6L
  y6_pct <- c(0, 2, 5, 9, 14, 20, 27)
  n <- length(y6_pct)
  zeros <- numeric(n)
  yields <- data.frame(y6 = y6_pct, y12 = zeros, y18 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp18 = zeros)

  result <- compute_expected_sdf(
    yields, term_premia,
    i = 12, step = step
  )

  s <- 12L %/% step
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  realized <- exp(-m_step * y6_pct[(s + 1):n] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL)
  correction <- mean(realized) - 1 # exp(n_hat) = 1 everywhere
  expect_equal(result, rep(1 + correction, n), tolerance = 1e-12)
})

test_that("compute_expected_sdf averages the correction over finite pairs only", {
  test_env <- setup_standard_test_env()
  step <- HETID_CONSTANTS$DEFAULT_STEP
  i <- 60
  s <- i %/% step

  # Interior NA in the realized one-period (y12) leg only; n_hat(60) does
  # not use y12, so exp(n_hat) stays finite and one paired term drops out.
  yields_na <- test_env$yields
  yields_na$y12[40] <- NA_real_

  result <- compute_expected_sdf(yields_na, test_env$term_premia, i = i)

  n_hat <- compute_n_hat(yields_na, test_env$term_premia, i = i)
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  n_obs <- length(n_hat)
  exp_n_hat <- exp(n_hat)
  paired <- seq_len(n_obs - s)
  realized <- exp(-m_step * yields_na$y12[(s + 1):n_obs] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL)
  valid <- is.finite(realized) & is.finite(exp_n_hat[paired])
  correction <- mean(realized[valid] - exp_n_hat[paired][valid])

  expect_true(anyNA(realized)) # the NA reaches the realized window
  expect_equal(result, exp_n_hat + correction, tolerance = 1e-12)
})

test_that("expected_sdf series mean-matches realized one-period price over T_i", {
  test_env <- setup_standard_test_env()
  step <- HETID_CONSTANTS$DEFAULT_STEP
  i <- 48
  s <- i %/% step

  result <- compute_expected_sdf(test_env$yields, test_env$term_premia, i = i)
  n_obs <- length(result)

  y_step <- test_env$yields$y12
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  realized <- exp(-m_step * y_step[(s + 1):n_obs] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL)

  # Averaging the estimator over T_i = {1, ..., T - s} recovers the
  # sample mean of the realized one-period price
  expect_equal(
    mean(result[seq_len(n_obs - s)]), mean(realized),
    tolerance = 1e-12
  )
})

test_that("compute_expected_sdf leads the one-period yield by i/step rows", {
  # y24 = y36 = tp24 = tp36 = 0 => n_hat(24, t) = 0 => exp(n_hat) = 1, so
  # the correction reduces to mean(exp(-y12[t+2]/100)) - 1 over t = 1..T-2,
  # i.e. the last T-2 of the y12 series. Distinct y12 values make that
  # window distinguishable from any other row shift.
  y12_pct <- c(0, 1, 3, 6, 10, 15, 21)
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)

  result <- compute_expected_sdf(yields, term_premia, i = 24)

  s <- 24 %/% HETID_CONSTANTS$DEFAULT_STEP
  m_step <- HETID_CONSTANTS$DEFAULT_STEP / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  realized <- exp(-m_step * y12_pct[(s + 1):n] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL)
  correction <- mean(realized) - 1 # exp(n_hat) = 1 everywhere
  expect_equal(result, rep(1 + correction, n), tolerance = 1e-12)

  # A wrong shift (s = 1) would average a different y12 window
  wrong <- mean(exp(-m_step * y12_pct[2:n] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL)) - 1
  expect_false(isTRUE(all.equal(1 + correction, 1 + wrong)))
})

test_that("compute_expected_sdf raises when no valid correction pairs", {
  test_env <- setup_standard_test_env()
  yields_na <- test_env$yields
  yields_na$y12 <- NA_real_ # realized one-period leg all NA

  expect_error(
    compute_expected_sdf(yields_na, test_env$term_premia, i = 60),
    "No valid observations",
    class = "hetid_error_insufficient_data"
  )
})

test_that("compute_expected_sdf raises a structured error on a short series", {
  # T = 5 rows but i = 108, step = 12 needs s = 9 news periods, so the
  # paired index set is empty: must signal hetid_error_insufficient_data.
  syn <- create_synthetic_test_data(n = 5)
  expect_error(
    compute_expected_sdf(syn$yields, syn$term_premia, i = 108),
    "Not enough observations",
    class = "hetid_error_insufficient_data"
  )
})

test_that("compute_expected_sdf rejects invalid maturity values", {
  test_env <- setup_standard_test_env()

  expect_error(
    compute_expected_sdf(test_env$yields, test_env$term_premia, i = 1.5),
    "integer"
  )
  # Above the effective max (n_hat needs data at i + step)
  expect_error(
    compute_expected_sdf(test_env$yields, test_env$term_premia, i = 120),
    "between"
  )
  # Not a positive multiple of step (the realized leg shifts whole rows)
  expect_error(
    compute_expected_sdf(test_env$yields, test_env$term_premia, i = 18),
    "multiple of step"
  )
})

test_that("compute_expected_sdf rejects mismatched yields and term_premia rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  expect_error(
    compute_expected_sdf(syn_long$yields, syn_short$term_premia, i = 60),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
})
