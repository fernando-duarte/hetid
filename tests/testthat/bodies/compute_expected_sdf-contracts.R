{
  test_env <- setup_standard_test_env()
  step <- HETID_CONSTANTS$DEFAULT_STEP
  i <- 60

  result <- compute_expected_sdf(
    test_env$yields, test_env$term_premia,
    i = i, paired = TRUE, dates = test_env$data$date
  )$expected_sdf

  n_hat <- n_hat_series(test_env$yields, test_env$term_premia, i = i)
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
}

{
  # step = 6: the one-period bond is y6 and the lead is s = i/step = 2 rows
  # y12 = y18 = tp12 = tp18 = 0 => n_hat(12, step = 6) = 0 => exp(n_hat) = 1
  step <- 6L
  y6_pct <- c(0, 2, 5, 9, 14, 20, 27)
  n <- length(y6_pct)
  zeros <- numeric(n)
  yields <- data.frame(y6 = y6_pct, y12 = zeros, y18 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp18 = zeros)
  dts <- seq(as.Date("1990-03-31"), by = "quarter", length.out = n)

  result <- compute_expected_sdf(
    yields, term_premia,
    i = 12, step = step, paired = TRUE, dates = dts
  )$expected_sdf

  s <- 12L %/% step
  # the non-unit m_step = 0.5 isolates the y6 lead; a y12- or step=12-hardcoded
  # implementation cannot pass this
  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  realized <- exp(-m_step * y6_pct[(s + 1):n] /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL)
  correction <- mean(realized) - 1 # exp(n_hat) = 1 everywhere
  expect_equal(result, rep(1 + correction, n), tolerance = 1e-12)
}

{
  test_env <- setup_standard_test_env()
  step <- HETID_CONSTANTS$DEFAULT_STEP
  i <- 60
  s <- i %/% step

  # Interior NA in the realized one-period (y12) leg only; n_hat(60) does
  # not use y12, so exp(n_hat) stays finite and one paired term drops out
  yields_na <- test_env$yields
  yields_na$y12[40] <- NA_real_

  result <- compute_expected_sdf(
    yields_na, test_env$term_premia,
    i = i, paired = TRUE, dates = test_env$data$date
  )$expected_sdf

  n_hat <- n_hat_series(yields_na, test_env$term_premia, i = i)
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
}

{
  test_env <- setup_standard_test_env()
  step <- HETID_CONSTANTS$DEFAULT_STEP
  i <- 48
  s <- i %/% step

  result <- compute_expected_sdf(
    test_env$yields, test_env$term_premia,
    i = i, paired = TRUE, dates = test_env$data$date
  )$expected_sdf
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
}

{
  # y24 = y36 = tp24 = tp36 = 0 => n_hat(24) = 0 => exp(n_hat) = 1, so the
  # correction is mean(exp(-y12[t+2]/100)) - 1; distinct y12 values pin the shift
  y12_pct <- c(0, 1, 3, 6, 10, 15, 21)
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)
  dts <- seq(as.Date("1990-03-31"), by = "quarter", length.out = n)

  result <- compute_expected_sdf(
    yields, term_premia,
    i = 24, paired = TRUE, dates = dts
  )$expected_sdf

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
}

{
  # y24 = y36 = tp* = 0 => n_hat(24) = 0 => exp(n_hat) = 1, so the default
  # correction averages all rows (no i / step lead), not the paired t + s window
  y12_pct <- c(0, 1, 3, 6, 10, 15, 21)
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)
  dts <- seq(as.Date("1990-03-31"), by = "quarter", length.out = n)

  result <- compute_expected_sdf(
    yields, term_premia,
    i = 24, dates = dts
  )$expected_sdf # default

  m_step <- HETID_CONSTANTS$DEFAULT_STEP / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  realized_all <- exp(-m_step * y12_pct / HETID_CONSTANTS$PERCENT_TO_DECIMAL)
  correction <- mean(realized_all) - 1 # exp(n_hat) = 1 over all rows
  expect_equal(result, rep(1 + correction, n), tolerance = 1e-12)

  # paired averages only the t + s window => a different value
  paired_result <- compute_expected_sdf(
    yields, term_premia,
    i = 24, paired = TRUE, dates = dts
  )$expected_sdf
  expect_false(isTRUE(all.equal(result, paired_result)))
}

{
  # The horizon-agnostic correction needs no i / step lead, so a non-multiple
  # of step is admissible; paired = TRUE rejects it. Needs monthly columns
  data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    maturities = c(12, 18, 30)
  )
  yields <- data[, paste0("y", c(12, 18, 30))]
  term_premia <- data[, paste0("tp", c(12, 18, 30))]

  # 18 is not a multiple of 12; default correction admits it
  result <- compute_expected_sdf(
    yields, term_premia,
    i = 18, dates = data$date
  )$expected_sdf
  expect_type(result, "double")
  expect_length(result, nrow(yields))

  expect_error(
    compute_expected_sdf(yields, term_premia, i = 18, paired = TRUE),
    "multiple of step"
  )
}
