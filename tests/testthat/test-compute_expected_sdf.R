test_that("compute_expected_sdf returns a numeric level series of length T", {
  test_env <- setup_standard_test_env()

  result <- compute_expected_sdf(
    test_env$yields, test_env$term_premia,
    i = 60, dates = test_env$data$date
  )$expected_sdf

  expect_type(result, "double")
  expect_length(result, nrow(test_env$yields))
  expect_true(all(is.finite(result)))
})

test_that("compute_expected_sdf returns a dated data frame", {
  test_env <- setup_standard_test_env()

  result <- compute_expected_sdf(
    test_env$yields, test_env$term_premia,
    i = 60,
    dates = test_env$data$date
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("date", "expected_sdf"))
  expect_equal(nrow(result), nrow(test_env$yields))
  expect_equal(result$date, test_env$data$date)
})

test_that("compute_expected_sdf drops an Inf realized leg from the correction", {
  # A wildly negative one-period yield overflows exp() to Inf; is.finite()
  # masking must drop that pair -- !is.na() would keep it and poison the series
  y12_pct <- c(0, 0, -1e5, 2) # -1e5% -> exp(+1000) = Inf in the realized leg
  n <- length(y12_pct)
  zeros <- numeric(n)
  yields <- data.frame(y12 = y12_pct, y24 = zeros, y36 = zeros)
  term_premia <- data.frame(tp12 = zeros, tp24 = zeros, tp36 = zeros)
  dts <- seq(as.Date("1990-03-31"), by = "quarter", length.out = n)

  result <- compute_expected_sdf(yields, term_premia, i = 24, dates = dts)$expected_sdf

  expect_length(result, n)
  expect_true(all(is.finite(result)))
})

test_that("compute_expected_sdf matches manual exp(n_hat) + correction", {
  eval(parse("bodies/compute_expected_sdf-contracts.R", encoding = "UTF-8")[[1]], environment())
})

test_that("compute_expected_sdf ignores the one-period term premium at i = step", {
  # At i = step the n_hat normalization TP^(1) := 0 drops tp{step}, and the
  # realized leg uses y{step}, so perturbing tp12 must leave the result alone
  test_env <- setup_standard_test_env()
  tp_perturbed <- test_env$term_premia
  tp_perturbed$tp12 <- tp_perturbed$tp12 + 1

  base <- compute_expected_sdf(
    test_env$yields, test_env$term_premia,
    i = HETID_CONSTANTS$DEFAULT_STEP, dates = test_env$data$date
  )$expected_sdf
  perturbed <- compute_expected_sdf(
    test_env$yields, tp_perturbed,
    i = HETID_CONSTANTS$DEFAULT_STEP, dates = test_env$data$date
  )$expected_sdf

  expect_equal(base, perturbed, tolerance = 1e-12)
})

test_that("compute_expected_sdf honors a non-default step", {
  eval(parse("bodies/compute_expected_sdf-contracts.R", encoding = "UTF-8")[[2]], environment())
})

test_that("compute_expected_sdf averages the correction over finite pairs only", {
  eval(parse("bodies/compute_expected_sdf-contracts.R", encoding = "UTF-8")[[3]], environment())
})

test_that("expected_sdf series mean-matches realized one-period price over T_i", {
  eval(parse("bodies/compute_expected_sdf-contracts.R", encoding = "UTF-8")[[4]], environment())
})

test_that("compute_expected_sdf leads the one-period yield by i/step rows", {
  eval(parse("bodies/compute_expected_sdf-contracts.R", encoding = "UTF-8")[[5]], environment())
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
  # paired index set is empty: must signal hetid_error_insufficient_data
  syn <- create_synthetic_test_data(n = 5)
  expect_error(
    compute_expected_sdf(syn$yields, syn$term_premia, i = 108, paired = TRUE),
    "Not enough observations",
    class = "hetid_error_insufficient_data"
  )
})

test_that("compute_expected_sdf at i = 0 returns the exact realized one-period price", {
  test_env <- setup_standard_test_env()
  step <- HETID_CONSTANTS$DEFAULT_STEP

  expect_warning(
    result <- compute_expected_sdf(
      test_env$yields, test_env$term_premia,
      i = 0, dates = test_env$data$date
    ),
    class = "hetid_warning_horizon_zero"
  )

  m_step <- step / HETID_CONSTANTS$MATURITY_UNITS_PER_YEAR
  realized <- exp(-m_step * test_env$yields$y12 /
    HETID_CONSTANTS$PERCENT_TO_DECIMAL)

  expect_named(result, c("date", "expected_sdf"))
  expect_equal(nrow(result), nrow(test_env$yields))
  expect_equal(result$expected_sdf, realized, tolerance = 1e-12)
})

test_that("compute_expected_sdf at i = 0 uses only y{step}, never the term premium", {
  # Horizon 0 must not route through n_hat(0), which would carry the step-bond
  # term premium the TP^(1) := 0 normalization removes, so tp12 changes nothing
  test_env <- setup_standard_test_env()
  tp_perturbed <- test_env$term_premia
  tp_perturbed$tp12 <- tp_perturbed$tp12 + 5

  base <- suppressWarnings(compute_expected_sdf(
    test_env$yields, test_env$term_premia,
    i = 0, dates = test_env$data$date
  )$expected_sdf)
  perturbed <- suppressWarnings(compute_expected_sdf(
    test_env$yields, tp_perturbed,
    i = 0, dates = test_env$data$date
  )$expected_sdf)

  expect_equal(base, perturbed, tolerance = 1e-12)
})

test_that("compute_expected_sdf still rejects negative maturities", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_expected_sdf(test_env$yields, test_env$term_premia, i = -12),
    "between"
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
  # Not a positive multiple of step (the realized leg shifts whole rows);
  # only paired = TRUE enforces this -- the default admits any maturity
  expect_error(
    compute_expected_sdf(
      test_env$yields, test_env$term_premia,
      i = 18, paired = TRUE
    ),
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

test_that("compute_expected_sdf default correction uses the full sample, no lead", {
  eval(parse("bodies/compute_expected_sdf-contracts.R", encoding = "UTF-8")[[6]], environment())
})

test_that("compute_expected_sdf default admits non-multiple maturities", {
  eval(parse("bodies/compute_expected_sdf-contracts.R", encoding = "UTF-8")[[7]], environment())
})
