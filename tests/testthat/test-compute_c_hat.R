test_that("compute_c_hat returns single numeric value for given maturity", {
  test_env <- setup_standard_test_env()

  c_hat_60 <- compute_c_hat(test_env$yields, test_env$term_premia, i = 60)

  expect_type(c_hat_60, "double")
  expect_length(c_hat_60, 1)
  expect_true(is.finite(c_hat_60))
})

test_that("c_hat is always positive", {
  test_env <- setup_standard_test_env()

  for (i in seq(12, 108, by = 12)) {
    c_hat_i <- compute_c_hat(test_env$yields, test_env$term_premia, i = i)
    expect_gt(c_hat_i, 0,
      label = paste("c_hat should be positive for maturity", i)
    )
  }
})

test_that("c_hat bounds are correct", {
  test_env <- setup_standard_test_env()

  c_hat_12 <- compute_c_hat(test_env$yields, test_env$term_premia, i = 12)
  expect_lt(c_hat_12, 1.02,
    label = "c_hat for maturity 12 should be below 1.02"
  )

  for (i in seq(24, 108, by = 12)) {
    c_hat_i <- compute_c_hat(test_env$yields, test_env$term_premia, i = i)
    expect_lt(c_hat_i, 1,
      label = paste("c_hat should be below 1 for maturity", i)
    )
  }
})

test_that("c_hat equals exp(2 * max(n_hat)) over the bound index set", {
  test_env <- setup_standard_test_env()
  step <- HETID_CONSTANTS$DEFAULT_STEP

  for (i in c(36, 60, 84)) {
    c_hat_i <- compute_c_hat(test_env$yields, test_env$term_premia, i = i)
    n_hat_i <- n_hat_series(test_env$yields, test_env$term_premia, i = i)

    # C_i maxes over the bound index set T_i = {1, ..., T - i/step},
    # not all dates (the realized leg needs i/step further news periods)
    horizon <- i %/% step
    n_hat_trimmed <- n_hat_i[seq_len(length(n_hat_i) - horizon)]
    expected_c_hat <- exp(2 * max(n_hat_trimmed, na.rm = TRUE))

    expect_equal(c_hat_i, expected_c_hat,
      tolerance = 1e-10,
      label = paste("c_hat should equal exp(2*max(n_hat)) over T_i for maturity", i)
    )
  }
})

test_that("c_hat degenerate branch returns typed numeric NA", {
  test_env <- setup_standard_test_env()
  yields_na <- test_env$yields

  # All-NA y60 makes n_hat all NA for any maturity that needs y60
  yields_na$y60 <- NA_real_

  # vapply(..., numeric(1)) enforces a double return on the NA branch
  c_vals <- vapply(
    c(48, 60),
    function(i) compute_c_hat(yields_na, test_env$term_premia, i = i),
    numeric(1)
  )

  expect_type(c_vals, "double")
  expect_true(all(is.na(c_vals)))
  expect_identical(
    compute_c_hat(yields_na, test_env$term_premia, i = 60),
    NA_real_
  )
})

test_that("compute_c_hat rejects mismatched yields and term_premia rows", {
  syn_long <- create_synthetic_test_data(n = 30)
  syn_short <- create_synthetic_test_data(n = 15)
  expect_error(
    compute_c_hat(syn_long$yields, syn_short$term_premia, i = 60),
    "same number of observations",
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("compute_c_hat raises a structured error on a short series", {
  # T = 5 but i = 108, step = 12 needs i/step = 9 news periods, so the bound
  # index set is empty: signal insufficient_data, not a seq_len(negative) crash
  syn <- create_synthetic_test_data(n = 5)
  expect_error(
    compute_c_hat(syn$yields, syn$term_premia, i = 108),
    "Not enough observations",
    class = "hetid_error_insufficient_data"
  )
})

test_that("compute_c_hat rejects invalid maturity values", {
  test_env <- setup_standard_test_env()
  expect_error(
    compute_c_hat(test_env$yields, test_env$term_premia, i = 1.5),
    "integer"
  )
  expect_error(
    compute_c_hat(test_env$yields, test_env$term_premia, i = 120),
    "between"
  )
})

test_that("compute_c_hat rejects a non-step-multiple maturity", {
  # i = 18 is in range but not a multiple of step = 12, so i/step is not a
  # whole number of news periods and the bound index set is ill-defined; error
  test_env <- setup_standard_test_env()
  expect_error(
    compute_c_hat(test_env$yields, test_env$term_premia, i = 18, step = 12),
    "positive multiple of step",
    class = "hetid_error_bad_argument"
  )
})
