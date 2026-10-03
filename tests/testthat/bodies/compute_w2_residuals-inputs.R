{
  test_env <- setup_standard_test_env()

  set.seed(42)
  user_pcs <- matrix(
    rnorm(nrow(test_env$yields) * 2),
    ncol = 2
  )

  expect_no_warning(
    expect_no_message(
      compute_w2_residuals(
        test_env$yields, test_env$term_premia,
        maturities = 60, n_pcs = 2,
        pcs = user_pcs, dates = test_env$data$date
      )
    )
  )
}

{
  test_env <- setup_standard_test_env()

  set.seed(42)
  user_pcs <- matrix(
    rnorm(nrow(test_env$yields) * 2),
    ncol = 2
  )

  # PCs provided, dates required (full-T, one per yield row); no message
  result <- expect_no_message(
    compute_w2_residuals(
      test_env$yields, test_env$term_premia,
      maturities = 60, n_pcs = 2,
      pcs = user_pcs, return_df = TRUE, dates = test_env$data$date
    )
  )
  # The date column is the real t+1 (lead) realization Date, drawn from the
  # supplied full-T dates shifted by one (dates[-1])
  expect_s3_class(result$date, "Date")
  expect_true(all(result$date %in% test_env$data$date[-1]))
}

{
  test_env <- setup_standard_test_env()

  set.seed(42)
  user_pcs <- matrix(
    rnorm(nrow(test_env$yields) * 2),
    ncol = 2
  )
  # Full-T dates: one per yield row (length nrow(yields)); internally shifted
  # to the t+1 realization dates user_dates[-1]
  user_dates <- seq(
    as.Date("2000-01-01"),
    length.out = nrow(test_env$yields),
    by = "quarter"
  )

  result <- expect_no_message(
    compute_w2_residuals(
      test_env$yields, test_env$term_premia,
      maturities = 60, n_pcs = 2,
      pcs = user_pcs, return_df = TRUE,
      dates = user_dates
    )
  )
  # Verify user-supplied dates are used (shifted to t+1), not bundled
  expect_true(all(result$date %in% user_dates[-1]))
  expect_false(any(result$date %in% test_env$data$date))
}

{
  test_env <- setup_standard_test_env()

  set.seed(42)
  n <- nrow(test_env$yields)
  user_pcs <- matrix(rnorm(n * 2), ncol = 2)
  user_pcs[10, 1] <- NA

  # Full-T real dates (one per yield row); internally shifted to the t+1
  # realization dates user_dates[-1], so we can verify alignment
  user_dates <- seq(as.Date("1990-03-31"), by = "quarter", length.out = n)

  result <- compute_w2_residuals(
    test_env$yields, test_env$term_premia,
    maturities = 60, n_pcs = 2,
    pcs = user_pcs, return_df = TRUE,
    dates = user_dates
  )

  # The realization date for lagged PC row 10 is user_dates[-1][10] ==
  # user_dates[11]; it must be missing because that row's PC was NA
  dropped_date <- user_dates[-1][10]
  expect_false(dropped_date %in% result$date)

  # The date column is a real Date, never integer row indices
  expect_s3_class(result$date, "Date")

  expect_equal(
    length(result$date),
    length(result$residuals)
  )

  # Dates should not be the unbroken t+1 index (they should skip the NA row)
  expect_false(
    identical(result$date, user_dates[-1])
  )
}

{
  test_env <- setup_standard_test_env()

  # 2-column PCs but n_pcs = 4
  set.seed(42)
  bad_pcs <- matrix(rnorm(nrow(test_env$yields) * 2), ncol = 2)

  expect_error(
    compute_w2_residuals(
      test_env$yields, test_env$term_premia,
      maturities = 60, n_pcs = 4, pcs = bad_pcs
    ),
    "n_pcs must be between 1 and 2",
    class = "hetid_error_bad_argument"
  )
}

{
  test_env <- setup_standard_test_env()

  expect_error(
    compute_w2_residuals(test_env$yields, test_env$term_premia, n_pcs = 0),
    "n_pcs must be between"
  )

  expect_error(
    compute_w2_residuals(test_env$yields, test_env$term_premia, n_pcs = 100),
    "n_pcs must be between"
  )

  expect_error(
    compute_w2_residuals(test_env$yields, test_env$term_premia, n_pcs = NA),
    "n_pcs must be a single finite"
  )
}

{
  test_env <- setup_standard_test_env()

  expect_error(
    suppressWarnings(compute_w2_residuals(
      test_env$yields, test_env$term_premia,
      maturities = c(18.5, 36)
    )),
    "must be finite integer values"
  )

  expect_error(
    suppressWarnings(compute_w2_residuals(
      test_env$yields, test_env$term_premia,
      maturities = c(-1, 24)
    )),
    "must be between 1 and"
  )
}

{
  # Maturity 36 needs y48/tp48, which y12..y36 data lacks; previously
  # this hard-errored and destroyed the maturity-24 results too
  inputs <- make_w2_skip_inputs(c(12, 24, 36))

  expect_warning(
    result <- compute_w2_residuals(
      inputs$yields, inputs$term_premia,
      maturities = c(24, 36), n_pcs = 2, pcs = inputs$pcs,
      dates = inputs$dates
    ),
    "[Ss]kipping maturity 36"
  )
  expect_named(result$residuals, "maturity_24")
  expect_true(length(result$residuals$maturity_24) > 0)
  expect_true(is.finite(result$r_squared[1]))
}
