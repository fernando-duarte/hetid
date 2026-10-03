{
  inputs <- make_w2_skip_inputs(c(12, 24, 36))

  expect_warning(
    result <- compute_w2_residuals(
      inputs$yields, inputs$term_premia,
      maturities = c(24, 36), n_pcs = 2, pcs = inputs$pcs,
      dates = inputs$dates
    ),
    "[Ss]kipping maturity 36"
  )
  expect_true(is.na(result$r_squared[2]))
  expect_true(is.na(result$n_obs[2]))
  expect_false("maturity_36" %in% names(result$residuals))
  expect_true(all(is.na(result$coefficients["maturity_36", ])))
  expect_named(result$skipped, "maturity_36")
  expect_match(result$skipped[["maturity_36"]], "[Ss]kipping maturity 36")
}

{
  test_data <- create_synthetic_test_data(n = 40, n_maturities = 3)
  # Full-T dates: one per yield row (40); shifted internally to the t+1
  # realization index user_dates[-1] (39 W2 news observations)
  user_dates <- seq(as.Date("2000-01-01"), by = "quarter", length.out = 40)

  res <- suppressWarnings(compute_w2_residuals(
    test_data$yields, test_data$term_premia,
    maturities = c(12, 24, 36), n_pcs = 3,
    pcs = matrix(rnorm(40 * 3), nrow = 40), dates = user_dates
  ))

  # dates is a per-maturity named list, keyed exactly like residuals
  expect_type(res$dates, "list")
  expect_identical(names(res$dates), names(res$residuals))

  # each maturity's dates align 1:1 with its residual vector
  for (key in names(res$residuals)) {
    expect_length(res$dates[[key]], length(res$residuals[[key]]))
    # and equal the t+1 dates (user_dates[-1]) subset by that maturity's kept_idx
    expect_identical(res$dates[[key]], user_dates[-1][which(res$kept_idx[[key]])])
  }
}

{
  test_data <- create_synthetic_test_data(n = 40, n_maturities = 3)
  # Punch an interior NA into maturity 24 only -> its kept_idx shrinks while
  # maturities 12 and 36 keep every row: a flat date vector could not align all
  test_data$yields[15, "y24"] <- NA
  test_data$term_premia[15, "tp24"] <- NA
  # Full-T dates (40); shifted internally to the t+1 realization index
  user_dates <- seq(as.Date("2000-01-01"), by = "quarter", length.out = 40)

  res <- suppressWarnings(compute_w2_residuals(
    test_data$yields, test_data$term_premia,
    maturities = c(12, 24, 36), n_pcs = 3,
    pcs = matrix(rnorm(40 * 3), nrow = 40), dates = user_dates
  ))

  len12 <- length(res$dates[["maturity_12"]])
  len24 <- length(res$dates[["maturity_24"]])
  expect_lt(len24, len12)
  expect_length(res$dates[["maturity_24"]], length(res$residuals[["maturity_24"]]))
  # the dropped date is exactly the one masked by kept_idx, not a blind shift
  expect_identical(
    res$dates[["maturity_24"]],
    user_dates[-1][which(res$kept_idx[["maturity_24"]])]
  )
}

{
  test_data <- create_synthetic_test_data(n = 30, n_maturities = 2)
  # Dates are mandatory now (full-T, one per yield row); there is no row-index
  # fallback. The per-maturity dates are still a parallel named list
  user_dates <- seq(as.Date("2000-01-01"), by = "quarter", length.out = 30)
  res <- suppressWarnings(compute_w2_residuals(
    test_data$yields, test_data$term_premia,
    maturities = c(12, 24), n_pcs = 2,
    pcs = matrix(rnorm(30 * 2), nrow = 30), dates = user_dates
  ))
  expect_type(res$dates, "list")
  for (key in names(res$residuals)) {
    expect_length(res$dates[[key]], length(res$residuals[[key]]))
    expect_s3_class(res$dates[[key]], "Date")
    expect_identical(res$dates[[key]], user_dates[-1][which(res$kept_idx[[key]])])
  }
}

{
  test_data <- create_synthetic_test_data(n = 40, n_maturities = 3)
  test_data$yields[15, "y24"] <- NA
  # Full-T dates: one per yield row (40), shifted internally to t+1
  user_dates <- seq(as.Date("2000-01-01"), by = "quarter", length.out = 40)
  common <- list(
    test_data$yields, test_data$term_premia,
    maturities = c(12, 24, 36), n_pcs = 3,
    pcs = matrix(rnorm(40 * 3), nrow = 40), dates = user_dates
  )
  lst <- suppressWarnings(do.call(compute_w2_residuals, common))
  df <- suppressWarnings(do.call(compute_w2_residuals, c(common, list(return_df = TRUE))))

  # the data-frame date column for a maturity equals the list-mode dates
  df24 <- df$date[df$maturity == 24]
  expect_identical(df24, lst$dates[["maturity_24"]])
}
