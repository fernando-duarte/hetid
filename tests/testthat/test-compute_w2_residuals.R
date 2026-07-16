test_that("compute_w2_residuals works for single maturity", {
  test_env <- setup_standard_test_env()
  set.seed(123)
  pcs <- matrix(stats::rnorm(nrow(test_env$yields) * 4), nrow = nrow(test_env$yields))

  res_y2 <- suppressWarnings(compute_w2_residuals(test_env$yields, test_env$term_premia,
    maturities = 60, n_pcs = 4, pcs = pcs, dates = test_env$data$date
  ))

  expect_type(res_y2, "list")
  expect_true("residuals" %in% names(res_y2))
  expect_true("r_squared" %in% names(res_y2))
  expect_true("coefficients" %in% names(res_y2))

  expect_length(res_y2$residuals, 1)
  expect_length(res_y2$r_squared, 1)
})

test_that("compute_w2_residuals works for maturity 12", {
  test_env <- setup_standard_test_env()
  set.seed(123)
  pcs <- matrix(stats::rnorm(nrow(test_env$yields) * 4), nrow = nrow(test_env$yields))

  res_y2_mat12 <- suppressWarnings(compute_w2_residuals(test_env$yields, test_env$term_premia,
    maturities = 12, n_pcs = 4, pcs = pcs, dates = test_env$data$date
  ))

  expect_type(res_y2_mat12, "list")
  expect_true("residuals" %in% names(res_y2_mat12))
  expect_true("r_squared" %in% names(res_y2_mat12))

  expect_true("maturity_12" %in% names(res_y2_mat12$residuals))
  expect_true(length(res_y2_mat12$residuals$maturity_12) > 0)

  expect_gt(res_y2_mat12$r_squared[1], 0)
  expect_lt(res_y2_mat12$r_squared[1], 1)
})

test_that("compute_w2_residuals works for multiple maturities", {
  test_env <- setup_standard_test_env()

  set.seed(123)
  pcs <- matrix(stats::rnorm(nrow(test_env$yields) * 4), nrow = nrow(test_env$yields))

  maturities <- c(12, 24, 60, 84)
  res_y2 <- suppressWarnings(compute_w2_residuals(test_env$yields, test_env$term_premia,
    maturities = maturities, n_pcs = 4, pcs = pcs, dates = test_env$data$date
  ))

  expect_length(res_y2$residuals, length(maturities))
  expect_length(res_y2$r_squared, length(maturities))
  expect_equal(nrow(res_y2$coefficients), length(maturities))
})

test_that("residual properties check", {
  test_env <- setup_standard_test_env()
  set.seed(123)
  pcs <- matrix(stats::rnorm(nrow(test_env$yields) * 4), nrow = nrow(test_env$yields))

  res_y2 <- suppressWarnings(compute_w2_residuals(test_env$yields, test_env$term_premia,
    maturities = 36, n_pcs = 4, pcs = pcs, dates = test_env$data$date
  ))
  residuals <- res_y2$residuals[[1]]

  expect_lt(abs(mean(residuals, na.rm = TRUE)), 1e-10)

  expect_true(all(is.finite(residuals) | is.na(residuals)))
})

test_that("compute_w2_residuals uses SDF innovations", {
  test_env <- setup_standard_test_env()

  set.seed(123)
  pcs <- matrix(stats::rnorm(nrow(test_env$yields) * 4), nrow = nrow(test_env$yields))

  # compute_sdf_innovations returns a dated frame (length T, leading NA for the
  # t->t+1 alignment); drop the NA for the T-1 news vector compared here
  i <- 48
  sdf_df <- compute_sdf_innovations(
    test_env$yields, test_env$term_premia,
    i = i, dates = test_env$data$date
  )
  sdf_innov <- sdf_df$sdf_innovations[-1]

  res_y2 <- suppressWarnings(compute_w2_residuals(test_env$yields, test_env$term_premia,
    maturities = i, n_pcs = 4, pcs = pcs, dates = test_env$data$date
  ))

  # The residuals length will be limited by the shorter of SDF innovations
  # and available PCs (from variables data)
  expect_true(length(res_y2$residuals[[1]]) <= length(sdf_innov))
  expect_true(length(res_y2$residuals[[1]]) > 0)

  expect_type(res_y2$residuals[[1]], "double")
  expect_true(all(is.finite(res_y2$residuals[[1]]) | is.na(res_y2$residuals[[1]])))
})

test_that("R-squared matches manual regression", {
  data("variables", package = "hetid", envir = environment())

  mats <- HETID_CONSTANTS$DEFAULT_ACM_MATURITIES
  acm_data <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    maturities = mats,
    frequency = "quarterly",
    use_incomplete_quarters = FALSE
  )

  # Variables: Q1=Jan, Q2=Apr, Q3=Jul, Q4=Oct
  variables$year <- as.numeric(format(variables$date, "%Y"))
  variables$quarter <- ceiling(as.numeric(format(variables$date, "%m")) / 3)
  variables$year_quarter <- paste(variables$year, variables$quarter, sep = "-Q")

  # ACM: Q1=Mar, Q2=Jun, Q3=Sep, Q4=Dec
  acm_data$year <- as.numeric(format(acm_data$date, "%Y"))
  acm_data$quarter <- ceiling(as.numeric(format(acm_data$date, "%m")) / 3)
  acm_data$year_quarter <- paste(acm_data$year, acm_data$quarter, sep = "-Q")

  merged_data <- merge(
    variables[, c("year_quarter", "date", get_pc_column_names(4))],
    acm_data[, c("year_quarter", "date", paste0("y", mats), paste0("tp", mats))],
    by = "year_quarter",
    suffixes = c("_var", "_acm")
  )

  pcs_merged <- as.matrix(merged_data[, get_pc_column_names(4)])
  yields_merged <- merged_data[, paste0("y", mats)]
  term_premia_merged <- merged_data[, paste0("tp", mats)]

  i <- 60

  # Dated return prepends a leading NA to align news to t+1; drop it for the
  # T-1 news vector aligned with the lagged PCs below (news[t] pairs with PC_t)
  sdf_df <- compute_sdf_innovations(
    yields_merged, term_premia_merged,
    i = i, dates = merged_data$date_acm
  )
  sdf_innov <- sdf_df$sdf_innovations[-1]

  # Create lagged PCs (aligned with SDF innovations which have length T-1)
  n_obs <- nrow(merged_data)
  pcs_lagged <- pcs_merged[1:(n_obs - 1), ]

  complete_idx <- complete.cases(sdf_innov, pcs_lagged)
  y_clean <- sdf_innov[complete_idx]
  pcs_clean <- pcs_lagged[complete_idx, ]

  reg_data_manual <- data.frame(
    y = y_clean,
    pc1 = pcs_clean[, 1],
    pc2 = pcs_clean[, 2],
    pc3 = pcs_clean[, 3],
    pc4 = pcs_clean[, 4]
  )

  manual_model <- lm(y ~ pc1 + pc2 + pc3 + pc4, data = reg_data_manual)
  manual_r2 <- summary(manual_model)$r.squared
  manual_coefs <- coef(manual_model)

  res_w2 <- compute_w2_residuals(
    yields_merged,
    term_premia_merged,
    maturities = i,
    n_pcs = 4,
    pcs = pcs_merged,
    dates = merged_data$date_acm
  )

  expect_equal(res_w2$r_squared[1], manual_r2,
    tolerance = 1e-10,
    label = "R-squared should match manual calculation"
  )

  function_coefs <- res_w2$coefficients[1, ]
  names(function_coefs) <- names(manual_coefs)

  expect_equal(function_coefs, manual_coefs,
    tolerance = 1e-10,
    label = "Coefficients should match manual calculation"
  )

  expect_equal(length(res_w2$residuals[[1]]), length(residuals(manual_model)),
    label = "Residuals should have same length"
  )

  expect_lt(abs(mean(res_w2$residuals[[1]])), 1e-10,
    label = "Residuals should have near-zero mean"
  )
})

test_that("quarterly data alignment test", {
  data("variables", package = "hetid", envir = environment())
  acm_monthly <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "monthly"
  )
  acm_quarterly <- extract_acm_data(
    data_types = c("yields", "term_premia"),
    frequency = "quarterly",
    use_incomplete_quarters = FALSE
  )

  expect_lt(nrow(acm_quarterly), nrow(acm_monthly))

  quarterly_months <- format(acm_quarterly$date, "%m")
  expect_true(all(quarterly_months %in% c("03", "06", "09", "12")))
})

test_that("length verification for output", {
  test_env <- setup_standard_test_env()
  set.seed(123)
  pcs <- matrix(stats::rnorm(nrow(test_env$yields) * 4), nrow = nrow(test_env$yields))

  maturities <- seq(12, 108, by = 12)
  res_y2 <- suppressWarnings(compute_w2_residuals(test_env$yields, test_env$term_premia,
    maturities = maturities, n_pcs = 4, pcs = pcs, dates = test_env$data$date
  ))

  expect_length(res_y2$residuals, 9)
  expect_length(res_y2$r_squared, 9)

  residual_lengths <- lengths(res_y2$residuals)
  expect_true(all(residual_lengths == residual_lengths[1]))
})

test_that("no message when user provides PCs", {
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
})

test_that("return_df date column is the real t+1 Date when user provides PCs", {
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
})

test_that("no message for dates when user provides dates", {
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
})

test_that("return_df dates align correctly with interior NA in PCs", {
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
})

test_that("error when user pcs has wrong number of rows", {
  test_env <- setup_standard_test_env()

  # PCs with 5 rows but yields has many more
  bad_pcs <- matrix(rnorm(5 * 4), ncol = 4)

  expect_error(
    compute_w2_residuals(
      test_env$yields, test_env$term_premia,
      maturities = 60, n_pcs = 4, pcs = bad_pcs,
      dates = test_env$data$date
    ),
    "must match number of rows"
  )
})

test_that("error when pcs has fewer columns than n_pcs", {
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
})

test_that("error when n_pcs is invalid", {
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
})

test_that("error when maturities are non-integer or negative", {
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
})

# Synthetic inputs for the warn-and-skip contract tests:
# yields/term_premia restricted to the requested column indices,
# with user-supplied PCs to avoid the bundled-data fallback
make_w2_skip_inputs <- function(col_indices, n = 40, seed = 123) {
  set.seed(seed)
  yields <- as.data.frame(
    matrix(rnorm(n * length(col_indices), mean = 2), nrow = n)
  )
  names(yields) <- paste0("y", col_indices)
  term_premia <- as.data.frame(
    matrix(rnorm(n * length(col_indices), mean = 0.5), nrow = n)
  )
  names(term_premia) <- paste0("tp", col_indices)
  list(
    yields = yields,
    term_premia = term_premia,
    pcs = matrix(rnorm(n * 2), ncol = 2),
    dates = seq(as.Date("1990-03-31"), by = "quarter", length.out = n)
  )
}

test_that("maturity equal to the highest available column skips while others succeed", {
  inputs <- make_w2_skip_inputs(c(12, 24))

  expect_warning(
    result <- compute_w2_residuals(
      inputs$yields, inputs$term_premia,
      maturities = c(12, 24), n_pcs = 2, pcs = inputs$pcs,
      dates = inputs$dates
    ),
    "[Ss]kipping maturity 24"
  )
  expect_named(result$residuals, "maturity_12")
  expect_true(is.finite(result$r_squared[1]))
  expect_true(is.na(result$r_squared[2]))
})

test_that("one bad maturity does not abort valid maturities", {
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
})

test_that("non-contiguous column subsets pass validation and compute", {
  # Maturity 60 needs only columns 48/60/72; y12/tp12 are extra
  inputs <- make_w2_skip_inputs(c(12, 48, 60, 72))

  result <- compute_w2_residuals(
    inputs$yields, inputs$term_premia,
    maturities = 60, n_pcs = 2, pcs = inputs$pcs,
    dates = inputs$dates
  )
  expect_named(result$residuals, "maturity_60")
  expect_true(is.finite(result$r_squared[1]))
  expect_true(result$n_obs[1] > 0)
})

test_that("skipped maturities report NA, not zero", {
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
})

test_that("all maturities skipped with return_df gives zero-row data frame", {
  inputs <- make_w2_skip_inputs(c(12, 24))

  expect_warning(
    result <- compute_w2_residuals(
      inputs$yields, inputs$term_premia,
      maturities = 36, n_pcs = 2, pcs = inputs$pcs,
      return_df = TRUE, dates = inputs$dates
    ),
    "[Ss]kipping maturity 36"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, c("date", "maturity", "residuals", "fitted"))
})

test_that("list mode returns per-maturity dates parallel to residuals", {
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
})

test_that("list-mode dates are ragged when maturities drop different rows", {
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
})

test_that("list-mode dates are real Dates per maturity for custom PCs", {
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
})

test_that("list-mode and data-frame-mode dates agree", {
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
})
