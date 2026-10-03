{
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
}

{
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
}

{
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

  # The residual count is the T-1 news rows less any dropped by complete.cases
  # (the PCs are row-locked to the yields, so they cannot bind first)
  expect_true(length(res_y2$residuals[[1]]) <= length(sdf_innov))
  expect_true(length(res_y2$residuals[[1]]) > 0)

  expect_type(res_y2$residuals[[1]], "double")
  expect_true(all(is.finite(res_y2$residuals[[1]]) | is.na(res_y2$residuals[[1]])))
}

{
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
}

{
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
}

{
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
}
