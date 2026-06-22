# Tests for the y1_lags / impose_b_zero (common-conditioning) features of
# compute_w2_residuals. Backward-compatibility, lag-column naming and trimming,
# OLS orthogonality, the literal B=0 projection, and validation errors.

# Shared aligned fixture: bundled PCs merged with quarterly ACM yields/tp by
# year-quarter, plus consumption growth on the same rows (the X_t conditioning).
make_w2_fixture <- function() {
  mats <- c(12, 24, 36, 48, 60)
  acm <- suppressWarnings(extract_acm_data(
    data_types = c("yields", "term_premia"),
    maturities = mats, frequency = "quarterly"
  ))
  data("variables", package = "hetid", envir = environment())
  variables <- get("variables", envir = environment())
  acm$yq <- paste0(format(acm$date, "%Y"), "-", quarters(acm$date))
  variables$yq <- paste0(
    format(variables$date, "%Y"), "-", quarters(variables$date)
  )
  pc_cols <- get_pc_column_names(4)
  merged <- merge(
    variables[, c("yq", pc_cols, "gr1.pcecc96")],
    acm[, c("yq", "date", paste0("y", mats), paste0("tp", mats))],
    by = "yq"
  )
  merged <- merged[order(merged$yq), ]
  list(
    pcs = as.matrix(merged[, pc_cols]),
    y1 = merged$gr1.pcecc96,
    yields = merged[, paste0("y", mats)],
    tp = merged[, paste0("tp", mats)],
    dates = merged$date
  )
}

test_that("y1_lags = 0 reproduces the default PC-only path byte-for-byte", {
  fx <- make_w2_fixture()
  base <- compute_w2_residuals(
    fx$yields, fx$tp,
    maturities = c(24, 36), n_pcs = 4, pcs = fx$pcs, dates = fx$dates
  )
  zero <- compute_w2_residuals(
    fx$yields, fx$tp,
    maturities = c(24, 36), n_pcs = 4, pcs = fx$pcs,
    y1 = fx$y1, y1_lags = 0L, dates = fx$dates
  )
  expect_equal(zero$residuals, base$residuals, tolerance = 1e-12)
  expect_equal(zero$coefficients, base$coefficients, tolerance = 1e-12)
  expect_equal(zero$r_squared, base$r_squared, tolerance = 1e-12)
})

test_that("y1_lags = H adds H named lag columns, trims rows, stays orthogonal", {
  fx <- make_w2_fixture()
  h <- 2L
  res <- compute_w2_residuals(
    fx$yields, fx$tp,
    maturities = 24, n_pcs = 4, pcs = fx$pcs,
    y1 = fx$y1, y1_lags = h, dates = fx$dates
  )
  expect_true(all(c("l.y1", "l2.y1") %in% colnames(res$coefficients)))
  expect_equal(ncol(res$coefficients), 1L + 4L + h)
  # Residuals orthogonal to the appended lag columns (OLS normal equations)
  resid <- res$residuals[["maturity_24"]]
  kept <- res$kept_idx[["maturity_24"]]
  n <- length(fx$y1)
  lag1 <- fx$y1[seq_len(n - 1L)] # Y_{1,t} at predictor row t
  lag2 <- c(NA, fx$y1[seq_len(n - 2L)]) # Y_{1,t-1}
  lag1 <- lag1[kept]
  lag2 <- lag2[kept]
  expect_lt(abs(sum(resid * lag1)), 1e-8)
  expect_lt(abs(sum(resid * lag2)), 1e-8)
})

test_that("impose_b_zero = TRUE yields literal W2 = Y2 with zero coefficients", {
  fx <- make_w2_fixture()
  fit <- compute_w2_residuals(
    fx$yields, fx$tp,
    maturities = 24, n_pcs = 4, pcs = fx$pcs,
    y1 = fx$y1, y1_lags = 2L, impose_b_zero = TRUE, dates = fx$dates
  )
  # Recover the raw SDF innovation and the complete rows for maturity 24. The
  # dated return prepends a leading NA (news realized at t+1); drop it to get the
  # bare T-1 news vector that kept_idx (a mask over the W2 rows) indexes into
  sdf_df <- compute_sdf_innovations(
    as.data.frame(fx$yields), as.data.frame(fx$tp),
    i = 24, dates = fx$dates, step = 12
  )
  sdf <- sdf_df$sdf_innovations[-1]
  kept <- fit$kept_idx[["maturity_24"]]
  resid <- fit$residuals[["maturity_24"]]
  expect_equal(unname(resid), unname(sdf[kept]), tolerance = 1e-12)
  expect_true(all(fit$fitted[["maturity_24"]] == 0))
  expect_equal(ncol(fit$coefficients), 1L + 4L + 2L)
  expect_true(all(fit$coefficients["maturity_24", ] == 0))
  expect_true(is.na(fit$r_squared[[1]]))
})

test_that("unnamed custom pcs keep l.y1* names (no relabel to pc*)", {
  fx <- make_w2_fixture()
  pcs_unnamed <- fx$pcs
  colnames(pcs_unnamed) <- NULL
  res <- compute_w2_residuals(
    fx$yields, fx$tp,
    maturities = 24, n_pcs = 4, pcs = pcs_unnamed,
    y1 = fx$y1, y1_lags = 2L, dates = fx$dates
  )
  expect_true(all(c("l.y1", "l2.y1") %in% colnames(res$coefficients)))
})

test_that("y1_lags > 0 requires a length-matched y1", {
  fx <- make_w2_fixture()
  expect_error(
    compute_w2_residuals(
      fx$yields, fx$tp,
      maturities = 24, n_pcs = 4, pcs = fx$pcs, y1_lags = 2L,
      dates = fx$dates
    ),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_w2_residuals(
      fx$yields, fx$tp,
      maturities = 24, n_pcs = 4, pcs = fx$pcs,
      y1 = fx$y1[-1], y1_lags = 2L, dates = fx$dates
    ),
    class = "hetid_error"
  )
})
