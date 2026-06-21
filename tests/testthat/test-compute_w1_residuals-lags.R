# Tests for the y1_lags (lagged-outcome regressors) feature of
# compute_w1_residuals. Backward-compatibility, numerical correctness against a
# manual lm, sample trimming, date alignment, and validation errors.

test_that("y1_lags = 0 is identical to the default (backward compatible)", {
  data("variables", package = "hetid", envir = environment())
  base <- suppressMessages(compute_w1_residuals(n_pcs = 3, data = variables))
  zero <- suppressMessages(
    compute_w1_residuals(n_pcs = 3, data = variables, y1_lags = 0L)
  )
  expect_equal(zero$residuals, base$residuals, tolerance = 1e-12)
  expect_equal(zero$coefficients, base$coefficients, tolerance = 1e-12)
})

test_that("y1_lags matches a manual regression with lagged outcomes", {
  data("variables", package = "hetid", envir = environment())
  h <- 2L
  res <- suppressMessages(
    compute_w1_residuals(n_pcs = 3, data = variables, y1_lags = h)
  )

  # Manual build: regress Y1_{t+1} on PC_t and Y1_t, Y1_{t-1}.
  y1 <- variables$gr1.pcecc96
  pc <- as.matrix(variables[, get_pc_column_names(3)])
  n <- length(y1)
  lag1 <- y1 # Y_{1,t}
  lag2 <- c(NA, y1[seq_len(n - 1L)]) # Y_{1,t-1}
  reg <- cbind(pc, l.y1 = lag1, l2.y1 = lag2)
  reg_lagged <- reg[seq_len(n - 1L), , drop = FALSE]
  y_future <- y1[seq.int(2L, n)]
  keep <- stats::complete.cases(y_future, reg_lagged)
  manual <- lm(y_future[keep] ~ reg_lagged[keep, ])

  expect_equal(res$r_squared, summary(manual)$r.squared, tolerance = 1e-10)
  expect_true(all(c("l.y1", "l2.y1") %in% names(res$coefficients)))
  expect_equal(unname(res$residuals), unname(residuals(manual)), tolerance = 1e-10)
})

test_that("lagging drops exactly H-1 leading rows and trims dates", {
  data("variables", package = "hetid", envir = environment())
  base <- suppressMessages(compute_w1_residuals(n_pcs = 4, data = variables))
  lagged <- suppressMessages(
    compute_w1_residuals(n_pcs = 4, data = variables, y1_lags = 4L)
  )
  expect_equal(length(lagged$residuals), length(base$residuals) - 3L)
  # Dates are the trailing block of the no-lag dates (first H-1 dropped).
  expect_equal(lagged$dates, utils::tail(base$dates, length(lagged$dates)))
})

test_that("y1_lags works alongside exog and keeps lag-column names", {
  data("variables", package = "hetid", envir = environment())
  z <- as.matrix(variables[, get_pc_column_names(2)])
  colnames(z) <- c("", NA) # force the blank/NA sanitizer path
  res <- suppressMessages(
    compute_w1_residuals(data = variables, exog = z, y1_lags = 2L)
  )
  expect_true(all(c("l.y1", "l2.y1") %in% names(res$coefficients)))
})

test_that("y1_lags rejects invalid values", {
  data("variables", package = "hetid", envir = environment())
  expect_error(
    compute_w1_residuals(n_pcs = 3, data = variables, y1_lags = -1L),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    compute_w1_residuals(n_pcs = 3, data = variables, y1_lags = 1.5),
    class = "hetid_error_bad_argument"
  )
})

test_that("oversized y1_lags errors with insufficient data", {
  few <- data.frame(
    date = seq(as.Date("1959-03-31"), by = "quarter", length.out = 6),
    gr1.pcecc96 = c(0.5, 0.2, 0.1, 0.4, 0.3, 0.6),
    pc1 = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
    pc2 = c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
  )
  expect_error(
    compute_w1_residuals(n_pcs = 2, data = few, y1_lags = 3L),
    class = "hetid_error_insufficient_data"
  )
})
