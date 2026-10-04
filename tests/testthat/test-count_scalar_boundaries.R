test_that("instrument counts are bounded before integer coercion", {
  z <- matrix(1:6, 3, dimnames = list(NULL, c("a", "b")))
  old <- options(warn = 2)
  on.exit(options(old))
  for (count in c(.Machine$integer.max + 1, Inf, NA_real_, 1.5, 0)) {
    err <- tryCatch(align_instrument_sets(list(z), count), error = identity)
    expect_s3_class(err, "hetid_error_bad_argument")
    expect_identical(err$arg, "n_components")
    err <- tryCatch(
      lambda_from_support(list(1L), list(matrix(1)), count),
      error = identity
    )
    expect_s3_class(err, "hetid_error_bad_argument")
    expect_identical(err$arg, "j_total")
  }
  aligned <- align_instrument_sets(list(z), 1)
  expected <- z
  storage.mode(expected) <- "double"
  expect_identical(aligned, list(instruments = expected, support = list(1:2)))
  expect_identical(
    lambda_from_support(list(c(3L, 1L)), list(matrix(c(0.6, 0.8), 2)), 4),
    list(matrix(c(0.8, 0, 0.6, 0), 4))
  )
})

test_that("public maturity errors use the argument identifier and display label", {
  y <- data.frame(y24 = 4:9, y36 = 6:11)
  tp <- data.frame(tp24 = rep(0.5, 6), tp36 = rep(0.8, 6))
  dates <- as.Date(paste0(2000:2005, "-12-31"))
  for (i in list("24", NULL, NA_real_, NaN, Inf, 24.5, 0, 120)) {
    err <- tryCatch(compute_n_hat(y, tp, i, dates), error = identity)
    expect_s3_class(err, "hetid_error_bad_argument")
    expect_identical(err$arg, "i")
    expect_match(conditionMessage(err), "Maturity index i")
  }
  expect_identical(
    withVisible(assert_scalar_finite(1, "value")),
    list(value = TRUE, visible = FALSE)
  )
  err <- tryCatch(assert_scalar_finite(Inf, "display", arg = "value"), error = identity)
  expect_identical(err$arg, "value")
  expect_match(conditionMessage(err), "display must be a single finite numeric value")
  expect_equal(
    compute_n_hat(y, tp, 24, dates)$n_hat,
    (2 * y$y24 - 3 * y$y36 + 3 * tp$tp36 - 2 * tp$tp24) / 100
  )
})

test_that("lag counts reject unrepresentable values in W1 and W2 without warnings", {
  t <- seq_len(24)
  dates <- as.Date(paste0(2000 + t, "-12-31"))
  d <- data.frame(date = dates, pc1 = sin(t), gr1.pcecc96 = sin(t^2 / 7) + t / 10)
  y <- data.frame(y12 = 2 + sin(t), y24 = 4 + cos(t), y36 = 6 + sin(t / 2))
  tp <- data.frame(tp12 = rep(0, 24), tp24 = rep(0.5, 24), tp36 = rep(0.8, 24))
  pcs <- as.matrix(d["pc1"])
  w2 <- function(lags) {
    compute_w2_residuals(
      y, tp,
      maturities = 24, n_pcs = 1, pcs = pcs,
      y1 = d$gr1.pcecc96, y1_lags = lags, dates = dates
    )
  }
  old <- options(warn = 2)
  on.exit(options(old))
  for (lags in list(Inf, .Machine$integer.max + 1, NA_real_, NaN, -1, 1.5)) {
    calls <- list(
      function() validate_y1_lags(lags, nrow(d)),
      function() compute_w1_residuals(1, d, y1_lags = lags),
      function() w2(lags)
    )
    for (call in calls) {
      err <- tryCatch(call(), error = identity)
      expect_s3_class(err, "hetid_error_bad_argument")
      expect_identical(err$arg, "y1_lags")
    }
  }
  expect_identical(validate_y1_lags(0, 24), 0L)
  expect_identical(validate_y1_lags(2, 24), 2L)
  expect_error(validate_y1_lags(24, 24), class = "hetid_error_insufficient_data")
  expect_error(compute_w1_residuals(1, d, y1_lags = 24),
    class = "hetid_error_insufficient_data"
  )
  expect_error(w2(24), class = "hetid_error_insufficient_data")
  expect_identical(compute_w1_residuals(1, d), compute_w1_residuals(1, d, y1_lags = 0))
  fit <- compute_w1_residuals(1, d, y1_lags = 2)
  reg <- data.frame(
    response = d$gr1.pcecc96[3:24], pc1 = d$pc1[2:23],
    l.y1 = d$gr1.pcecc96[2:23], l2.y1 = d$gr1.pcecc96[1:22]
  )
  manual <- stats::lm(response ~ ., data = reg)
  expect_equal(fit$coefficients, stats::coef(manual), tolerance = 1e-12)
  expect_equal(unname(fit$residuals), unname(stats::residuals(manual)), tolerance = 1e-12)
  expect_identical(fit$dates, dates[3:24])
  w2_fit <- w2(2)
  expect_identical(rownames(w2_fit$coefficients), "maturity_24")
  expect_true(all(c("l.y1", "l2.y1") %in% colnames(w2_fit$coefficients)))
  sdf <- compute_sdf_innovations(y, tp, 24, dates)$sdf_innovations[-1]
  reg$response <- sdf[2:23]
  manual <- stats::lm(response ~ ., data = reg)
  expect_equal(unname(w2_fit$residuals[[1]]), unname(stats::residuals(manual)),
    tolerance = 1e-12
  )
})
