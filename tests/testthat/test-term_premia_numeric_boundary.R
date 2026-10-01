term_premia_boundary_fixture <- function() {
  list(
    dates = as.Date("2020-01-31") + seq_len(6),
    yields = data.frame(y12 = 2:7, y24 = 4:9, y36 = 6:11),
    tp = data.frame(
      tp12 = rep(9, 6), tp24 = seq(0.2, 0.7, 0.1),
      tp36 = seq(0.8, 1.3, 0.1)
    )
  )
}

test_that("required term-premium columns reject nonnumeric types before arithmetic", {
  withr::local_options(warn = 2)
  f <- term_premia_boundary_fixture()
  for (i in c(12, 24)) {
    for (column in paste0("tp", c(i, i + 12))) {
      for (bad in list(rep("0.5", 6), factor(rep("0.5", 6)))) {
        tp <- f$tp
        tp[[column]] <- bad
        calls <- list(
          function() compute_n_hat(f$yields, tp, i, f$dates),
          function() compute_expected_sdf(f$yields, tp, i, f$dates),
          function() compute_expected_sdf(f$yields, tp, i, f$dates, paired = TRUE)
        )
        for (call in calls) {
          err <- tryCatch(call(), error = identity)
          expect_s3_class(err, "hetid_error_bad_argument")
          expect_s3_class(err, "hetid_error")
          expect_identical(err$arg, "term_premia")
          message <- if (inherits(err, "condition")) conditionMessage(err) else ""
          expect_match(message, column, fixed = TRUE)
        }
      }
    }
  }
})

test_that("numeric term premia preserve values, missingness and output shape", {
  withr::local_options(warn = 2)
  f <- term_premia_boundary_fixture()
  f$tp$unused <- rep("unused", 6)
  f$tp$tp24[2] <- NA_real_
  f$tp$tp36[5] <- Inf
  expected <- (2 * f$yields$y24 - 3 * f$yields$y36 +
    3 * f$tp$tp36 - 2 * f$tp$tp24) / 100
  for (tp in list(f$tp, as.matrix(f$tp[, 1:3]))) {
    result <- withVisible(compute_n_hat(f$yields, tp, 24, f$dates))
    expect_true(result$visible)
    expect_identical(result$value$date, f$dates)
    expect_named(result$value, c("date", "n_hat"))
    expect_equal(dim(result$value), c(6, 2))
    expect_type(result$value$n_hat, "double")
    expect_equal(result$value$n_hat, expected)
  }
  realized <- exp(-f$yields$y12 / 100)
  exp_n <- exp(expected)
  for (paired in c(FALSE, TRUE)) {
    aligned <- if (paired) c(realized[3:6], NA_real_, NA_real_) else realized
    common <- is.finite(aligned) & is.finite(exp_n)
    correction <- if (paired) {
      mean(aligned[common] - exp_n[common])
    } else {
      mean(aligned[common]) - mean(exp_n[common])
    }
    result <- compute_expected_sdf(f$yields, f$tp, 24, f$dates, paired = paired)
    expect_named(result, c("date", "expected_sdf"))
    expect_identical(result$date, f$dates)
    expect_equal(result$expected_sdf, exp_n + correction)
  }
})

test_that("one-period normalization and horizon-zero bypass remain intact", {
  f <- term_premia_boundary_fixture()
  f$tp$tp12 <- c(NA_real_, Inf, NaN, -Inf, 9, 10)
  expected <- (f$yields$y12 - 2 * f$yields$y24 + 2 * f$tp$tp24) / 100
  expect_equal(compute_n_hat(f$yields, f$tp, 12, f$dates)$n_hat, expected)
  for (paired in c(FALSE, TRUE)) {
    result <- compute_expected_sdf(f$yields, f$tp, 12, f$dates, paired = paired)
    zero_tp <- f$tp
    zero_tp$tp12 <- 0
    expect_identical(
      result,
      compute_expected_sdf(f$yields, zero_tp, 12, f$dates, paired = paired)
    )
    expect_warning(
      boundary <- compute_expected_sdf(
        f$yields, data.frame(unused = rep("unused", 6)), 0, f$dates,
        paired = paired
      ),
      class = "hetid_warning_horizon_zero"
    )
    expect_named(boundary, c("date", "expected_sdf"))
    expect_identical(boundary$date, f$dates)
    expect_equal(boundary$expected_sdf, exp(-f$yields$y12 / 100))
  }
})
