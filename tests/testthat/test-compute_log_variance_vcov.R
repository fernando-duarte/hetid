# Tests for compute_log_variance_vcov(): the four QMLE covariance variants
# read off a hetid_log_variance_fit, each pinned to a manual oracle computed
# at the fit's own coefficient.

test_that("naive vcov matches the manual dispersion oracle at fit$coef", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x)
  vc <- compute_log_variance_vcov(fit)
  # manual naive oracle AT fit$coef via the solve() path — a glm refit would
  # converge to slightly different coefficients and test the wrong point
  x_mat <- cbind(1, d$x)
  mu <- drop(exp(x_mat %*% fit$coef))
  r <- d$y - mu
  phi <- sum(r^2 / mu) / (length(d$y) - ncol(x_mat))
  naive <- phi * solve(crossprod(x_mat, x_mat * mu))
  expect_equal(unname(vc$naive), unname(naive), tolerance = 1e-10)
})

test_that("hc0/hc1/hac reduce to their manual oracles", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x)
  x_mat <- cbind(1, d$x)
  mu <- drop(exp(x_mat %*% fit$coef))
  r <- d$y - mu
  a_inv <- solve(crossprod(x_mat, x_mat * mu)) # different path than chol
  hc0 <- a_inv %*% crossprod(x_mat * r) %*% a_inv
  vc <- compute_log_variance_vcov(fit)
  expect_equal(unname(vc$hc0), unname(hc0), tolerance = 1e-10)
  n <- length(d$y)
  p <- ncol(x_mat)
  expect_equal(vc$hc1, vc$hc0 * n / (n - p), tolerance = 1e-12)
  expect_equal(unname(compute_log_variance_vcov(fit,
    hac_lags = 0L
  )$hac), unname(hc0), tolerance = 1e-12)
  scores <- x_mat * r
  lags <- LOG_VARIANCE_CONTROL$HAC_LAGS
  meat <- crossprod(scores)
  for (l in seq_len(lags)) {
    w <- 1 - l / (lags + 1)
    gam <- crossprod(
      scores[-seq_len(l), , drop = FALSE],
      scores[seq_len(nrow(scores) - l), , drop = FALSE]
    )
    meat <- meat + w * (gam + t(gam))
  }
  expect_equal(unname(vc$hac), unname(a_inv %*% meat %*% a_inv),
    tolerance = 1e-10
  )
})

test_that("vcov fails closed to all-NA on a failed fit", {
  d <- simulate_logvar_data()
  failed <- fit_log_variance(rep(0, nrow(d$x)), d$x)
  bad <- compute_log_variance_vcov(failed)
  expect_true(all(is.na(bad$naive)))
  expect_true(all(is.na(bad$hac)))
})

test_that("malformed arguments error loudly", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x)
  expect_error(compute_log_variance_vcov(fit, hac_lags = -1L),
    class = "hetid_error_bad_argument"
  )
  expect_error(compute_log_variance_vcov(fit$coef),
    class = "hetid_error_bad_argument"
  )
})

test_that("the variant list is keyed and labelled off the fit", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x)
  vc <- compute_log_variance_vcov(fit)
  labels <- attr(fit, "coef_labels")
  expect_identical(names(vc), LOG_VARIANCE_CONTROL$SE_TYPES)
  for (v in vc) {
    expect_identical(dim(v), c(length(labels), length(labels)))
    expect_identical(dimnames(v), list(labels, labels))
  }
})
