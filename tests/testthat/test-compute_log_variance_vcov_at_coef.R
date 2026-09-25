test_that("supplied-coefficient matrices match independent full-matrix oracles", {
  d <- covariance_fixture()
  for (est in c("ppml", "harvey")) {
    for (lag in c(0L, 2L, 9L, .Machine$integer.max)) {
      expect_warning(
        v <- compute_log_variance_vcov_at_coef(d$coef, d$y, d$x, est, lag),
        NA
      )
      expected <- covariance_oracle(d, est, as.double(lag))
      expect_identical(names(v), names(expected))
      expect_equal(v, expected, tolerance = 1e-12)
    }
  }
})

test_that("existing fit API and new entrypoint share matrices at accepted fits", {
  d <- simulate_logvar_data()
  for (est in c("ppml", "harvey")) {
    fit <- fit_log_variance(d$y, d$x, estimator = est)
    expect_true(log_variance_fit_ok(fit))
    expect_identical(
      compute_log_variance_vcov_at_coef(fit$coef, fit$y, fit$x_design, est),
      compute_log_variance_vcov(fit)
    )
    failed <- fit_log_variance(rep(0, length(d$y)), d$x, estimator = est)
    expect_true(all(vapply(
      compute_log_variance_vcov(failed),
      function(m) all(is.na(m)), logical(1)
    )))
  }
})

test_that("supplied conditioning controls affect both estimator families", {
  d <- covariance_fixture()
  for (est in c("ppml", "harvey")) {
    loose <- compute_log_variance_vcov_at_coef(d$coef, d$y, d$x, est)
    tight <- compute_log_variance_vcov_at_coef(d$coef, d$y, d$x, est, rcond_tol = 1)
    expect_true(all(vapply(loose, function(m) all(is.finite(m)), logical(1))))
    expect_true(all(vapply(tight, function(m) all(is.na(m)), logical(1))))
    expect_identical(
      compute_log_variance_vcov_at_coef(d$coef, d$y, d$x, est, rcond_tol = 1e-10),
      loose
    )
  }
})

test_that("response and column rescaling preserve the covariance transformation", {
  d <- covariance_fixture()
  for (est in c("ppml", "harvey")) {
    base <- compute_log_variance_vcov_at_coef(d$coef, d$y, d$x, est)
    for (s in c(1e-6, 1e6)) {
      beta <- d$coef + c(log(s), 0)
      scaled <- compute_log_variance_vcov_at_coef(beta, d$y * s, d$x, est)
      expect_equal(scaled, base, tolerance = 1e-10)
    }
    scale <- c(1, 1e6)
    scaled <- compute_log_variance_vcov_at_coef(
      d$coef / scale, d$y, sweep(d$x, 2, scale, "*"), est
    )
    for (key in names(base)) {
      expect_equal(scaled[[key]] * tcrossprod(scale), base[[key]], tolerance = 1e-10)
    }
  }
})

test_that("column identity is explicit and chronological order matters for HAC", {
  d <- covariance_fixture()
  perm <- c(2, 1)
  row_perm <- c(1, 3, 5, 2, 4, 6)
  for (est in c("ppml", "harvey")) {
    base <- compute_log_variance_vcov_at_coef(d$coef, d$y, d$x, est, 2L)
    named <- stats::setNames(d$coef, colnames(d$x))
    reordered <- compute_log_variance_vcov_at_coef(
      named[perm], d$y, d$x[, perm], est, 2L
    )
    for (key in names(base)) expect_equal(reordered[[key]], base[[key]][perm, perm])
    shuffled <- compute_log_variance_vcov_at_coef(
      d$coef, d$y[row_perm], d$x[row_perm, ], est, 2L
    )
    other <- setdiff(names(base), "hac")
    expect_equal(shuffled[other], base[other], tolerance = 1e-12)
    expect_false(isTRUE(all.equal(shuffled$hac, base$hac)))
  }
})

test_that("a singular Harvey OPG preserves other available variants", {
  d <- covariance_fixture()
  d$y <- exp(drop(d$x %*% d$coef))
  v <- compute_log_variance_vcov_at_coef(d$coef, d$y, d$x, "harvey")
  expect_true(all(is.na(v$opg)))
  expect_true(all(is.finite(v$expected)))
  expect_true(all(is.finite(v$observed)))
  expect_equal(unname(v$robust), matrix(0, 2, 2))
  expect_equal(v$hac, v$robust)
})
