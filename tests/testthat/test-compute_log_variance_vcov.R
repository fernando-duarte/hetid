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

# Paper-equivalence pin. The coefficient vector and all four covariance
# matrices come from the package chain and were verified equal (tolerance
# 1e-10) to the paper pipeline's logvar_ppml_fit_response / logvar_ppml_vcov
# against scripts-paper at HEAD b34044ac67ee38e133e264b51da386a05a951b7e on
# 2026-08-15, by the procedure in docs/verification/tau0_port_equivalence.R
# (local, git-ignored; re-run it to re-verify). Full matrices, dimnames
# included: a diagonal-only pin would let off-diagonal drift pass. The four
# paper files that own the fit and the SEs are
# log_variance/inference/standard_error_estimators.R and
# log_variance/estimators/ppml/{standard_errors,fit,acceptance}.R, with sha256:
#   standard_error_estimators.R: d1c71657f788ed9fc6cb1f7fe50e308fb1d427450c3cbe90de857f77917d2ffb
#   standard_errors.R: cd9860e0e6b5da2a10cfc8061a9823c694927968d92c8b39d15758d986d4f9f4
#   fit.R: 732653a3f8bda50d0acde5def18b1243733e083b1269e9b2ae82b091c8b75a0f
#   acceptance.R: 45cc27baf799c2d84a5f4d936e2290743f36bfa1867adb1150519ca46eca63b9
# The test itself never sources the paper pipeline.
test_that("pinned paper-equivalence fixture: ppml coef and vcov at the default seed", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x)
  expect_equal(
    fit$coef,
    c(
      `(Intercept)` = -0.61561017044240696, v1 = 0.48460472345722871,
      v2 = -0.3712531413455768
    ),
    tolerance = 1e-8
  )

  labels <- c("(Intercept)", "v1", "v2")
  pin <- function(values) matrix(values, 3L, 3L, dimnames = list(labels, labels))
  expected <- list(
    naive = pin(c(
      0.010767168765921939, -0.0045831187439318464, 0.0031047514674835833,
      -0.0045831187439318464, 0.0086374066254240513, -0.0017568523917700076,
      0.0031047514674835833, -0.0017568523917700076, 0.0079015730685633757
    )),
    hc0 = pin(c(
      0.0082823210839947127, -0.0013093636354273904, -0.00035709107200380778,
      -0.0013093636354273902, 0.0086116252233974806, 0.00013838943226929435,
      -0.0003570910720038074, 0.00013838943226929481, 0.0076979223799097107
    )),
    hc1 = pin(c(
      0.0083659808929239521, -0.0013225895307347379, -0.00036069805252909881,
      -0.0013225895307347377, 0.0086986113367651316, 0.00013978730532251957,
      -0.00036069805252909843, 0.00013978730532252003, 0.007775679171625971
    )),
    hac = pin(c(
      0.0089817594906886049, -0.0019101077882143428, 0.00052633637268013596,
      -0.0019101077882143422, 0.0098973419028301809, 0.0011228668930996254,
      0.00052633637268013564, 0.0011228668930996254, 0.0087560536498852704
    ))
  )

  vc <- compute_log_variance_vcov(fit, hac_lags = 4L)
  expect_identical(names(vc), names(expected))
  for (variant in names(expected)) {
    expect_equal(vc[[variant]], expected[[variant]], tolerance = 1e-8)
  }
})
