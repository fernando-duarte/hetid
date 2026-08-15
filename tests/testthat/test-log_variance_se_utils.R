# Tests for the estimator-agnostic SE scaffolding: the normalized fail-closed
# inverse, the Bartlett/Newey-West meat, and the preflight that either hands
# back mu or the all-NA skeleton.

test_that("se_norm_inv equals solve() on a well-conditioned matrix", {
  set.seed(3)
  a <- matrix(stats::rnorm(25), 5, 5)
  m <- crossprod(a) + diag(5)
  expect_equal(
    hetid:::se_norm_inv(m, LOG_VARIANCE_CONTROL$RCOND_TOLERANCE),
    solve(m),
    tolerance = 1e-12
  )
})

test_that("se_norm_inv gates on the normalized, not the raw, condition", {
  # a column rescaling drives rcond(m) to ~9e-13 while leaving the correlation
  # matrix perfectly conditioned; the SE must still be available
  m0 <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
  d <- c(1, 1e6)
  m <- diag(d) %*% m0 %*% diag(d)
  expect_lt(rcond(m), LOG_VARIANCE_CONTROL$RCOND_TOLERANCE)
  expect_equal(
    hetid:::se_norm_inv(m, LOG_VARIANCE_CONTROL$RCOND_TOLERANCE),
    solve(m),
    tolerance = 1e-12
  )
})

test_that("se_norm_inv returns NULL on unusable matrices", {
  tol <- LOG_VARIANCE_CONTROL$RCOND_TOLERANCE
  singular <- matrix(c(1, 1, 1, 1), 2, 2)
  expect_null(hetid:::se_norm_inv(singular, tol))

  nonfinite <- matrix(c(1, NA, NA, 1), 2, 2)
  expect_null(hetid:::se_norm_inv(nonfinite, tol))

  # normalized rcond ~ (1 - rho) / (1 + rho) = 5e-13, below the tolerance,
  # yet still positive definite: this is the rcond gate firing, not chol
  ill <- matrix(c(1, 1 - 1e-12, 1 - 1e-12, 1), 2, 2)
  expect_lt(rcond(ill), tol)
  expect_null(hetid:::se_norm_inv(ill, tol))

  zero_diag <- matrix(c(0, 0.5, 0.5, 1), 2, 2)
  expect_null(hetid:::se_norm_inv(zero_diag, tol))
})

test_that("se_bartlett_meat matches a hand-computed fixture", {
  scores <- matrix(c(1, 3, 5, 2, 4, 6), 3, 2)
  # crossprod(scores) = [[35, 44], [44, 56]]; gamma_1 = s2 s1' + s3 s2' =
  # [[18, 26], [22, 32]]; weight 1 - 1/2 = 0.5 on gamma_1 + t(gamma_1)
  expected <- matrix(c(53, 68, 68, 88), 2, 2)
  expect_equal(hetid:::se_bartlett_meat(scores, 1L), expected)
})

test_that("se_bartlett_meat matches the loop oracle and degenerates at 0", {
  set.seed(4)
  scores <- matrix(stats::rnorm(120), 40, 3)
  lags <- LOG_VARIANCE_CONTROL$HAC_LAGS
  meat <- crossprod(scores)
  for (l in seq_len(lags)) {
    gam <- crossprod(
      scores[-seq_len(l), , drop = FALSE],
      scores[seq_len(nrow(scores) - l), , drop = FALSE]
    )
    meat <- meat + (1 - l / (lags + 1)) * (gam + t(gam))
  }
  expect_equal(hetid:::se_bartlett_meat(scores, lags), meat, tolerance = 1e-12)
  expect_equal(
    hetid:::se_bartlett_meat(scores, 0L), crossprod(scores),
    tolerance = 1e-12
  )
})

test_that("se_preflight returns mu on ok inputs and the skeleton on bad ones", {
  d <- simulate_logvar_data(t_obs = 60)
  x_mat <- hetid:::log_variance_design(d$x)
  coef <- c(-0.5, 0.6, -0.4)
  se_types <- LOG_VARIANCE_CONTROL$SE_TYPES

  ok <- hetid:::se_preflight(coef, d$y, x_mat, 4L, se_types)
  expect_true(ok$ok)
  expect_equal(ok$mu, exp(drop(x_mat %*% coef)), tolerance = 1e-12)
  expect_identical(ok$n, nrow(x_mat))
  expect_identical(ok$p, ncol(x_mat))

  labels <- colnames(x_mat)
  for (bad_coef in list(NULL, c(NA_real_, 0.6, -0.4), c(0.1, 0.2))) {
    bad <- hetid:::se_preflight(bad_coef, d$y, x_mat, 4L, se_types)
    expect_false(bad$ok)
    expect_null(bad$mu)
    expect_identical(names(bad$na_out), se_types)
    expect_identical(dimnames(bad$na_out$hac), list(labels, labels))
    expect_true(all(is.na(bad$na_out$naive)))
  }

  # n <= p is a data-quality failure, not an error
  short <- hetid:::se_preflight(
    coef, d$y[1:2], x_mat[1:2, , drop = FALSE], 4L, se_types
  )
  expect_false(short$ok)
})
