# Tests for harvey_vcov_variants(): the five analytic QMLE covariance variants
# of the Harvey Gaussian multiplicative-heteroskedasticity log-variance fit,
# each pinned to a manual solve()-path oracle at a fixed coefficient. Every
# variant is a pure function of the coefficient, the response, and the design,
# so these tests never construct a Harvey fit.

# fixture inputs: the shared log-variance DGP, its design, and the coefficient
# the DGP was drawn from
harvey_fixture <- function() {
  d <- simulate_logvar_data()
  list(y = d$y, x_mat = hetid:::log_variance_design(d$x), coef = c(-0.5, 0.6, -0.4))
}

# the oracle pieces every variant is assembled from, at the fixture coefficient
harvey_oracle <- function(f) {
  mu <- drop(exp(f$x_mat %*% f$coef))
  r <- f$y / mu
  g <- 0.5 * (1 - r) * f$x_mat
  list(
    g = g,
    h_inv = solve(0.5 * crossprod(f$x_mat, r * f$x_mat)),
    ex_inv = solve(0.5 * crossprod(f$x_mat)),
    meat_opg = crossprod(g)
  )
}

test_that("expected, observed, and opg match their manual solve() oracles", {
  f <- harvey_fixture()
  o <- harvey_oracle(f)
  vc <- hetid:::harvey_vcov_variants(f$coef, f$y, f$x_mat, LOG_VARIANCE_CONTROL$HAC_LAGS)
  expect_equal(unname(vc$expected), unname(o$ex_inv), tolerance = 1e-10)
  expect_equal(unname(vc$observed), unname(o$h_inv), tolerance = 1e-10)
  expect_equal(unname(vc$opg), unname(solve(o$meat_opg)), tolerance = 1e-10)
  # the two informations are distinct objects: the expected bread ignores the
  # response, the observed one weights the design by r
  expect_false(isTRUE(all.equal(vc$expected, vc$observed)))
})

test_that("robust and hac are the sandwiches built on the observed bread", {
  f <- harvey_fixture()
  o <- harvey_oracle(f)
  lags <- LOG_VARIANCE_CONTROL$HAC_LAGS
  vc <- hetid:::harvey_vcov_variants(f$coef, f$y, f$x_mat, lags)
  expect_equal(
    unname(vc$robust), unname(o$h_inv %*% o$meat_opg %*% o$h_inv),
    tolerance = 1e-10
  )
  meat <- o$meat_opg
  n <- nrow(o$g)
  for (l in seq_len(lags)) {
    gam <- crossprod(
      o$g[-seq_len(l), , drop = FALSE], o$g[seq_len(n - l), , drop = FALSE]
    )
    meat <- meat + (1 - l / (lags + 1)) * (gam + t(gam))
  }
  expect_equal(
    unname(vc$hac), unname(o$h_inv %*% meat %*% o$h_inv),
    tolerance = 1e-10
  )
})

test_that("hac collapses to robust at zero lags", {
  f <- harvey_fixture()
  vc <- hetid:::harvey_vcov_variants(f$coef, f$y, f$x_mat, 0L)
  expect_equal(vc$hac, vc$robust, tolerance = 1e-12)
  # and a positive truncation genuinely moves it
  lagged <- hetid:::harvey_vcov_variants(f$coef, f$y, f$x_mat, 4L)
  expect_false(isTRUE(all.equal(lagged$hac, lagged$robust)))
})

test_that("the variant list is keyed and labelled off the design", {
  f <- harvey_fixture()
  labels <- colnames(f$x_mat)
  vc <- hetid:::harvey_vcov_variants(f$coef, f$y, f$x_mat, 4L)
  expect_identical(names(vc), LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES)
  for (v in vc) {
    expect_identical(dim(v), c(length(labels), length(labels)))
    expect_identical(dimnames(v), list(labels, labels))
    expect_true(all(is.finite(v)))
  }
})

test_that("the variants hold away from the coefficient the data came from", {
  f <- harvey_fixture()
  f$coef <- c(0.2, -0.3, 0.9)
  o <- harvey_oracle(f)
  vc <- hetid:::harvey_vcov_variants(f$coef, f$y, f$x_mat, 4L)
  expect_equal(unname(vc$expected), unname(o$ex_inv), tolerance = 1e-10)
  expect_equal(unname(vc$observed), unname(o$h_inv), tolerance = 1e-10)
  expect_equal(unname(vc$opg), unname(solve(o$meat_opg)), tolerance = 1e-10)
  expect_equal(
    unname(vc$robust), unname(o$h_inv %*% o$meat_opg %*% o$h_inv),
    tolerance = 1e-10
  )
})

# every fail-closed path returns the same skeleton: five all-NA matrices keyed
# and labelled exactly as a successful call would be
expect_harvey_na <- function(vc, labels) {
  expect_identical(names(vc), LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES)
  for (v in vc) {
    expect_true(all(is.na(v)))
    expect_identical(dimnames(v), list(labels, labels))
  }
}

test_that("a design with no residual degrees of freedom fails closed", {
  f <- harvey_fixture()
  labels <- colnames(f$x_mat)
  short <- hetid:::harvey_vcov_variants(
    f$coef, f$y[1:3], f$x_mat[1:3, , drop = FALSE], 4L
  )
  expect_harvey_na(short, labels)
})

test_that("a missing, malformed, or explosive coefficient fails closed", {
  f <- harvey_fixture()
  labels <- colnames(f$x_mat)
  bad_coefs <- list(
    NULL, c(NA_real_, 0.6, -0.4), c(-0.5, Inf, -0.4), c(-0.5, 0.6),
    # a coefficient this large overflows mu, which the preflight rejects
    c(1e5, 0.6, -0.4)
  )
  for (bad in bad_coefs) {
    expect_harvey_na(
      hetid:::harvey_vcov_variants(bad, f$y, f$x_mat, 4L), labels
    )
  }
})

test_that("a negative or non-finite response fails closed", {
  f <- harvey_fixture()
  labels <- colnames(f$x_mat)
  y_neg <- f$y
  y_neg[10] <- -1
  expect_harvey_na(
    hetid:::harvey_vcov_variants(f$coef, y_neg, f$x_mat, 4L), labels
  )
  y_na <- f$y
  y_na[10] <- NA_real_
  expect_harvey_na(
    hetid:::harvey_vcov_variants(f$coef, y_na, f$x_mat, 4L), labels
  )
})

test_that("a rank-deficient design fails every variant closed", {
  f <- harvey_fixture()
  x_dup <- cbind(f$x_mat, v2_copy = f$x_mat[, "v2"])
  vc <- hetid:::harvey_vcov_variants(c(f$coef, 0), f$y, x_dup, 4L)
  # the duplicated column is singular in all three breads, so the two
  # sandwiches lose their bread as well
  expect_harvey_na(vc, colnames(x_dup))
})

test_that("a zero response row is zero-safe", {
  f <- harvey_fixture()
  y0 <- f$y
  y0[5] <- 0
  vc <- hetid:::harvey_vcov_variants(f$coef, y0, f$x_mat, 4L)
  for (v in vc) {
    expect_true(all(is.finite(v)))
  }
  base <- hetid:::harvey_vcov_variants(f$coef, f$y, f$x_mat, 4L)
  # the zero row moves the score and the observed bread; the expected
  # information never sees the response
  expect_equal(vc$expected, base$expected, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(vc$observed, base$observed)))
  expect_false(isTRUE(all.equal(vc$opg, base$opg)))
})
