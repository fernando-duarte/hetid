# Fixture: simulate a common-sample triangular reduced form and return the
# OLS reduced-form coefficient objects plus the raw data, so the affine
# identity can be checked against an independent direct regression.
make_structural_fixture <- function(n = 200, i_dim = 2, n_pcs = 2) {
  x <- cbind(1, matrix(stats::rnorm(n * n_pcs), n, n_pcs))
  colnames(x) <- c("(Intercept)", paste0("pc", seq_len(n_pcs)))
  y1 <- as.numeric(x %*% c(0.4, 0.2, -0.1) + stats::rnorm(n))
  y2 <- x %*% matrix(stats::rnorm((n_pcs + 1) * i_dim), n_pcs + 1, i_dim) +
    matrix(stats::rnorm(n * i_dim), n, i_dim)
  beta1r <- stats::setNames(coef(stats::lm(y1 ~ x - 1)), colnames(x))
  beta2r <- t(vapply(
    seq_len(i_dim),
    function(i) coef(stats::lm(y2[, i] ~ x - 1)),
    numeric(n_pcs + 1)
  ))
  rownames(beta2r) <- paste0("maturity_", seq_len(i_dim))
  colnames(beta2r) <- colnames(x)
  list(x = x, y1 = y1, y2 = y2, beta1r = beta1r, beta2r = beta2r)
}

test_that("affine identity matches a direct OLS on a common sample", {
  set.seed(101)
  fx <- make_structural_fixture()
  theta <- c(0.3, -0.4)
  direct <- coef(stats::lm((fx$y1 - fx$y2 %*% theta) ~ fx$x - 1))
  recovered <- recover_structural_coefficients(fx$beta1r, fx$beta2r, theta)
  expect_equal(unname(recovered), unname(direct))
})

test_that("theta = 0 returns beta1r exactly with names", {
  set.seed(102)
  fx <- make_structural_fixture()
  out <- recover_structural_coefficients(fx$beta1r, fx$beta2r, c(0, 0))
  expect_equal(out, fx$beta1r)
  expect_identical(names(out), names(fx$beta1r))
})

test_that("the result equals the closed-form beta1r - t(beta2r) theta", {
  set.seed(103)
  fx <- make_structural_fixture()
  theta <- c(-0.7, 1.2)
  expect_equal(
    unname(recover_structural_coefficients(fx$beta1r, fx$beta2r, theta)),
    as.numeric(fx$beta1r - t(fx$beta2r) %*% theta)
  )
})

test_that("vector theta and a one-column matrix theta agree on values", {
  set.seed(104)
  fx <- make_structural_fixture()
  theta <- c(0.2, 0.5)
  vec_out <- recover_structural_coefficients(fx$beta1r, fx$beta2r, theta)
  mat_out <- recover_structural_coefficients(
    fx$beta1r, fx$beta2r, matrix(theta, ncol = 1)
  )
  expect_null(dim(vec_out))
  expect_true(is.matrix(mat_out))
  expect_equal(as.numeric(mat_out), unname(vec_out))
})

test_that("matrix theta evaluates each column independently", {
  set.seed(105)
  fx <- make_structural_fixture()
  theta_set <- cbind(a = c(0, 0), b = c(0.3, -0.4), c = c(1, 1))
  out <- recover_structural_coefficients(fx$beta1r, fx$beta2r, theta_set)
  expect_identical(dim(out), c(length(fx$beta1r), ncol(theta_set)))
  for (k in seq_len(ncol(theta_set))) {
    single <- recover_structural_coefficients(
      fx$beta1r, fx$beta2r, theta_set[, k]
    )
    expect_equal(unname(out[, k]), unname(single))
  }
})

test_that("names propagate to vector and matrix outputs", {
  set.seed(106)
  fx <- make_structural_fixture()
  vec_out <- recover_structural_coefficients(fx$beta1r, fx$beta2r, c(0.1, 0.2))
  expect_identical(names(vec_out), names(fx$beta1r))
  theta_set <- cbind(s1 = c(0.1, 0.2), s2 = c(0.3, 0.4))
  mat_out <- recover_structural_coefficients(fx$beta1r, fx$beta2r, theta_set)
  expect_identical(rownames(mat_out), names(fx$beta1r))
  expect_identical(colnames(mat_out), colnames(theta_set))
})

test_that("a predictor-axis dimension mismatch is signalled", {
  set.seed(107)
  fx <- make_structural_fixture()
  err <- expect_error(
    recover_structural_coefficients(
      fx$beta1r[-1], fx$beta2r, c(0.1, 0.2)
    ),
    class = "hetid_error_dimension_mismatch"
  )
  expect_match(conditionMessage(err), "predictor axis", fixed = TRUE)
})

test_that("a vector-theta component-axis mismatch is signalled", {
  set.seed(108)
  fx <- make_structural_fixture()
  expect_error(
    recover_structural_coefficients(fx$beta1r, fx$beta2r, c(0.1, 0.2, 0.3)),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("a matrix-theta component-axis mismatch is signalled", {
  set.seed(109)
  fx <- make_structural_fixture()
  expect_error(
    recover_structural_coefficients(
      fx$beta1r, fx$beta2r, matrix(0, nrow = 3, ncol = 2)
    ),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("non-numeric theta is rejected", {
  set.seed(110)
  fx <- make_structural_fixture()
  expect_error(
    recover_structural_coefficients(fx$beta1r, fx$beta2r, c("a", "b")),
    class = "hetid_error_bad_argument"
  )
})

test_that("non-finite content is rejected", {
  set.seed(111)
  fx <- make_structural_fixture()
  expect_error(
    recover_structural_coefficients(fx$beta1r, fx$beta2r, c(NA_real_, 0.2)),
    class = "hetid_error_bad_argument"
  )
  bad_beta2 <- fx$beta2r
  bad_beta2[1, 1] <- Inf
  expect_error(
    recover_structural_coefficients(fx$beta1r, bad_beta2, c(0.1, 0.2)),
    class = "hetid_error_bad_argument"
  )
})

test_that("beta2r must be a matrix and beta1r must be a vector", {
  set.seed(112)
  fx <- make_structural_fixture()
  expect_error(
    recover_structural_coefficients(fx$beta1r, as.numeric(fx$beta2r), c(0.1, 0.2)),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    recover_structural_coefficients(fx$beta2r, fx$beta2r, c(0.1, 0.2)),
    class = "hetid_error_bad_argument"
  )
})

test_that("mismatched predictor names are signalled", {
  set.seed(113)
  fx <- make_structural_fixture()
  bad_beta2 <- fx$beta2r
  colnames(bad_beta2) <- c("(Intercept)", "pcX", "pcY")
  err <- expect_error(
    recover_structural_coefficients(fx$beta1r, bad_beta2, c(0.1, 0.2)),
    class = "hetid_error_bad_argument"
  )
  expect_match(conditionMessage(err), "must match", fixed = TRUE)
})
