# Tests for the hetid_tau0_fit container: constructor, validator, methods

make_tau0_fixture <- function(n_obs = 60, i_dim = 2, j_dim = 1, seed = 42) {
  set.seed(seed)
  beta1r <- stats::setNames(rnorm(3), c("(Intercept)", "x1", "x2"))
  beta2r <- matrix(rnorm(i_dim * 3),
    nrow = i_dim, ncol = 3,
    dimnames = list(paste0("news", seq_len(i_dim)), names(beta1r))
  )
  w1 <- rnorm(n_obs)
  w2 <- matrix(rnorm(n_obs * i_dim), nrow = n_obs, ncol = i_dim)
  z <- matrix(rnorm(n_obs * j_dim), nrow = n_obs, ncol = j_dim)
  gamma_mat <- matrix(1, nrow = j_dim, ncol = i_dim)
  moments <- compute_identification_moments(w1, w2, z)
  theta <- rnorm(i_dim)
  point <- list(theta = theta, cond = 12.5)
  beta1 <- recover_structural_coefficients(beta1r, beta2r, theta)
  list(
    beta1r = beta1r, beta2r = beta2r, w1 = w1, w2 = w2, z = z,
    gamma = gamma_mat, moments = moments, point = point, beta1 = beta1,
    n_obs = n_obs, impose_null = FALSE, tol = 1e-8
  )
}

build_fit <- function(f) {
  new_hetid_tau0_fit(
    beta1r = f$beta1r, beta2r = f$beta2r, w1 = f$w1, w2 = f$w2, z = f$z,
    gamma = f$gamma, moments = f$moments, point = f$point, beta1 = f$beta1,
    n_obs = f$n_obs, impose_null = f$impose_null, tol = f$tol
  )
}

test_that("constructor preserves fields and stamps attributes", {
  f <- make_tau0_fixture()
  fit <- build_fit(f)

  expect_s3_class(fit, "hetid_tau0_fit")
  expect_identical(fit$beta1r, f$beta1r)
  expect_identical(fit$beta2r, f$beta2r)
  expect_identical(fit$w1, f$w1)
  expect_identical(fit$w2, f$w2)
  expect_identical(fit$z, f$z)
  expect_identical(fit$gamma, f$gamma)
  expect_identical(fit$moments, f$moments)
  expect_identical(fit$point, f$point)
  expect_identical(fit$beta1, f$beta1)
  expect_identical(attr(fit, "n_obs"), 60L)
  expect_identical(attr(fit, "impose_null"), FALSE)
  expect_identical(attr(fit, "tol"), 1e-8)

  expect_invisible(validate_hetid_tau0_fit(fit))
  expect_identical(validate_hetid_tau0_fit(fit), fit)
})

test_that("constructor accepts a NULL point and NULL beta1", {
  f <- make_tau0_fixture()
  f$point <- NULL
  f["beta1"] <- list(NULL) # f$beta1<-NULL deletes the key; f$beta1 then partial-matches f$beta1r
  fit <- build_fit(f)
  expect_identical(validate_hetid_tau0_fit(fit), fit)
})

test_that("constructor coerces non-integer-valued n_obs to integer", {
  f <- make_tau0_fixture()
  f$n_obs <- 60
  fit <- build_fit(f)
  expect_identical(attr(fit, "n_obs"), 60L)
})

test_that("validator rejects a wrong w2 row count", {
  f <- make_tau0_fixture()
  f$w2 <- f$w2[-1, , drop = FALSE]
  fit <- build_fit(f)
  expect_error(
    validate_hetid_tau0_fit(fit),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("validator rejects a wrong w1 length", {
  f <- make_tau0_fixture()
  f$w1 <- f$w1[-1]
  fit <- build_fit(f)
  expect_error(
    validate_hetid_tau0_fit(fit),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("validator rejects a wrong gamma shape", {
  f <- make_tau0_fixture()
  f$gamma <- f$gamma[, 1, drop = FALSE]
  fit <- build_fit(f)
  expect_error(
    validate_hetid_tau0_fit(fit),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("validator rejects a non-hetid_moments moments object", {
  f <- make_tau0_fixture()
  f$moments <- unclass(f$moments)
  fit <- build_fit(f)
  expect_error(
    validate_hetid_tau0_fit(fit),
    "hetid_moments object",
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects beta1 present while point is NULL", {
  f <- make_tau0_fixture()
  f$point <- NULL
  fit <- build_fit(f)
  expect_error(
    validate_hetid_tau0_fit(fit),
    "beta1",
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects beta1 absent while point is non-NULL", {
  f <- make_tau0_fixture()
  f["beta1"] <- list(NULL) # f$beta1<-NULL deletes the key; f$beta1 then partial-matches f$beta1r
  fit <- build_fit(f)
  expect_error(
    validate_hetid_tau0_fit(fit),
    "beta1",
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects names(beta1r) vs colnames(beta2r) disagreement", {
  f <- make_tau0_fixture()
  colnames(f$beta2r) <- c("a", "b", "c")
  fit <- build_fit(f)
  expect_error(
    validate_hetid_tau0_fit(fit),
    "colnames\\(beta2r\\)",
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects a malformed point list", {
  f <- make_tau0_fixture()
  f$point <- list(theta = f$point$theta[-1], cond = f$point$cond)
  fit <- build_fit(f)
  expect_error(
    validate_hetid_tau0_fit(fit),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("assert_hetid_tau0_fit rejects plain lists", {
  f <- make_tau0_fixture()
  fit <- build_fit(f)
  expect_error(
    assert_hetid_tau0_fit(unclass(fit)),
    "hetid_tau0_fit object",
    class = "hetid_error_bad_argument"
  )
  expect_true(assert_hetid_tau0_fit(fit))
})

test_that("print shows the header, n_obs, impose_null, and the point line", {
  f <- make_tau0_fixture()
  fit <- build_fit(f)
  printed <- capture.output(print(fit))
  expect_true(any(grepl("<hetid_tau0_fit>", printed, fixed = TRUE)))
  expect_true(any(grepl("n_obs", printed, fixed = TRUE)))
  expect_true(any(grepl("impose_null", printed, fixed = TRUE)))
  expect_true(any(grepl("theta", printed, fixed = TRUE)))
  expect_true(any(grepl("cond", printed, fixed = TRUE)))
})

test_that("print shows the generic no-point message, not a specific cause", {
  f <- make_tau0_fixture()
  f$point <- NULL
  f["beta1"] <- list(NULL) # f$beta1<-NULL deletes the key; f$beta1 then partial-matches f$beta1r
  fit <- build_fit(f)
  expect_output(print(fit), "no tau=0 point", fixed = TRUE)
  printed <- capture.output(print(fit))
  expect_false(any(grepl("rank", printed, ignore.case = TRUE)))
})

test_that("print returns its argument invisibly", {
  f <- make_tau0_fixture()
  fit <- build_fit(f)
  expect_invisible(print(fit))
  expect_identical(withVisible(print(fit))$value, fit)
})
