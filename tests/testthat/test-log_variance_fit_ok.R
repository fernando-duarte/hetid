test_that("fit success predicates accept raw lists and retain the existing truth table", {
  fits <- list(
    NULL, 1, list(), list(status = "ok", converged = TRUE, coef = 1),
    list(fit_status = "ok", converged = TRUE, coef = c(0, 1)),
    list(fit_status = "ok", converged = FALSE, coef = 1),
    list(fit_status = "domain_failure", converged = TRUE, coef = 1),
    list(fit_status = "ok", converged = TRUE, coef = NULL),
    list(fit_status = "ok", converged = TRUE, coef = NA_real_),
    list(fit_status = "ok", converged = TRUE, coef = Inf),
    list(fit_status = "ok", converged = TRUE, coef = numeric(0))
  )
  expect_identical(
    vapply(fits, log_variance_fit_ok, logical(1)),
    c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
  for (converged in list(NULL, NA, 1, c(TRUE, TRUE))) {
    expect_false(log_variance_fit_ok(list(fit_status = "ok", converged = converged, coef = 1)))
  }
  expect_false(log_variance_fit_ok(list(
    fit_status = c("ok", "ok"),
    converged = TRUE, coef = 1
  )))
  expect_true(log_variance_fit_ok(list(
    fit_status = "ok", converged = TRUE,
    coef = matrix(1, 1, 1)
  )))
  expect_error(log_variance_fit_ok(list(
    fit_status = "ok", converged = TRUE,
    coef = list(1)
  )))
})

test_that("fit success predicates also accept package fit containers", {
  x <- matrix(seq(-1, 1, length.out = 20), ncol = 1)
  fit <- fit_log_variance(exp(0.2 + 0.3 * x[, 1]), x, "harvey")
  expect_s3_class(fit, "hetid_log_variance_fit")
  expect_true(log_variance_fit_ok(fit))
  expect_false(log_variance_fit_ok(fit_log_variance(rep(0, 20), x, "harvey")))
})
