# Tests for the hetid_log_variance_fit container: constructor, validator, methods

make_log_variance_fit_inputs <- function(status = "ok", n_obs = 60,
                                         seed = 42) {
  set.seed(seed)
  coef_labels <- c("(Intercept)", "x1", "x2")
  x_design <- cbind(
    "(Intercept)" = 1, x1 = rnorm(n_obs), x2 = rnorm(n_obs)
  )
  y <- exp(rnorm(n_obs))

  if (status == "ok") {
    coef_vals <- stats::setNames(c(0.1, 0.5, -0.2), coef_labels)
    warm_start <- stats::setNames(c(0.05, 0.4, -0.3), coef_labels)
    list(
      coef = coef_vals, fit_status = "ok", converged = TRUE, objective = 12.3,
      score_norm = 0.001, convergence_code = 4L, warm_start = warm_start,
      diagnostics = list(
        error_class = NA_character_,
        start_attempts = list(list(source = "supplied", error_class = NA_character_)),
        warnings = character(0), messages = character(0)
      ),
      y = y, x_design = x_design, estimator = "ppml", response_scale = 1,
      n_obs = n_obs, coef_labels = coef_labels
    )
  } else {
    list(
      coef = NULL, fit_status = "nonconvergence", converged = FALSE,
      objective = NA_real_, score_norm = NA_real_, convergence_code = -1L,
      warm_start = NULL,
      diagnostics = list(
        error_class = "rank_unresolved",
        start_attempts = list(list(source = "supplied", error_class = "invalid_start")),
        warnings = character(0), messages = character(0)
      ),
      y = y, x_design = x_design, estimator = "ppml", response_scale = 1,
      n_obs = n_obs, coef_labels = coef_labels
    )
  }
}

build_log_variance_fit <- function(f) {
  new_hetid_log_variance_fit(
    coef = f$coef, fit_status = f$fit_status, converged = f$converged,
    objective = f$objective, score_norm = f$score_norm,
    convergence_code = f$convergence_code, warm_start = f$warm_start,
    diagnostics = f$diagnostics, y = f$y, x_design = f$x_design,
    estimator = f$estimator, response_scale = f$response_scale,
    n_obs = f$n_obs, coef_labels = f$coef_labels
  )
}

test_that("constructor round-trips a success shape and stamps attributes", {
  f <- make_log_variance_fit_inputs("ok")
  fit <- build_log_variance_fit(f)

  expect_s3_class(fit, "hetid_log_variance_fit")
  expect_identical(fit$coef, f$coef)
  expect_identical(fit$fit_status, "ok")
  expect_true(fit$converged)
  expect_identical(fit$objective, f$objective)
  expect_identical(fit$score_norm, f$score_norm)
  expect_identical(fit$convergence_code, f$convergence_code)
  expect_identical(fit$warm_start, f$warm_start)
  expect_identical(fit$diagnostics, f$diagnostics)
  expect_identical(fit$y, f$y)
  expect_identical(fit$x_design, f$x_design)
  expect_identical(attr(fit, "estimator"), "ppml")
  expect_identical(attr(fit, "response_scale"), 1)
  expect_identical(attr(fit, "n_obs"), 60L)
  expect_identical(attr(fit, "coef_labels"), f$coef_labels)

  expect_invisible(validate_hetid_log_variance_fit(fit))
  expect_identical(validate_hetid_log_variance_fit(fit), fit)
})

test_that("constructor round-trips a failure shape", {
  f <- make_log_variance_fit_inputs("nonconvergence")
  fit <- build_log_variance_fit(f)

  expect_s3_class(fit, "hetid_log_variance_fit")
  expect_null(fit$coef)
  expect_identical(fit$fit_status, "nonconvergence")
  expect_false(fit$converged)
  expect_true(is.na(fit$objective))
  expect_true(is.na(fit$score_norm))
  expect_identical(fit$convergence_code, -1L)
  expect_null(fit$warm_start)
  expect_identical(fit$diagnostics$error_class, "rank_unresolved")

  expect_identical(validate_hetid_log_variance_fit(fit), fit)
})

test_that("constructor coerces non-integer-valued n_obs to integer", {
  f <- make_log_variance_fit_inputs("ok")
  f$n_obs <- 60
  fit <- build_log_variance_fit(f)
  expect_identical(attr(fit, "n_obs"), 60L)
})

test_that("validator rejects an unknown fit_status", {
  f <- make_log_variance_fit_inputs("ok")
  f$fit_status <- "domain_failure"
  fit <- build_log_variance_fit(f)
  expect_error(
    validate_hetid_log_variance_fit(fit),
    "fit_status must be one of",
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects a coef/coef_labels length mismatch", {
  f <- make_log_variance_fit_inputs("ok")
  f$coef <- f$coef[1:2]
  fit <- build_log_variance_fit(f)
  expect_error(
    validate_hetid_log_variance_fit(fit),
    "names\\(coef\\) must equal coef_labels",
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects y/x_design dimension drift", {
  f <- make_log_variance_fit_inputs("ok")
  f$y <- f$y[-1]
  fit <- build_log_variance_fit(f)
  expect_error(
    validate_hetid_log_variance_fit(fit),
    class = "hetid_error_dimension_mismatch"
  )

  f2 <- make_log_variance_fit_inputs("ok")
  f2$x_design <- f2$x_design[-1, , drop = FALSE]
  fit2 <- build_log_variance_fit(f2)
  expect_error(
    validate_hetid_log_variance_fit(fit2),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("validator rejects non-list diagnostics", {
  f <- make_log_variance_fit_inputs("ok")
  f$diagnostics <- "not a list"
  fit <- build_log_variance_fit(f)
  expect_error(
    validate_hetid_log_variance_fit(fit),
    "diagnostics must be a list",
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects diagnostics missing start_attempts", {
  f <- make_log_variance_fit_inputs("ok")
  f$diagnostics$start_attempts <- NULL
  fit <- build_log_variance_fit(f)
  expect_error(
    validate_hetid_log_variance_fit(fit),
    "diagnostics must contain error_class and start_attempts",
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects ok status without converged", {
  f <- make_log_variance_fit_inputs("ok")
  f$converged <- FALSE
  fit <- build_log_variance_fit(f)
  expect_error(
    validate_hetid_log_variance_fit(fit),
    "converged must be TRUE when fit_status is ok",
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects nonconvergence status carrying coef", {
  f <- make_log_variance_fit_inputs("nonconvergence")
  f$coef <- stats::setNames(c(0.1, 0.2, 0.3), f$coef_labels)
  fit <- build_log_variance_fit(f)
  expect_error(
    validate_hetid_log_variance_fit(fit),
    "coef must be NULL when fit_status is nonconvergence",
    class = "hetid_error_bad_argument"
  )
})

test_that("validator rejects a missing diagnostics$error_class on failure", {
  f <- make_log_variance_fit_inputs("nonconvergence")
  f$diagnostics$error_class <- NA_character_
  fit <- build_log_variance_fit(f)
  expect_error(
    validate_hetid_log_variance_fit(fit),
    "error_class must be non-missing",
    class = "hetid_error_bad_argument"
  )
})

test_that("assert_hetid_log_variance_fit rejects plain lists", {
  f <- make_log_variance_fit_inputs("ok")
  fit <- build_log_variance_fit(f)
  expect_error(
    assert_hetid_log_variance_fit(unclass(fit)),
    "hetid_log_variance_fit object",
    class = "hetid_error_bad_argument"
  )
  expect_true(assert_hetid_log_variance_fit(fit))
})

test_that("log_variance_fit_ok truth table on constructor-built objects", {
  ok_fit <- build_log_variance_fit(make_log_variance_fit_inputs("ok"))
  expect_true(log_variance_fit_ok(ok_fit))

  fail_fit <- build_log_variance_fit(make_log_variance_fit_inputs("nonconvergence"))
  expect_false(log_variance_fit_ok(fail_fit))

  f_unconverged <- make_log_variance_fit_inputs("ok")
  f_unconverged$converged <- FALSE
  expect_false(log_variance_fit_ok(build_log_variance_fit(f_unconverged)))

  f_na_coef <- make_log_variance_fit_inputs("ok")
  f_na_coef$coef[2] <- NA_real_
  expect_false(log_variance_fit_ok(build_log_variance_fit(f_na_coef)))
})

test_that("print shows the header, estimator, fit_status, and n_obs", {
  fit <- build_log_variance_fit(make_log_variance_fit_inputs("ok"))
  printed <- capture.output(print(fit))
  expect_true(any(grepl("<hetid_log_variance_fit>", printed, fixed = TRUE)))
  expect_true(any(grepl("estimator: ppml", printed, fixed = TRUE)))
  expect_true(any(grepl("fit_status: ok", printed, fixed = TRUE)))
  expect_true(any(grepl("n_obs: 60", printed, fixed = TRUE)))
})

test_that("print returns its argument invisibly", {
  fit <- build_log_variance_fit(make_log_variance_fit_inputs("ok"))
  expect_invisible(print(fit))
  expect_identical(withVisible(print(fit))$value, fit)
})
