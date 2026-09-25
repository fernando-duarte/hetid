test_that("prepared fits retain independent response and scale semantics", {
  set.seed(891)
  x <- cbind(v = rnorm(80))
  design <- cbind("(Intercept)" = 1, x)
  y <- exp(0.3 - 0.2 * x[, 1]) * rchisq(80, 2) / 2
  for (estimator in c("ppml", "harvey")) {
    fitter <- make_log_variance_fitter(x, estimator)
    family <- if (estimator == "ppml") quasipoisson("log") else Gamma("log")
    for (response in list(y, y * 7, rev(y))) {
      fit <- fitter(response)
      oracle <- glm.fit(design, response,
        family = family,
        control = glm.control(epsilon = 1e-12, maxit = 200)
      )
      expect_true(log_variance_fit_ok(fit))
      expect_equal(fit$coef, oracle$coefficients, tolerance = 1e-6)
      scaled <- fitter(response, response_scale = 7)
      expect_equal(scaled$coef, fit$coef, tolerance = 1e-6)
      expect_identical(fitter(response), fit)
    }
  }
})

test_that("a prepared fitter captures caller objects and checks each new call", {
  x <- cbind(v = seq(-1, 1, length.out = 30))
  original <- x
  control <- list(MAXIT = 150L)
  fitter <- make_log_variance_fitter(x, "harvey", control)
  x[, ] <- NA_real_
  control$MAXIT <- 1L
  y <- exp(0.2 + 0.1 * original[, 1])
  fit <- fitter(y)
  expect_equal(unname(fit$coef), c(0.2, 0.1), tolerance = 1e-7)
  expect_identical(fit$x_design[, -1L, drop = FALSE], original)
  expect_identical(fit$diagnostics$fit_control$MAXIT, 150L)
  expect_error(fitter(y[-1]), class = "hetid_error_dimension_mismatch")
  expect_error(fitter(replace(y, 1, -1)), class = "hetid_error_bad_argument")
  expect_error(fitter(replace(y, 1, Inf)), class = "hetid_error")
  expect_error(fitter(y, start = c(v = 0, "(Intercept)" = 0)), class = "hetid_error")
  expect_error(fitter(y, fallback_starts = list(0)), class = "hetid_error")
  expect_error(fitter(y, response_scale = 0), class = "hetid_error")
  expect_identical(fitter(y), fit)
})

test_that("prepared rank evidence never substitutes for positive-response rank", {
  x <- cbind(v = rep(c(0, 1), each = 10))
  ppml <- make_log_variance_fitter(x)
  expect_true(log_variance_fit_ok(ppml(rep(1, 20))))
  sparse <- ppml(c(rep(1, 10), rep(0, 10)))
  expect_false(log_variance_fit_ok(sparse))
  expect_identical(sparse$diagnostics$error_class, "rank_unresolved")
  expect_identical(ppml(rep(0, 20))$diagnostics$error_class, "all_zero_response")
  expect_true(log_variance_fit_ok(ppml(rep(1, 20))))
})

test_that("configured acceptance is recorded and Harvey diagnostic keys persist", {
  x <- cbind(v = seq(-1, 1, length.out = 30))
  y <- exp(0.2 + 0.1 * x[, 1])
  fit <- fit_log_variance(y, x, "harvey",
    start = c(3, 0),
    control = list(SCORE_TOLERANCE = 1e6, AUTO_INTERCEPT = FALSE)
  )
  expect_true(log_variance_fit_ok(fit))
  expect_equal(unname(fit$coef), c(3, 0))
  expect_identical(fit$diagnostics$fit_control$SCORE_TOLERANCE, 1e6)
  expect_true("per_start_criteria" %in% names(fit$diagnostics))
  expect_null(fit$diagnostics$per_start_criteria)
  empty <- fit_log_variance(y, x, "harvey", control = list(AUTO_INTERCEPT = FALSE))
  expect_identical(empty$diagnostics$error_class, "no_accepted_start")
  expect_true(all(c("per_start_criteria", "info_matrix") %in% names(empty$diagnostics)))
})
