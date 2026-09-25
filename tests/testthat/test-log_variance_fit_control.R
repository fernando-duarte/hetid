test_that("fitting controls preserve defaults and reject malformed overrides", {
  d <- simulate_logvar_data()
  for (est in c("ppml", "harvey")) {
    base <- fit_log_variance(d$y, d$x, estimator = est)
    expect_identical(base, fit_log_variance(d$y, d$x, estimator = est, control = list()))
    for (ctrl in list(
      list(unknown = 1), list(SCORE_TOLERANCE = NA_real_),
      list(SCORE_TOLERANCE = 0), list(SCORE_TOLERANCE = NULL),
      list(SKIP_NONFINITE_STARTS = NA), list(SCORE_TOLERANCE = c(1, 2)),
      list(SCORE_TOLERANCE = factor(1)), list(SCORE_TOLERANCE = Inf),
      list(SCORE_TOLERANCE = matrix(1)), list(1),
      setNames(list(1, 2), c("SCORE_TOLERANCE", "SCORE_TOLERANCE"))
    )) {
      expect_error(fit_log_variance(d$y, d$x, est, control = ctrl), class = "hetid_error")
    }
  }
})

test_that("PPML start order and numerical controls reach the solver", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x, control = list(
    START_ORDER = c("glm_default", "supplied", "fallback", "intercept_only")
  ))
  expect_true(log_variance_fit_ok(fit))
  expect_identical(fit$diagnostics$start_attempts[[1]]$source, "glm_default")
  failed <- fit_log_variance(d$y, d$x, control = list(GLM_MAXIT = 1L))
  expect_false(log_variance_fit_ok(failed))
  expect_error(fit_log_variance(d$y, d$x, control = list(GLM_MAXIT = 1.5)),
    class = "hetid_error_bad_argument"
  )
})

test_that("Harvey warm-only policy and nonfinite-start evidence are explicit", {
  d <- simulate_logvar_data()
  empty <- fit_log_variance(d$y, d$x, "harvey", control = list(AUTO_INTERCEPT = FALSE))
  expect_false(log_variance_fit_ok(empty))
  expect_length(empty$diagnostics$start_attempts, 0L)
  bad <- rep(Inf, ncol(d$x) + 1L)
  expect_error(fit_log_variance(d$y, d$x, "harvey", start = bad), class = "hetid_error")
  recovered <- fit_log_variance(d$y, d$x, "harvey",
    start = bad,
    control = list(SKIP_NONFINITE_STARTS = TRUE)
  )
  expect_true(log_variance_fit_ok(recovered))
  expect_identical(recovered$diagnostics$start_attempts[[1]]$error_class, "invalid_start")
  expect_true(is.matrix(recovered$diagnostics$info_matrix))
  expect_true(length(recovered$diagnostics$per_start_criteria) > 0L)
})

test_that("controls reject keys from another estimator and malformed start orders", {
  d <- simulate_logvar_data()
  expect_error(make_log_variance_fitter(d$x, "ppml", list(AUTO_INTERCEPT = FALSE)),
    class = "hetid_error_bad_argument"
  )
  expect_error(make_log_variance_fitter(d$x, "harvey", list(START_ORDER = "supplied")),
    class = "hetid_error_bad_argument"
  )
  invalid <- list("supplied", rep("supplied", 4), c("supplied", "fallback", NA, "glm_default"))
  for (order in invalid) {
    expect_error(make_log_variance_fitter(d$x, control = list(START_ORDER = order)),
      class = "hetid_error_bad_argument"
    )
  }
})

test_that("fitting at b forwards configured acceptance and start policy", {
  d <- simulate_logvar_data()
  w2 <- matrix(0, length(d$y), 1)
  for (estimator in c("ppml", "harvey")) {
    control <- if (estimator == "ppml") {
      list(GLM_MAXIT = 1L)
    } else {
      list(AUTO_INTERCEPT = FALSE)
    }
    fit <- fit_log_variance_at_b(0, sqrt(d$y), w2, d$x,
      estimator = estimator, control = control
    )
    expect_false(log_variance_fit_ok(fit))
    expect_identical(fit$diagnostics$fit_control[names(control)], control)
  }
})
