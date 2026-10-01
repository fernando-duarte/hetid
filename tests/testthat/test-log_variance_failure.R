test_that("failed fits retain their estimator and original data", {
  y <- c(0, 1, 2, 3)
  x_mat <- cbind("(Intercept)" = 1, v1 = c(-1, 0, 1, 2))
  attempts <- list(list(source = "supplied", error_class = "invalid_start"))
  failures <- list(harvey = harvey_failure, ppml = ppml_failure)

  for (estimator in names(failures)) {
    result <- withVisible(failures[[estimator]](
      "no_accepted_start", y, x_mat, response_scale = 1000,
      attempts = attempts, warnings = "recorded warning", rank_x_pos = 2L
    ))
    fit <- result$value

    expect_true(result$visible)
    expect_false(log_variance_fit_ok(fit))
    expect_identical(attr(fit, "estimator"), estimator)
    expect_identical(attr(fit, "response_scale"), 1000)
    expect_identical(attr(fit, "n_obs"), length(y))
    expect_identical(attr(fit, "coef_labels"), colnames(x_mat))
    expect_identical(fit$y, y)
    expect_identical(fit$x_design, x_mat)
    expect_identical(fit$diagnostics$error_class, "no_accepted_start")
    expect_identical(fit$diagnostics$start_attempts, attempts)
    expect_identical(fit$diagnostics$warnings, "recorded warning")
    expect_identical(fit$diagnostics$rank_x_pos, 2L)
    expect_null(fit$coef)
    expect_null(fit$warm_start)
    expect_true(is.na(fit$objective))
    expect_true(is.na(fit$score_norm))
    expect_identical(fit$convergence_code, -1L)
  }
})

test_that("failure diagnostics retain their estimator-specific NULL behavior", {
  y <- c(0, 1, 2)
  x_mat <- cbind("(Intercept)" = 1, v1 = c(-1, 0, 1))
  harvey <- harvey_failure("no_accepted_start", y, x_mat, 1, rank_x_pos = NULL)
  ppml <- ppml_failure("no_accepted_start", y, x_mat, 1, rank_x_pos = NULL)

  expect_true("rank_x_pos" %in% names(harvey$diagnostics))
  expect_null(harvey$diagnostics$rank_x_pos)
  expect_false("rank_x_pos" %in% names(ppml$diagnostics))
})
