{
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
}

{
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
}

{
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
}
