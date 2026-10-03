{
  x <- matrix(stats::rnorm(20), 10, 2)
  design <- hetid:::log_variance_design(x)
  expect_identical(colnames(design), c("(Intercept)", "pc1", "pc2"))
  expect_identical(unname(design[, 1]), rep(1, 10))

  duplicated_labels <- matrix(stats::rnorm(20), 10, 2,
    dimnames = list(NULL, c("a", "a"))
  )
  expect_error(
    hetid:::log_variance_design(duplicated_labels),
    class = "hetid_error_bad_argument"
  )

  # an x column already called "(Intercept)" collides with the prepended one
  colliding <- cbind("(Intercept)" = stats::rnorm(10), b = stats::rnorm(10))
  expect_error(
    hetid:::log_variance_design(colliding),
    class = "hetid_error_bad_argument"
  )

  blank <- cbind(a = stats::rnorm(10), b = stats::rnorm(10))
  colnames(blank) <- c("a", "")
  expect_error(
    hetid:::log_variance_design(blank),
    class = "hetid_error_bad_argument"
  )
}

{
  spec <- hetid:::log_variance_estimator("ppml")
  expect_identical(spec$id, "ppml")
  expect_identical(spec$se_types, LOG_VARIANCE_CONTROL$SE_TYPES)
  expect_true(is.function(spec$fit_response))
  expect_true(is.function(spec$vcov))

  for (id in names(hetid:::log_variance_estimator_specs())) {
    entry <- hetid:::log_variance_estimator(id)
    expect_true(
      all(c("id", "label", "fit_response", "vcov", "se_types") %in% names(entry))
    )
    expect_identical(entry$id, id)
    expect_true(is.character(entry$label) && length(entry$label) == 1L)
  }

  err <- expect_error(
    hetid:::log_variance_estimator("egarch"),
    class = "hetid_error_bad_argument"
  )
  expect_match(conditionMessage(err), "ppml, harvey")
}

{
  set.seed(3)
  x <- cbind(v1 = stats::rnorm(30), v2 = stats::rnorm(30))
  x_mat <- hetid:::log_variance_design(x)
  y <- rep(1, 30)
  expect_identical(hetid:::ppml_pos_rank(y, x_mat), ncol(x_mat))

  x_dup <- hetid:::log_variance_design(cbind(x, dup = x[, 1]))
  expect_identical(hetid:::ppml_pos_rank(y, x_dup), ncol(x_dup) - 1L)

  # two positive rows cannot resolve a three-column design
  y_sparse <- c(1, 1, rep(0, 28))
  expect_lt(hetid:::ppml_pos_rank(y_sparse, x_mat), ncol(x_mat))
}

{
  x_mat <- hetid:::log_variance_design(cbind(v1 = rep(c(-1, 1), 10)))
  y_scaled <- rep(1, nrow(x_mat))
  coef_ok <- stats::setNames(c(0, 0), colnames(x_mat))

  nonfinite <- hetid:::ppml_accept(
    list(coefficients = coef_ok + c(Inf, 0), converged = TRUE, boundary = FALSE),
    y_scaled, x_mat
  )
  expect_false(nonfinite$accepted)
  expect_identical(nonfinite$reason, "nonfinite_coef")

  stalled <- hetid:::ppml_accept(
    list(coefficients = coef_ok, converged = FALSE, boundary = FALSE),
    y_scaled, x_mat
  )
  expect_false(stalled$accepted)
  expect_identical(stalled$reason, "irls_not_converged")
}

{
  # a zero column never reaches this gate in production -- ppml_pos_rank
  # rejects it first -- so the info_scale branch is unit-tested directly
  x_mat <- cbind("(Intercept)" = rep(1, 20), zero = 0)
  verdict <- hetid:::ppml_accept(
    list(
      coefficients = stats::setNames(c(0, 0), colnames(x_mat)),
      converged = TRUE, boundary = FALSE
    ),
    rep(1, 20), x_mat
  )
  expect_false(verdict$accepted)
  expect_identical(verdict$reason, "info_scale")
}

{
  captured <- NULL
  expect_silent(
    captured <- hetid:::capture_glm_conditions({
      warning("gate warning")
      message("gate message")
      42
    })
  )
  expect_identical(captured$value, 42)
  expect_match(captured$warnings, "gate warning")
  expect_match(captured$messages, "gate message")
  expect_true(is.na(captured$error_class))

  failed <- hetid:::capture_glm_conditions(stop("boom"))
  expect_null(failed$value)
  expect_identical(failed$error_class, "simpleError")
  expect_match(failed$error_message, "boom")
}

{
  d <- simulate_logvar_data()
  x_mat <- hetid:::log_variance_design(d$x)
  fit <- hetid:::ppml_fit_response(
    d$y, x_mat,
    start = rep(1e6, ncol(x_mat))
  )
  expect_true(hetid:::log_variance_fit_ok(fit))

  attempts <- fit$diagnostics$start_attempts
  expect_gt(length(attempts), 1L)
  expect_identical(attempts[[1L]]$source, "supplied")
  expect_identical(attempts[[1L]]$error_class, "invalid_start")
  expect_true(is.na(attempts[[length(attempts)]]$error_class))
}

{
  d <- simulate_logvar_data()
  x_mat <- hetid:::log_variance_design(d$x)

  y_partial <- d$y
  y_partial[1L] <- 1e-315
  partial <- hetid:::ppml_fit_response(y_partial, x_mat, response_scale = 1e10)
  expect_identical(partial$diagnostics$error_class, "scaled_response_underflow")

  y_over <- d$y
  y_over[1L] <- 1e300
  over <- hetid:::ppml_fit_response(y_over, x_mat, response_scale = 1e-300)
  expect_identical(over$diagnostics$error_class, "scaled_response_overflow")

  zero <- hetid:::ppml_fit_response(rep(0, nrow(x_mat)), x_mat)
  expect_identical(zero$diagnostics$error_class, "all_zero_response")

  for (failed in list(partial, over, zero)) {
    expect_false(hetid:::log_variance_fit_ok(failed))
    expect_identical(failed$fit_status, "nonconvergence")
    expect_null(failed$coef)
    expect_identical(failed$convergence_code, -1L)
  }
}

{
  # y/response_scale can overflow to Inf or underflow to zero; unguarded svd fails on zero rows
  # Partial underflow is harder to detect because zero responses are otherwise valid
  d <- simulate_logvar_data()
  total <- fit_log_variance(d$y * 1e-300, d$x, response_scale = 1e300)
  expect_false(log_variance_fit_ok(total))
  y_partial <- d$y
  y_partial[1] <- 1e-315
  partial <- fit_log_variance(y_partial, d$x, response_scale = 1e10)
  expect_false(log_variance_fit_ok(partial))
  expect_identical(partial$diagnostics$error_class, "scaled_response_underflow")
  y_over <- d$y
  y_over[1] <- 1e300
  over <- fit_log_variance(y_over, d$x, response_scale = 1e-300)
  expect_false(log_variance_fit_ok(over))
  expect_identical(over$diagnostics$error_class, "scaled_response_overflow")
}
