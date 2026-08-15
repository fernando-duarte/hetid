# Tests for the PPML log-variance internals: the design builder, the estimator
# registry, the acceptance gates, the start ladder, and the scaled-response
# guards. Everything here is unexported, so it is reached via hetid:::.

# multiplicative chi-square response on exp(eta), the shape the log-variance
# equation actually feeds the estimator
ppml_test_response <- function(t_obs = 300, seed = 7) {
  set.seed(seed)
  x <- cbind(v1 = rnorm(t_obs), v2 = rnorm(t_obs))
  eta <- drop(cbind(1, x) %*% c(-0.5, 0.6, -0.4))
  list(y = exp(eta) * rchisq(t_obs, df = 1), x = x)
}

test_that("log_variance_design labels the design and rejects ambiguous names", {
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
})

test_that("log_variance_estimator owns the valid-estimator set", {
  spec <- hetid:::log_variance_estimator("ppml")
  expect_identical(spec$id, "ppml")
  expect_identical(spec$se_types, LOG_VARIANCE_CONTROL$SE_TYPES)
  expect_true(is.function(spec$fit_response))

  for (id in names(hetid:::log_variance_estimator_specs())) {
    entry <- hetid:::log_variance_estimator(id)
    expect_true(
      all(c("id", "label", "fit_response", "se_types") %in% names(entry))
    )
    expect_identical(entry$id, id)
    expect_true(is.character(entry$label) && length(entry$label) == 1L)
  }

  err <- expect_error(
    hetid:::log_variance_estimator("harvey"),
    class = "hetid_error_bad_argument"
  )
  expect_match(conditionMessage(err), "ppml")
})

test_that("ppml_pos_rank ranks the positive-response rows only", {
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
})

test_that("ppml_accept fails closed on non-finite coef and no convergence", {
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
})

test_that("ppml_accept rejects a non-positive information column scale", {
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
})

test_that("capture_glm_conditions records conditions and muffles them", {
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
})

test_that("the ladder records an overflowing start and keeps going", {
  d <- ppml_test_response()
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
})

test_that("the scaled-response guards fail closed with distinct classes", {
  d <- ppml_test_response()
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
})

test_that("a clean simulated response fits end to end", {
  d <- ppml_test_response()
  x_mat <- hetid:::log_variance_design(d$x)
  fit <- hetid:::ppml_fit_response(d$y, x_mat)

  expect_identical(fit$fit_status, "ok")
  expect_true(hetid:::log_variance_fit_ok(fit))
  expect_named(fit$coef, colnames(x_mat))
  expect_named(fit$warm_start, colnames(x_mat))
  expect_lt(fit$score_norm, LOG_VARIANCE_CONTROL$SCORE_TOLERANCE)
  expect_identical(attr(fit, "n_obs"), nrow(x_mat))
})

# Oracle tests for the exported fit_log_variance() wrapper: boundary
# validation plus end-to-end parity against a direct glm.fit call.

test_that("coefficients match a direct glm.fit parity run", {
  # quasipoisson, matching production: poisson would warn on every
  # non-integer response; the mathematical (oracle) check is the
  # score-equation test below, this one pins glm.fit parity
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x)
  parity <- stats::glm.fit(
    x = cbind(1, d$x), y = d$y, family = stats::quasipoisson(link = "log"),
    control = stats::glm.control(epsilon = 1e-10, maxit = 100L)
  )
  expect_true(log_variance_fit_ok(fit))
  expect_equal(unname(fit$coef), unname(parity$coefficients), tolerance = 1e-8)
})

test_that("score equation holds at the reported coefficients", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x)
  x_mat <- cbind(1, d$x)
  mu <- drop(exp(x_mat %*% fit$coef))
  scaled <- abs(crossprod(x_mat, d$y - mu)) /
    (max(1, median(d$y[d$y > 0])) * colSums(abs(x_mat)))
  expect_lt(max(scaled), LOG_VARIANCE_CONTROL$SCORE_TOLERANCE)
})

test_that("response_scale shifts only the intercept by log(s)", {
  # within one fit the identity coef[1] - warm_start[1] == log(s) is exact by
  # construction; across the two runs glm.fit's mustart is not scale-equivariant,
  # so coefficients agree only to convergence tolerance
  d <- simulate_logvar_data()
  base <- fit_log_variance(d$y, d$x)
  scaled <- fit_log_variance(d$y, d$x, response_scale = 7)
  expect_identical(scaled$coef[[1]], scaled$warm_start[[1]] + log(7))
  expect_equal(scaled$coef, base$coef, tolerance = 1e-8)
  expect_equal(scaled$warm_start[-1], base$warm_start[-1], tolerance = 1e-8)
})

test_that("all-zero response fails closed, malformed arguments error", {
  d <- simulate_logvar_data()
  z_fit <- fit_log_variance(rep(0, nrow(d$x)), d$x)
  expect_false(log_variance_fit_ok(z_fit))
  expect_identical(z_fit$fit_status, "nonconvergence")
  expect_error(fit_log_variance(c(-1, d$y[-1]), d$x),
    class = "hetid_error_bad_argument"
  )
  expect_error(fit_log_variance(d$y[-1], d$x),
    class = "hetid_error_dimension_mismatch"
  )
})

test_that("rank-deficient positive-response design fails closed", {
  d <- simulate_logvar_data()
  x_dup <- cbind(d$x, dup = d$x[, 1])
  fit <- fit_log_variance(d$y, x_dup)
  expect_false(log_variance_fit_ok(fit))
})

test_that("a bad supplied start falls through the ladder and still fits", {
  d <- simulate_logvar_data()
  fit <- fit_log_variance(d$y, d$x, start = c(1e6, 1e6, 1e6))
  expect_true(log_variance_fit_ok(fit))
  # start_attempts is a list of attempt records, one per ladder rung tried
  expect_gt(length(fit$diagnostics$start_attempts), 1L)
})

test_that("extreme regressor rescale keeps acceptance and coefficients", {
  d <- simulate_logvar_data()
  x_big <- d$x
  x_big[, 1] <- x_big[, 1] * 1e8
  fit <- fit_log_variance(d$y, x_big)
  base <- fit_log_variance(d$y, d$x)
  expect_true(log_variance_fit_ok(fit))
  expect_equal(fit$coef[["v1"]] * 1e8, base$coef[["v1"]], tolerance = 1e-6)
})

test_that("scaled-response underflow and overflow fail closed, not as base errors", {
  # y/response_scale can underflow to zero (svd on zero rows is a base error
  # without the guard) or overflow to Inf; partial underflow is the sneaky
  # case because zeros are otherwise valid responses
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
})

test_that("captured glm conditions land in diagnostics and do not escape", {
  # simulate_logvar_data()'s fixture is pinned clean by the parity and
  # score-equation tests above, and the ladder's final rung falls back to
  # glm.fit's own data-driven start (mustart from y), which converges in a
  # handful of iterations for every fixture searched (bimodal responses,
  # extreme outliers, near-collinear and huge-scale regressors) -- so a
  # fixture where the full ladder fails closed while still carrying a
  # warning could not be constructed deterministically. Substituting a
  # capture_glm_conditions()-level check around a real glm.fit call that
  # DOES warn (a supplied start far from the data-driven optimum, which
  # hits maxit): the warning must land in the recorded list and nothing
  # may escape, including through the full ladder that then recovers.
  d <- simulate_logvar_data()
  expect_no_warning(fit_log_variance(d$y, d$x))

  x_mat <- hetid:::log_variance_design(d$x)
  captured <- NULL
  expect_no_warning(
    captured <- hetid:::capture_glm_conditions(stats::glm.fit(
      x = x_mat, y = d$y, family = stats::quasipoisson(link = "log"),
      start = c(100, 0, 0),
      control = stats::glm.control(
        epsilon = LOG_VARIANCE_CONTROL$GLM_EPSILON,
        maxit = LOG_VARIANCE_CONTROL$GLM_MAXIT
      )
    ))
  )
  expect_true(any(grepl("did not converge", captured$warnings)))

  # the same bad start still recovers through the wrapper's ladder, and the
  # muffled warning from its rejected first rung never escapes to the caller
  fit <- NULL
  expect_no_warning(fit <- fit_log_variance(d$y, d$x, start = c(100, 0, 0)))
  expect_true(log_variance_fit_ok(fit))
  expect_identical(
    fit$diagnostics$start_attempts[[1]]$error_class, "irls_not_converged"
  )
})

test_that("a wrong-length start or fallback element is rejected", {
  d <- simulate_logvar_data()
  expect_error(fit_log_variance(d$y, d$x, start = c(1, 2)),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    fit_log_variance(d$y, d$x, fallback_starts = list(c(1, 2))),
    class = "hetid_error_bad_argument"
  )
})

test_that("a non-list fallback_starts is rejected", {
  d <- simulate_logvar_data()
  expect_error(
    fit_log_variance(d$y, d$x, fallback_starts = c(1, 2, 3)),
    class = "hetid_error_bad_argument"
  )
})

test_that("a matrix or character fallback_starts element is rejected", {
  d <- simulate_logvar_data()
  expect_error(
    fit_log_variance(d$y, d$x, fallback_starts = list(matrix(1, 1, 3))),
    class = "hetid_error_bad_argument"
  )
  expect_error(
    fit_log_variance(d$y, d$x, fallback_starts = list(c("a", "b", "c"))),
    class = "hetid_error_bad_argument"
  )
})

test_that("a named start with permuted names is rejected, in-order names accepted", {
  d <- simulate_logvar_data()
  labels <- colnames(hetid:::log_variance_design(d$x))

  permuted <- stats::setNames(c(0, 0.1, -0.1), rev(labels))
  expect_error(
    fit_log_variance(d$y, d$x, start = permuted),
    class = "hetid_error_bad_argument"
  )

  ordered <- stats::setNames(c(0, 0.1, -0.1), labels)
  fit <- fit_log_variance(d$y, d$x, start = ordered)
  expect_true(log_variance_fit_ok(fit))
})
