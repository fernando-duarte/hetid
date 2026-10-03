# PPML log-variance internals: the design builder, the estimator registry, the acceptance
# gates, the start ladder, and the scaled-response guards. Unexported, so via hetid:::

test_that("log_variance_design labels the design and rejects ambiguous names", {
  eval(parse("bodies/fit_log_variance-contracts.R", encoding = "UTF-8")[[1]], environment())
})

test_that("log_variance_estimator owns the valid-estimator set", {
  eval(parse("bodies/fit_log_variance-contracts.R", encoding = "UTF-8")[[2]], environment())
})

test_that("ppml_pos_rank ranks the positive-response rows only", {
  eval(parse("bodies/fit_log_variance-contracts.R", encoding = "UTF-8")[[3]], environment())
})

test_that("ppml_accept fails closed on non-finite coef and no convergence", {
  eval(parse("bodies/fit_log_variance-contracts.R", encoding = "UTF-8")[[4]], environment())
})

test_that("ppml_accept rejects a non-positive information column scale", {
  eval(parse("bodies/fit_log_variance-contracts.R", encoding = "UTF-8")[[5]], environment())
})

test_that("capture_glm_conditions records conditions and muffles them", {
  eval(parse("bodies/fit_log_variance-contracts.R", encoding = "UTF-8")[[6]], environment())
})

test_that("the ladder records an overflowing start and keeps going", {
  eval(parse("bodies/fit_log_variance-contracts.R", encoding = "UTF-8")[[7]], environment())
})

test_that("the scaled-response guards fail closed with distinct classes", {
  eval(parse("bodies/fit_log_variance-contracts.R", encoding = "UTF-8")[[8]], environment())
})

test_that("a clean simulated response fits end to end", {
  d <- simulate_logvar_data()
  x_mat <- hetid:::log_variance_design(d$x)
  fit <- hetid:::ppml_fit_response(d$y, x_mat)

  expect_identical(fit$fit_status, "ok")
  expect_true(hetid:::log_variance_fit_ok(fit))
  expect_named(fit$coef, colnames(x_mat))
  expect_named(fit$warm_start, colnames(x_mat))
  expect_lt(fit$score_norm, LOG_VARIANCE_CONTROL$SCORE_TOLERANCE)
  expect_identical(attr(fit, "n_obs"), nrow(x_mat))
})

test_that("fit_log_variance returns visibly on both the ok and nonconvergence paths", {
  d <- simulate_logvar_data()
  ok_visible <- withVisible(fit_log_variance(d$y, d$x))
  expect_true(ok_visible$visible)
  expect_identical(ok_visible$value$fit_status, "ok")

  failed_visible <- withVisible(fit_log_variance(rep(0, nrow(d$x)), d$x))
  expect_true(failed_visible$visible)
  expect_identical(failed_visible$value$fit_status, "nonconvergence")
})

# Oracle tests for the exported fit_log_variance() wrapper: boundary
# validation plus end-to-end parity against a direct glm.fit call

test_that("coefficients match a direct glm.fit parity run", {
  # quasipoisson, matching production: poisson would warn on every non-integer response; the
  # mathematical (oracle) check is the score-equation test below, this one pins glm.fit parity
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
  # Within a fit, coef[1] - warm_start[1] == log(s) by construction; glm.fit's mustart is not
  # scale-equivariant across runs, so their coefficients agree only to convergence tolerance
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
  eval(parse("bodies/fit_log_variance-contracts.R", encoding = "UTF-8")[[9]], environment())
})

test_that("captured glm conditions land in diagnostics and do not escape", {
  eval(parse("bodies/fit_log_variance-inputs.R", encoding = "UTF-8")[[1]], environment())
})

# GLM_MAXIT = 1 exhausts every rung by construction; the old near-collinear fixture hit the cap
# only where platform rounding kept glm.fit's deviance unsettled
test_that("exhausting IRLS on every rung fails closed with the warning on record", {
  eval(parse("bodies/fit_log_variance-inputs.R", encoding = "UTF-8")[[2]], environment())
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
