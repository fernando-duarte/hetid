boundary_profile_fixture <- function() {
  d <- simulate_tau0_dgp(t_obs = 200)
  fit <- compute_tau0_system(d$y1, d$y2, d$x, d$z)
  list(
    finite = compute_identified_set_box(fit, tau = 0.05, n_grid = 3L),
    infinite = compute_identified_set_box(fit, tau = 0.95, n_grid = 3L),
    x = d$x_var
  )
}

test_that("public finite and infinite boxes enforce estimator and finite regressors", {
  parts <- boundary_profile_fixture()
  expect_true(all(is.finite(parts$finite$bounds$lower)))
  expect_true(all(is.finite(parts$finite$bounds$upper)))
  expect_true(any(is.infinite(parts$infinite$bounds$lower)))
  for (box in parts[c("finite", "infinite")]) {
    err <- tryCatch(
      profile_log_variance_set(box, parts$x, estimator = "bogus", n_points = 1L),
      error = identity
    )
    expect_s3_class(err, "hetid_error_bad_argument")
    expect_identical(err$arg, "estimator")
    for (value in c(NA_real_, NaN, Inf, -Inf)) {
      bad_x <- parts$x
      bad_x[1, 1] <- value
      err <- tryCatch(profile_log_variance_set(box, bad_x), error = identity)
      expect_s3_class(err, "hetid_error_bad_argument")
      expect_s3_class(err, "hetid_error")
      expect_identical(err$arg, "x_var")
      err <- tryCatch(
        profile_log_variance_set(box, bad_x, estimator = "bogus"),
        error = identity
      )
      expect_s3_class(err, "hetid_error_bad_argument")
      expect_identical(err$arg, "estimator")
    }
  }
})

test_that("profiling validates tabularity, labels, alignment and sample size first", {
  parts <- boundary_profile_fixture()
  bad_inputs <- list(
    parts$x[, 1], as.list(parts$x[, 1]),
    matrix("1", nrow(parts$x), ncol(parts$x))
  )
  for (box in parts[c("finite", "infinite")]) {
    for (bad_x in bad_inputs) {
      err <- tryCatch(profile_log_variance_set(box, bad_x), error = identity)
      expect_s3_class(err, "hetid_error_bad_argument")
      expect_identical(err$arg, "x_var")
    }
    for (labels in list(
      c("v1", "v1"), c("", "v2"), c(NA, "v2"),
      c("(Intercept)", "v2")
    )) {
      bad_x <- parts$x
      colnames(bad_x) <- labels
      err <- tryCatch(profile_log_variance_set(box, bad_x), error = identity)
      expect_s3_class(err, "hetid_error_bad_argument")
      expect_identical(err$arg, "x")
    }
    expect_error(
      profile_log_variance_set(box, parts$x[-1, , drop = FALSE]),
      class = "hetid_error_dimension_mismatch"
    )
    wide_x <- matrix(0, nrow(parts$x), nrow(parts$x) - 1L)
    expect_error(
      profile_log_variance_set(box, wide_x),
      class = "hetid_error_insufficient_data"
    )
  }
})

test_that("valid infinite-box designs return labeled bounds without preparing a fit", {
  parts <- boundary_profile_fixture()
  local_mocked_bindings(
    make_log_variance_fitter = function(...) stop_hetid("unexpected fitter preparation"),
    fit_log_variance_at_b = function(...) stop_hetid("unexpected numerical fit")
  )
  unnamed <- unname(parts$x)
  intercept_only <- matrix(numeric(), nrow(parts$x), 0L)
  designs <- list(parts$x, as.data.frame(parts$x), unnamed, intercept_only)
  labels <- list(c("v1", "v2"), c("v1", "v2"), c("pc1", "pc2"), character())
  for (estimator in c("ppml", "harvey")) {
    for (i in seq_along(designs)) {
      out <- withVisible(profile_log_variance_set(parts$infinite, designs[[i]], estimator))
      expect_true(out$visible)
      expect_identical(names(out$value), c("term", "lower", "upper"))
      expect_identical(out$value$term, c("(Intercept)", labels[[i]]))
      expect_identical(out$value$lower, rep(NA_real_, length(labels[[i]]) + 1L))
      expect_identical(out$value$upper, out$value$lower)
      expect_identical(attr(out$value, "n_attempted"), 0L)
      expect_identical(attr(out$value, "n_failed"), 0L)
      expect_identical(attr(out$value, "estimator"), estimator)
    }
  }
})

test_that("valid finite profiles retain coefficients, candidate filtering and metadata", {
  parts <- boundary_profile_fixture()
  candidates <- profile_set_candidates(parts$finite, 1L)
  checker <- make_relative_feasibility_checker(parts$finite$quadratic)
  expect_true(all(apply(candidates, 1, checker)))
  expect_identical(nrow(unique(candidates)), nrow(candidates))
  for (estimator in c("ppml", "harvey")) {
    profile <- profile_log_variance_set(parts$finite, parts$x, estimator, n_points = 1L)
    direct <- fit_log_variance_at_b(
      candidates[1, ], parts$finite$w1, parts$finite$w2, parts$x, estimator
    )
    expect_true(log_variance_fit_ok(direct))
    expect_identical(profile$term, names(direct$coef))
    expect_true(all(direct$coef >= profile$lower - 1e-10))
    expect_true(all(direct$coef <= profile$upper + 1e-10))
    expect_identical(attr(profile, "n_attempted"), nrow(candidates))
    expect_identical(attr(profile, "n_failed"), 0L)
    expect_identical(attr(profile, "estimator"), estimator)
    frame_profile <- profile_log_variance_set(
      parts$finite, as.data.frame(parts$x), estimator,
      n_points = 1L
    )
    expect_identical(frame_profile, profile)
  }
})
