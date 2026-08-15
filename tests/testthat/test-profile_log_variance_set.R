# Profiling the log-variance coefficients over an identified set: parity
# with the single-b fit, warm starts advancing only on success, and the
# fail-closed paths that must report NA rather than a narrowed range.

profile_box <- function(tau = 0.05, n_grid = 11L) {
  d <- simulate_box_dgp()
  fit <- compute_tau0_system(d$y1, d$y2, d$x, d$z)
  list(
    box = compute_identified_set_box(fit, tau = tau, n_grid = n_grid),
    x_var = d$x_var,
    fit = fit
  )
}

ok_fit <- function(coef, warm) {
  list(
    coef = coef, fit_status = "ok", converged = TRUE, objective = 0,
    score_norm = 0, convergence_code = 0L, warm_start = warm,
    diagnostics = list()
  )
}

failed_fit <- function() {
  list(
    coef = NULL, fit_status = "nonconvergence", converged = FALSE,
    objective = NA_real_, score_norm = NA_real_, convergence_code = -1L,
    warm_start = NULL, diagnostics = list(error_class = "fit_error")
  )
}

test_that("the profile brackets a direct fit inside the same set", {
  parts <- profile_box()
  prof <- profile_log_variance_set(parts$box, parts$x_var)
  direct <- fit_log_variance_at_b(
    parts$fit$point$theta, parts$box$w1, parts$box$w2, parts$x_var
  )
  expect_true(all(direct$coef >= prof$lower - 1e-10))
  expect_true(all(direct$coef <= prof$upper + 1e-10))
  expect_identical(prof$term, names(direct$coef))
})

test_that("every profiled point lies in the set", {
  parts <- profile_box()
  checker <- make_system_checker(parts$box$quadratic)
  candidates <- profile_set_candidates(parts$box, 5L)
  slack <- apply(candidates, 1, function(b) max(checker(b)))
  expect_true(all(slack <= IDENTIFIED_SET_CONTROL$FEAS_TOL))
})

test_that("a single candidate reproduces the single-b fit exactly", {
  parts <- profile_box()
  point <- parts$fit$point$theta
  box <- parts$box
  box$arg_lower <- matrix(point, nrow = nrow(box$arg_lower), ncol = length(point), byrow = TRUE)
  box$arg_upper <- box$arg_lower
  prof <- profile_log_variance_set(box, parts$x_var, n_points = 1L)
  direct <- fit_log_variance_at_b(point, box$w1, box$w2, parts$x_var)
  expect_equal(prof$lower, unname(direct$coef), tolerance = 1e-10)
  expect_equal(prof$upper, unname(direct$coef), tolerance = 1e-10)
  expect_identical(attr(prof, "n_failed"), 0L)
})

test_that("an infinite box yields NA rather than a truncated range", {
  parts <- profile_box()
  box <- parts$box
  box$bounds$upper[1] <- Inf
  box$arg_upper[1, ] <- NA_real_
  prof <- profile_log_variance_set(box, parts$x_var)
  expect_true(all(is.na(prof$lower)))
  expect_true(all(is.na(prof$upper)))
  expect_identical(attr(prof, "n_attempted"), 0L)
})

test_that("an unknown estimator is rejected by the registry", {
  parts <- profile_box()
  expect_error(
    profile_log_variance_set(parts$box, parts$x_var, estimator = "bogus"),
    class = "hetid_error_bad_argument"
  )
})

test_that("failed fits are skipped and counted, not fatal", {
  parts <- profile_box()
  attempt <- 0L
  testthat::local_mocked_bindings(
    fit_log_variance_at_b = function(b, w1, w2, x, estimator = "ppml",
                                     start = NULL, ...) {
      attempt <<- attempt + 1L
      if (attempt %% 2L == 0L) {
        failed_fit()
      } else {
        ok_fit(c("(Intercept)" = attempt * 1.0, v1 = 0, v2 = 0), c(1, 0, 0))
      }
    }
  )
  prof <- profile_log_variance_set(parts$box, parts$x_var, n_points = 2L)
  expect_gt(attr(prof, "n_failed"), 0L)
  expect_lt(attr(prof, "n_failed"), attr(prof, "n_attempted"))
  expect_false(anyNA(prof$lower))
})

test_that("all fits failing yields NA with the counts intact", {
  parts <- profile_box()
  testthat::local_mocked_bindings(
    fit_log_variance_at_b = function(...) failed_fit()
  )
  prof <- profile_log_variance_set(parts$box, parts$x_var, n_points = 2L)
  expect_true(all(is.na(prof$lower)))
  expect_true(all(is.na(prof$upper)))
  expect_identical(attr(prof, "n_failed"), attr(prof, "n_attempted"))
  expect_gt(attr(prof, "n_attempted"), 0L)
})

test_that("the warm start advances only on a successful fit", {
  parts <- profile_box()
  starts <- list()
  attempt <- 0L
  testthat::local_mocked_bindings(
    fit_log_variance_at_b = function(b, w1, w2, x, estimator = "ppml",
                                     start = NULL, ...) {
      attempt <<- attempt + 1L
      starts[[attempt]] <<- start
      if (attempt == 2L) {
        return(failed_fit())
      }
      ok_fit(
        c("(Intercept)" = 0, v1 = 0, v2 = 0),
        c(attempt * 1.0, 0, 0)
      )
    }
  )
  invisible(profile_log_variance_set(parts$box, parts$x_var, n_points = 2L))
  expect_null(starts[[1]])
  # call 2 carries call 1's warm start; call 2 fails, so call 3 must carry
  # call 1's again rather than nothing or the failed fit's NULL
  expect_identical(starts[[2]], c(1, 0, 0))
  expect_identical(starts[[3]], c(1, 0, 0))
})
