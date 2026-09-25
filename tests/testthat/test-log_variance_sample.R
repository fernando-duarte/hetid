test_that("retained PPML and Harvey samples reproduce the old marginal profile", {
  d <- simulate_tau0_dgp(t_obs = 100)
  box <- compute_identified_set_box(compute_tau0_system(d$y1, d$y2, d$x, d$z),
    0.02,
    n_grid = 5
  )
  for (estimator in c("ppml", "harvey")) {
    out <- sample_log_variance_set(box, d$x_var, estimator, n_points = 2)
    expect_identical(out$bounds, profile_log_variance_set(box, d$x_var, estimator, n_points = 2))
    expect_equal(unname(out$candidates), unname(profile_set_candidates(box, 2)))
    warm <- NULL
    for (i in seq_len(nrow(out$candidates))) {
      fit <- fit_log_variance_at_b(out$candidates[i, ], box$w1, box$w2, d$x_var,
        estimator = estimator, start = warm
      )
      expect_identical(out$fits[[i]]$diagnostics, fit$diagnostics)
      if (log_variance_fit_ok(fit)) {
        expect_identical(out$coefficients[i, ], fit$coef)
        warm <- fit$warm_start
      } else {
        expect_true(all(is.na(out$coefficients[i, ])))
      }
    }
    newx <- d$x_var[1:5, , drop = FALSE]
    predicted <- predict(out, newx)
    ok <- stats::complete.cases(out$coefficients)
    eta <- cbind(1, newx) %*% t(out$coefficients[ok, , drop = FALSE])
    expect_equal(predicted$bounds$lower, unname(apply(eta, 1, min)), tolerance = 1e-12)
    expect_equal(predicted$bounds$upper, unname(apply(eta, 1, max)), tolerance = 1e-12)
    expect_output(print(out), "hetid_log_variance_sample")
  }
})

test_that("failed candidates retain identity and only successful starts advance", {
  parts <- sample_box_fixture()
  attempt <- 0L
  starts <- list()
  testthat::local_mocked_bindings(make_log_variance_fitter = function(...) {
    function(y, start = NULL, ...) {
      attempt <<- attempt + 1L
      starts[attempt] <<- list(start)
      if (attempt == 2L) {
        return(sample_mock_fit())
      }
      sample_mock_fit(c("(Intercept)" = attempt, v1 = -attempt))
    }
  })
  out <- sample_log_variance_set(parts$box, parts$x, n_points = 2)
  expect_identical(nrow(out$candidates), 5L)
  expect_identical(attr(out$bounds, "n_failed"), 1L)
  expect_true(all(is.na(out$coefficients[2, ])))
  expect_identical(out$fits[[2]]$diagnostics$error_class, "fixture_failure")
  expect_null(starts[[1]])
  expect_identical(starts[[2]], starts[[3]])
  predicted <- predict(out, matrix(0, 1, 1))
  expect_identical(predicted$candidate_lower, 1L)
  expect_identical(predicted$candidate_upper, 5L)
  expect_identical(predicted$bounds$upper_status, "sampled_partial")
})

test_that("empty geometry and all failed fits remain distinct", {
  parts <- sample_box_fixture()
  infinite <- parts$box
  infinite$bounds$upper <- Inf
  infinite$arg_upper[, ] <- NA_real_
  absent <- sample_log_variance_set(infinite, parts$x)
  expect_identical(absent$reason, "infinite_box")
  expect_identical(nrow(absent$coefficients), 0L)
  expect_true(all(is.na(predict(absent)$bounds$lower)))
  expect_error(sample_log_variance_set(infinite, parts$x, estimator = "bogus"),
    class = "hetid_error_bad_argument"
  )
  testthat::local_mocked_bindings(make_log_variance_fitter = function(...) {
    function(...) sample_mock_fit()
  })
  failed <- sample_log_variance_set(parts$box, parts$x)
  expect_identical(failed$reason, "all_fits_failed")
  expect_identical(attr(failed$bounds, "n_failed"), nrow(failed$candidates))
  expect_true(all(predict(failed)$bounds$lower_status == "all_fits_failed"))
})

test_that("retained sample axes and dates reject malformed inputs", {
  parts <- sample_box_fixture()
  out <- sample_log_variance_set(parts$box, parts$x)
  expect_error(sample_log_variance_set(parts$box, parts$x[-1, , drop = FALSE]),
    class = "hetid_error_dimension_mismatch"
  )
  expect_error(sample_log_variance_set(parts$box, parts$x, dates = seq_len(20)),
    class = "hetid_error_bad_argument"
  )
  bad <- out
  colnames(bad$coefficients) <- rev(colnames(bad$coefficients))
  expect_error(predict(bad), "axes", class = "hetid_error_bad_argument")
  bad <- out
  bad$fits[[1]]$converged <- FALSE
  expect_error(predict(bad), "disagree", class = "hetid_error_bad_argument")
  bad <- out
  bad$coefficients[1, ] <- NA_real_
  expect_error(predict(bad), "statuses", class = "hetid_error_bad_argument")
})


test_that("no retained candidates remains distinct from fitting failure", {
  parts <- sample_box_fixture()
  testthat::local_mocked_bindings(profile_set_candidates = function(...) NULL)
  out <- sample_log_variance_set(parts$box, parts$x)
  expect_identical(out$reason, "no_candidates")
  expect_identical(attr(out$bounds, "n_attempted"), 0L)
  expect_identical(attr(out$bounds, "n_failed"), 0L)
  expect_true(all(predict(out)$bounds$lower_status == "no_candidates"))
})

test_that("invalid responses remain structured errors rather than failed fits", {
  parts <- sample_box_fixture()
  parts$box$w1[] <- 1e200
  expect_error(sample_log_variance_set(parts$box, parts$x), class = "hetid_error")
})


test_that("retained marginal summaries agree with joint coefficient rows", {
  parts <- sample_box_fixture()
  sampled <- sample_log_variance_set(parts$box, parts$x)
  sampled$bounds$lower[1] <- sampled$bounds$lower[1] - 1
  expect_error(predict(sampled), "summary disagrees", class = "hetid_error_bad_argument")
})
