test_that("matched empty and small residual samples reject before diagnostics", {
  for (warn in c(1, 2)) {
    withr::local_options(warn = warn)
    for (n_obs in 0:2) {
      warnings <- list()
      err <- tryCatch(withCallingHandlers(
        fit_log_variance_at_b(
          0, rep(1, n_obs), matrix(seq_len(n_obs), ncol = 1),
          matrix(seq_len(n_obs), ncol = 1)
        ),
        warning = function(cond) warnings[[length(warnings) + 1L]] <<- cond
      ), error = identity)
      expect_length(warnings, 0L)
      expect_s3_class(err, "hetid_error_insufficient_data")
      expect_s3_class(err, "hetid_error")
      expect_match(conditionMessage(err), paste0("got ", n_obs, ", need at least 3"))
    }
  }
})

test_that("success and failure residual diagnostics retain the original minimum", {
  x <- matrix(seq(-1, 1, length.out = 20),
    ncol = 1,
    dimnames = list(NULL, "v1")
  )
  w2 <- matrix(seq_len(nrow(x)), ncol = 1, dimnames = list(NULL, "news1"))
  b <- c(news1 = 0.1)
  eps <- exp(0.1 + 0.2 * x[, 1])
  for (estimator in c("ppml", "harvey")) {
    for (residual in list(eps, rep(0, length(eps)))) {
      direct <- fit_log_variance(residual^2, x, estimator, response_scale = 2)
      result <- withVisible(fit_log_variance_at_b(
        b, drop(w2 %*% b) + residual, w2, x, estimator,
        response_scale = 2
      ))
      expect_true(result$visible)
      actual_eps <- drop(result$value$y^0.5)
      expect_equal(result$value$diagnostics$min_abs_eps, min(abs(actual_eps)))
      fit <- result$value
      fit$diagnostics$min_abs_eps <- NULL
      expect_equal(fit, direct, tolerance = 1e-12)
      expect_identical(attr(fit, "coef_labels"), c("(Intercept)", "v1"))
      expect_identical(dim(fit$x_design), c(20L, 2L))
      validated <- withVisible(validate_hetid_log_variance_fit(result$value))
      expect_false(validated$visible)
      expect_identical(validated$value, result$value)
    }
  }
})

test_that("public failure fixtures require exactly a FALSE convergence flag", {
  x <- matrix(seq(-1, 1, length.out = 20), ncol = 1)
  malformed <- list(TRUE, NA, NULL, logical(), c(FALSE, FALSE), 0, "FALSE")
  for (estimator in c("ppml", "harvey")) {
    failed <- fit_log_variance(rep(0, nrow(x)), x, estimator)
    expect_identical(failed$fit_status, "nonconvergence")
    expect_identical(failed$converged, FALSE)
    valid <- withVisible(validate_hetid_log_variance_fit(failed))
    expect_false(valid$visible)
    expect_identical(valid$value, failed)
    for (flag in malformed) {
      bad <- failed
      bad["converged"] <- list(flag)
      err <- tryCatch(validate_hetid_log_variance_fit(bad), error = identity)
      expect_s3_class(err, "hetid_error_bad_argument")
      expect_s3_class(err, "hetid_error")
      expect_identical(err$arg, "converged")
    }
  }
})
