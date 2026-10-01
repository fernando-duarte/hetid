expect_harvey_numerical_failure <- function(fit, reason) {
  expect_s3_class(fit, "hetid_log_variance_fit")
  expect_identical(fit$fit_status, "nonconvergence")
  expect_false(fit$converged)
  expect_identical(fit$diagnostics$error_class, reason)
  expect_null(fit$coef)
  expect_null(fit$warm_start)
  expect_true(is.na(fit$objective))
  expect_true(is.na(fit$score_norm))
  expect_identical(fit$convergence_code, -1L)
  expect_null(fit$diagnostics$info_matrix)
  expect_false(hetid:::log_variance_fit_ok(fit))
  covariance <- compute_log_variance_vcov(fit)
  expect_named(covariance, LOG_VARIANCE_HARVEY_CONTROL$SE_TYPES)
  expect_true(all(is.na(unlist(covariance))))
  expect_true(all(is.na(as.matrix(compute_log_variance_se(fit)[-1L]))))
}

test_that("stalled Harvey steps cannot expose an accepted fit", {
  x <- cbind(v = c(-1, 1, -0.5, 1, 0.4, -0.5))
  y <- c(0.3, 0.36, 1, 7, 0.15, 0.16)
  start <- c(0.4, -3.5)
  control <- list(LINE_SEARCH_HALVINGS = 0L, AUTO_INTERCEPT = FALSE)
  x_mat <- hetid:::log_variance_design(x)
  pos <- y > 0
  col_abs <- colSums(abs(x_mat))
  cur <- hetid:::harvey_eval(start, y, x_mat, pos, col_abs)
  resolved <- hetid:::log_variance_fit_control("harvey", control)
  newton <- hetid:::harvey_newton_dir(cur, x_mat, resolved)
  expect_false(is.null(newton))
  fisher <- drop(solve(crossprod(x_mat), cur$moment))
  criterion <- function(theta) {
    eta <- drop(x_mat %*% theta)
    0.5 * sum(eta + y / exp(eta))
  }
  expect_gt(criterion(start + newton), criterion(start))
  expect_gt(criterion(start + fisher), criterion(start))
  scored <- hetid:::harvey_scoring(
    cur, y, x_mat, pos, col_abs, chol(crossprod(x_mat)), resolved
  )
  expect_identical(scored$status, "line_search_stall")
  expect_identical(scored$iters, -1L)
  expect_identical(scored$eval$theta, start)
  fit <- fit_log_variance(y, x, "harvey", start = start, control = control)
  expect_harvey_numerical_failure(fit, "line_search_stall")
  expect_identical(fit$diagnostics$start_attempts, list(list(
    source = "supplied", error_class = "line_search_stall"
  )))
  expect_identical(fit$diagnostics$per_start_criteria[[1]]$status, "line_search_stall")
  recovered <- fit_log_variance(
    y, x, "harvey",
    start = start, control = list(AUTO_INTERCEPT = FALSE)
  )
  expect_true(hetid:::log_variance_fit_ok(recovered))
  expect_gt(recovered$diagnostics$n_halvings, 0L)
  expect_true(all(is.finite(unlist(compute_log_variance_vcov(recovered)))))
  fallback <- fit_log_variance(
    y, x, "harvey",
    start = start, fallback_starts = list(c(0, 0)), control = control
  )
  expect_true(hetid:::log_variance_fit_ok(fallback))
  expect_equal(fallback$coef, recovered$coef, tolerance = 1e-8)
  expect_identical(fallback$diagnostics$start_attempts[[1]], list(
    source = "supplied", error_class = "line_search_stall"
  ))
  expect_identical(fallback$diagnostics$start_attempts[[2]], list(
    source = "fallback", error_class = NA_character_
  ))
})

test_that("Harvey rejects a converged point with ill-conditioned information", {
  v <- seq(-1, 1, length.out = 9)
  perturbation <- rep(c(1, -1, 0), 3)
  x <- cbind(a = v, b = v + 1e-5 * perturbation)
  y <- rep(1, length(v))
  x_mat <- hetid:::log_variance_design(x)
  start <- rep(0, ncol(x_mat))
  control <- hetid:::log_variance_fit_control("harvey", list(AUTO_INTERCEPT = FALSE))
  expect_identical(qr(x_mat, tol = control$RANK_TOLERANCE)$rank, 3L)
  expect_true(all(is.finite(chol(crossprod(x_mat)))))
  info <- 0.5 * crossprod(x_mat)
  rc <- rcond(info / tcrossprod(sqrt(diag(info))))
  expect_gt(rc, 0)
  expect_lt(rc, control$RCOND_TOLERANCE)
  expect_identical(unname(drop(crossprod(x_mat, y - 1))), c(0, 0, 0))
  expect_null(hetid:::harvey_post_stop(
    start, y, x_mat, y > 0, colSums(abs(x_mat)), control
  ))
  fit <- fit_log_variance(
    y, x, "harvey",
    start = start, control = list(AUTO_INTERCEPT = FALSE)
  )
  expect_harvey_numerical_failure(fit, "post_stop_reject")
  expect_identical(fit$diagnostics$per_start_criteria[[1]]$status, "converged")
  expect_equal(fit$diagnostics$per_start_criteria[[1]]$score_norm, 0)
  expect_identical(fit$diagnostics$start_attempts, list(list(
    source = "supplied", error_class = "post_stop_reject"
  )))
  relaxed <- fit_log_variance(
    y, x, "harvey",
    start = start,
    control = list(AUTO_INTERCEPT = FALSE, RCOND_TOLERANCE = 1e-12)
  )
  expect_true(hetid:::log_variance_fit_ok(relaxed))
  expect_equal(relaxed$diagnostics$rcond_info, rc, tolerance = 1e-5)
  stable <- cbind(a = v, b = v + 0.1 * perturbation)
  accepted <- fit_log_variance(
    y, stable, "harvey",
    start = start, control = list(AUTO_INTERCEPT = FALSE)
  )
  expect_true(hetid:::log_variance_fit_ok(accepted))
  expect_equal(unname(accepted$coef), start)
})
