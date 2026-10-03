lv_set_harvey_control <- function() {
  list(
    estimator_version = "harvey-v1",
    fit = c(LOG_VARIANCE_HARVEY_CONTROL[setdiff(
      names(LOG_VARIANCE_HARVEY_CONTROL),
      "SE_TYPES"
    )], list(SKIP_NONFINITE_STARTS = TRUE)),
    recession_rank_tol = 1e-10,
    recession_rate_multiplier = 1e-9,
    certificate_tol = 1e-8,
    lp_xtol_rel = 1e-12,
    lp_maxeval = 2000L,
    lp_bound = 1e6,
    jacobian_rcond_tol = 1e-10,
    cold_start_rtol = 1e-6,
    fit_stage_policy = c("warm", "ppml_at_b", "standalone"),
    standalone_start_policy = c("ppml_point", "logols_shifted", "intercept_only")
  )
}

lv_set_normal_gap <- function() -(digamma(0.5) + log(2))

lv_set_failure <- function(fit_status, error_class, recession = NULL) {
  lv_set_fit_result(
    coef = NULL, fit_status = fit_status, converged = FALSE,
    diagnostics = list(
      error_class = error_class, start_attempts = list(),
      recession_certificate = recession
    )
  )
}

lv_set_harvey_fitter <- function(x_mat, control = lv_set_harvey_control()) {
  lv_set_assert(
    is.matrix(x_mat), identical(colnames(x_mat)[[1L]], "(Intercept)"),
    all(x_mat[, 1L] == 1)
  )
  build <- function(auto_intercept) {
    make_log_variance_fitter(
      x_mat[, -1L, drop = FALSE], "harvey",
      c(control$fit, list(AUTO_INTERCEPT = auto_intercept))
    )
  }
  complete <- build(TRUE)
  warm_only <- build(FALSE)
  recession_map <- list(
    negative_recession = c("nonexistence", "negative_recession"),
    zero_recession = c("nonconvergence", "zero_recession_unresolved"),
    certificate_failure = c("nonconvergence", "recession_certificate_failed")
  )
  function(y, start = NULL, fallback_starts = list(), auto_intercept = TRUE) {
    lv_set_assert(is.numeric(y), length(y) == nrow(x_mat), all(is.finite(y)), all(y >= 0))
    if (!any(y > 0)) {
      return(lv_set_failure("nonexistence", "negative_recession_all_zero"))
    }
    recession <- NULL
    if (any(y == 0)) {
      recession <- lv_set_recession(y, x_mat, control)
      if (!identical(recession$classification, "pass")) {
        mapped <- recession_map[[recession$classification]]
        return(lv_set_failure(mapped[[1L]], mapped[[2L]], recession))
      }
    }
    fit <- lv_set_slim_fit((if (auto_intercept) complete else warm_only)(y, start,
      fallback_starts))
    fit$diagnostics$recession_certificate <- recession
    fit
  }
}

lv_set_harvey_jacobian <- function(fit, b, w1, w2, x_mat, control) {
  if (!log_variance_fit_ok(fit)) {
    return(NULL)
  }
  theta <- fit$warm_start
  eps <- drop(w1 - w2 %*% b)
  eta <- drop(x_mat %*% theta)
  mu <- exp(eta)
  if (!all(is.finite(mu)) || any(mu <= 0)) {
    return(NULL)
  }
  information <- crossprod(x_mat, compute_harvey_ratio(theta, eps^2, x_mat) * x_mat)
  if (!all(is.finite(information)) || rcond(information) < control$jacobian_rcond_tol) {
    return(NULL)
  }
  # eps / mu on the log scale, an exact zero residual stays an exact zero
  eps_over_mu <- numeric(length(eps))
  nonzero <- eps != 0
  eps_over_mu[nonzero] <- sign(eps[nonzero]) * exp(log(abs(eps[nonzero])) - eta[nonzero])
  chol_r <- tryCatch(chol(information), error = function(e) NULL)
  if (is.null(chol_r)) {
    return(NULL)
  }
  out <- backsolve(chol_r, forwardsolve(
    t(chol_r),
    crossprod(x_mat, (-2 * eps_over_mu) * w2)
  ))
  rownames(out) <- colnames(x_mat)
  out
}

lv_set_harvey_estimator <- function(sample, point = NULL, ppml, logols_coef,
                                    control = lv_set_harvey_control()) {
  lv_set_assert(
    identical(control$fit_stage_policy, c("warm", "ppml_at_b", "standalone")),
    identical(
      control$standalone_start_policy,
      c("ppml_point", "logols_shifted", "intercept_only")
    ),
    identical(ppml$metadata$sample_id, sample$sample_id)
  )
  w1 <- sample$w1
  w2 <- sample$w2
  x_mat <- sample$x_mat
  fit_response <- lv_set_harvey_fitter(x_mat, control)
  fit_b <- function(b, start, fallbacks, auto_intercept) {
    lv_set_axis(b, colnames(w2), "b")
    eps <- drop(w1 - w2 %*% b)
    fit <- fit_response(eps^2, start, fallbacks, auto_intercept)
    fit$diagnostics$min_abs_eps <- min(abs(eps))
    fit
  }
  ladder <- c(
    if (!is.null(ppml$start_bundle)) list(ppml$start_bundle$coef_original),
    list(logols_coef + c(lv_set_normal_gap(), rep(0, length(logols_coef) - 1L)))
  )
  standalone <- function(b) fit_b(b, ladder[[1L]], ladder[-1L], TRUE)
  has_point <- !is.null(point) && !anyNA(point)
  point_fit <- if (has_point) standalone(point)
  if (!log_variance_fit_ok(point_fit)) point_fit <- NULL
  point_warm <- point_fit$warm_start
  spec_id <- lv_set_spec_id(list(
    control = control,
    ppml_spec = ppml$metadata$spec_id, logols_start = logols_coef,
    normal_gap = lv_set_normal_gap(), point = if (has_point) point else "null"
  ))
  list(
    metadata = list(
      estimator = "harvey", target_functional = "theta_var_gaussian",
      sample_id = sample$sample_id, smoothness = "smooth", response_scale = "variance",
      response_scale_value = 1, spec_id = spec_id,
      cold_start_rtol = control$cold_start_rtol
    ),
    coef_labels = colnames(x_mat), theta_labels = colnames(w2),
    point_fit = point_fit,
    fit_at_b = function(b, start = NULL, phase = NULL) {
      warm <- if (!is.null(start)) start else point_warm
      best <- lv_set_failure("nonconvergence", "start_policy_exhausted")
      if (!is.null(warm)) best <- fit_b(b, warm, list(), FALSE)
      if (!log_variance_fit_ok(best)) {
        at_b <- tryCatch(ppml$fit_at_b(b), error = function(e) NULL)
        if (log_variance_fit_ok(at_b)) {
          attempt <- fit_b(b, at_b$coef, list(), FALSE)
          if (log_variance_fit_ok(attempt)) best <- attempt
        }
      }
      if (!log_variance_fit_ok(best)) best <- standalone(b)
      best
    },
    jacobian_at_b = function(b, fit = NULL) {
      lv_set_axis(b, colnames(w2), "b")
      if (is.null(fit)) {
        return(NULL)
      }
      lv_set_harvey_jacobian(fit, b, w1, w2, x_mat, control)
    },
    precheck = function(quadratic, theta_table) {
      list(unresolved = lv_set_recession_self_test(x_mat, control))
    }
  )
}
