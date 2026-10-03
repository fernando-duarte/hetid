fitted_volatility_adapter <- function(estimator, x_mat, labels, source_cache) {
  lv_set_validate_estimator(estimator)
  lv_set_labels(labels, "labels")
  assert_bad_argument_ok(
    is.matrix(x_mat) && is.numeric(x_mat),
    "projection design must be a numeric matrix", "estimator"
  )
  assert_bad_argument_ok(
    all(c(
      all(is.finite(x_mat)), nrow(x_mat) == length(labels),
      identical(colnames(x_mat), estimator$coef_labels),
      identical(estimator$metadata$response_scale, "variance"), is.null(estimator$sides)
    )),
    "projection design must match a variance map without sides", "estimator"
  )
  lv_set_validate_cache(source_cache)
  source_budget <- lv_set_budget()
  lv_set_validate_budget(source_budget)
  lv_set_cache_bind(source_cache, estimator$metadata)
  source_fit <- lv_set_evaluator(estimator, source_cache, source_budget)
  metadata <- estimator$metadata
  metadata$target_functional <- "fitted_log_variance_path"
  metadata$spec_id <- paste(metadata$spec_id,
    "derived_functional=fitted-log-variance-path-v1",
    sep = "\n"
  )
  list(
    metadata = metadata, coef_labels = labels, theta_labels = estimator$theta_labels,
    source_budget = source_budget,
    fit_at_b = fitted_volatility_fit(source_fit, x_mat, labels),
    jacobian_at_b = fitted_volatility_jacobian(estimator, x_mat, labels),
    precheck = estimator$precheck
  )
}

fitted_volatility_fit <- function(source_fit, x_mat, labels) {
  function(b, start = NULL, phase = "scan") {
    cold <- identical(phase, "cold_start")
    fit <- source_fit(b, phase, start = if (cold) NULL else start, use_cache = !cold)
    fit$source_coef <- fit$coef
    if (!log_variance_fit_ok(fit)) {
      return(fit)
    }
    eta <- drop(x_mat %*% fit$source_coef)
    if (any(!is.finite(eta))) {
      fit$fit_status <- "nonfinite_fitted_log_variance"
      fit$converged <- FALSE
      fit$coef <- stats::setNames(rep(NA_real_, nrow(x_mat)), labels)
      return(fit)
    }
    fit$coef <- stats::setNames(eta, labels)
    fit
  }
}

fitted_volatility_jacobian <- function(estimator, x_mat, labels) {
  function(b, fit = NULL) {
    if (is.null(fit) || is.null(fit$source_coef)) {
      return(NULL)
    }
    fit$coef <- fit$source_coef
    jacobian <- estimator$jacobian_at_b(b, fit)
    if (is.null(jacobian)) {
      return(NULL)
    }
    lv_set_matrix_axes(jacobian, estimator$coef_labels, estimator$theta_labels, "Jacobian")
    out <- x_mat %*% jacobian
    rownames(out) <- labels
    out
  }
}
