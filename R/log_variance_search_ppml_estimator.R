lv_set_ppml_control <- function() {
  list(
    ESTIMATOR_VERSION = "ppml-v1",
    fit = c(
      LOG_VARIANCE_CONTROL[c(
        "GLM_EPSILON", "GLM_MAXIT", "SCORE_TOLERANCE",
        "RANK_TOLERANCE", "RCOND_TOLERANCE"
      )],
      list(
        START_ORDER = c("supplied", "fallback", "intercept_only", "glm_default"),
        SKIP_NONFINITE_STARTS = TRUE
      )
    ),
    JACOBIAN_RCOND_TOL = 1e-10,
    COLD_START_RTOL = 1e-6,
    MORTON_BITS = 17L,
    PILOT_OVERFLOW_MARGIN = 5,
    PILOT_CONDITION_LIMIT = 1e10,
    PILOT_GRID_POINTS = 10L
  )
}

lv_set_slim_fit <- function(fit) {
  diagnostics <- fit$diagnostics
  diagnostics$fit_control <- NULL
  lv_set_fit_result(
    coef = fit$coef, fit_status = fit$fit_status,
    converged = fit$converged, objective = fit$objective, score_norm = fit$score_norm,
    convergence_code = fit$convergence_code, warm_start = fit$warm_start,
    diagnostics = diagnostics
  )
}

lv_set_ppml_fitter <- function(x_mat, control = lv_set_ppml_control()) {
  lv_set_assert(
    is.matrix(x_mat), identical(colnames(x_mat)[[1L]], HETID_CONSTANTS$INTERCEPT_LABEL),
    all(x_mat[, 1L] == 1)
  )
  fitter <- make_log_variance_fitter(x_mat[, -1L, drop = FALSE], "ppml", control$fit)
  function(y, start = NULL, fallback_starts = list(), response_scale = 1) {
    lv_set_slim_fit(fitter(y, start, fallback_starts, response_scale))
  }
}

lv_set_ppml_jacobian <- function(fit, b, w1, w2, x_mat, response_scale, control) {
  if (!log_variance_fit_ok(fit)) {
    return(NULL)
  }
  mu <- exp(drop(x_mat %*% fit$warm_start))
  eps <- drop(w1 - w2 %*% b)
  information <- crossprod(x_mat, mu * x_mat)
  rhs <- crossprod(x_mat, (-2 / response_scale) * eps * w2)
  norms <- sqrt(colSums(mu * x_mat^2))
  if (!all(is.finite(norms)) || any(norms <= 0)) {
    return(NULL)
  }
  scaled <- information / tcrossprod(norms)
  if (!all(is.finite(scaled)) || rcond(scaled) < control$JACOBIAN_RCOND_TOL) {
    return(NULL)
  }
  chol_r <- tryCatch(chol(scaled), error = function(e) NULL)
  if (is.null(chol_r)) {
    return(NULL)
  }
  out <- backsolve(chol_r, forwardsolve(t(chol_r), rhs / norms)) / norms
  rownames(out) <- colnames(x_mat)
  out
}

lv_set_ppml_start_bundle <- function(fit, response_scale, source, b) {
  if (!log_variance_fit_ok(fit)) {
    return(NULL)
  }
  list(
    coef_original = fit$coef, coef_scaled = fit$warm_start,
    response_scale = response_scale, source = source, b = as.numeric(b)
  )
}

lv_set_ppml_pilot <- function(sample, anchor, grid_points,
                              control = lv_set_ppml_control()) {
  fitter <- lv_set_ppml_fitter(sample$x_mat, control)
  guard <- log(.Machine$double.xmax) - control$PILOT_OVERFLOW_MARGIN
  triggers <- function(b) {
    fit <- fitter(drop(sample$w1 - sample$w2 %*% b)^2)
    condition <- fit$diagnostics$condition_weighted_scaled
    bad_condition <- length(condition) != 1L || !is.finite(condition) ||
      condition > control$PILOT_CONDITION_LIMIT
    bad_eta <- !is.null(fit$warm_start) &&
      max(drop(sample$x_mat %*% fit$warm_start)) > guard
    isTRUE(!isTRUE(fit$converged) || bad_condition || bad_eta)
  }
  n_grid <- if (is.null(grid_points)) 0L else min(control$PILOT_GRID_POINTS, nrow(grid_points))
  triggered <- c(triggers(anchor), vapply(seq_len(n_grid), function(i) {
    triggers(grid_points[i, ])
  }, logical(1)))
  response_scale <- 1
  if (any(triggered)) {
    response <- drop(sample$w1 - sample$w2 %*% anchor)^2
    positive <- response[response > 0]
    if (!length(positive)) {
      lv_set_stop("The PPML response needs a scale but the anchor response has no positive value.")
    }
    response_scale <- stats::median(positive)
  }
  list(
    response_scale = response_scale, n_fits = length(triggered),
    n_triggered = sum(triggered)
  )
}

lv_set_ppml_estimator <- function(sample, point = NULL, anchor, anchor_source,
                                  response_scale = 1,
                                  control = lv_set_ppml_control()) {
  w1 <- sample$w1
  w2 <- sample$w2
  x_mat <- sample$x_mat
  fit_response <- lv_set_ppml_fitter(x_mat, control)
  fit_b <- function(b, start = NULL, fallback_starts = list()) {
    lv_set_axis(b, colnames(w2), "b")
    eps <- drop(w1 - w2 %*% b)
    fit <- fit_response(eps^2, start, fallback_starts, response_scale)
    fit$diagnostics$min_abs_eps <- min(abs(eps))
    fit
  }
  if (!any(drop(w1 - w2 %*% anchor)^2 > 0)) {
    lv_set_stop("The PPML anchor response has no positive value.")
  }
  anchor_bundle <- lv_set_ppml_start_bundle(
    fit_b(anchor), response_scale,
    anchor_source, anchor
  )
  has_point <- !is.null(point) && !anyNA(point)
  start_bundle <- if (has_point) {
    lv_set_ppml_start_bundle(fit_b(point), response_scale, "tau_zero_point", point)
  }
  fallback <- if (!is.null(start_bundle)) {
    list(start_bundle$coef_scaled)
  } else if (!is.null(anchor_bundle)) {
    list(anchor_bundle$coef_scaled)
  } else {
    list()
  }
  spec_id <- lv_set_spec_id(list(
    control = control, response_scale = response_scale,
    point = if (has_point) point else "null", anchor = anchor,
    anchor_source = anchor_source,
    branch = if (!is.null(start_bundle)) "point" else "anchor"
  ))
  list(
    metadata = list(
      estimator = "ppml", target_functional = "theta_var",
      sample_id = sample$sample_id, smoothness = "smooth", response_scale = "variance",
      response_scale_value = response_scale, spec_id = spec_id,
      cold_start_rtol = control$COLD_START_RTOL
    ),
    coef_labels = colnames(x_mat), theta_labels = colnames(w2),
    start_bundle = start_bundle,
    anchor_bundle = anchor_bundle,
    fit_at_b = function(b, start = NULL, phase = NULL) fit_b(b, start, fallback),
    jacobian_at_b = function(b, fit = NULL) {
      lv_set_axis(b, colnames(w2), "b")
      if (is.null(fit)) {
        return(NULL)
      }
      lv_set_ppml_jacobian(fit, b, w1, w2, x_mat, response_scale, control)
    }
  )
}

lv_set_morton_id <- function() "morton-v1"
