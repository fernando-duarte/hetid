# Estimator adapter for date-indexed fitted log variance. The source estimator
# still owns every b-indexed response fit; this layer projects its coefficient
# vector and implicit Jacobian onto the dated variance-equation design. It is
# estimator-agnostic across accepted variance-scale estimators that implement
# the shared logvar engine contract.

logvar_fitted_vol_call <- function(est, b, start = NULL, phase = NULL) {
  if ("phase" %in% names(formals(est$fit_at_b))) {
    est$fit_at_b(b, start = start, phase = phase)
  } else {
    est$fit_at_b(b, start = start)
  }
}

logvar_fitted_vol_source <- function(est, source_cache = NULL) {
  if (is.null(source_cache)) {
    return(list(
      fit = function(b, start = NULL, phase = NULL) {
        logvar_fitted_vol_call(est, b, start = start, phase = phase)
      },
      budget = NULL
    ))
  }
  logvar_cache_bind(source_cache, est$metadata)
  source_budget <- logvar_budget_state()
  evaluate <- logvar_make_evaluator(est, source_cache, source_budget)
  list(
    fit = function(b, start = NULL, phase = "scan") {
      cold <- identical(phase, "cold_start")
      evaluate(
        b,
        phase = phase, start = if (cold) NULL else start,
        use_cache = !cold
      )
    },
    budget = source_budget
  )
}

logvar_fitted_vol_domain <- function(est, labels) {
  domain <- est$analyze_domain
  if (is.null(domain)) {
    return(list())
  }
  source_sides <- domain$sides
  if (is.null(source_sides)) {
    return(domain)
  }
  domain$sides <- function(qs, b_tab, scan, ctx) {
    sides <- logvar_call_sides(source_sides, qs, b_tab, scan, ctx)
    n <- length(labels)
    valid <- is.list(sides) &&
      length(sides$lower_unbounded) == n &&
      length(sides$upper_unbounded) == n
    if (!valid) {
      closure <- if (is.list(sides)) sides$closure_diagnostics else NULL
      return(list(
        lower_unbounded = stats::setNames(rep(FALSE, n), labels),
        upper_unbounded = stats::setNames(rep(FALSE, n), labels),
        unresolved_endpoints = c(
          paste(labels, "min", sep = ":"),
          paste(labels, "max", sep = ":")
        ),
        closure_diagnostics = closure,
        info = list(method = "source-side-hook-axis-mismatch")
      ))
    }
    lower_na <- is.na(sides$lower_unbounded)
    upper_na <- is.na(sides$upper_unbounded)
    sides$lower_unbounded <- stats::setNames(
      as.logical(sides$lower_unbounded) & !lower_na, labels
    )
    sides$upper_unbounded <- stats::setNames(
      as.logical(sides$upper_unbounded) & !upper_na, labels
    )
    lower_unresolved <- if (any(lower_na)) {
      paste(labels[lower_na], "min", sep = ":")
    } else {
      character(0)
    }
    upper_unresolved <- if (any(upper_na)) {
      paste(labels[upper_na], "max", sep = ":")
    } else {
      character(0)
    }
    sides$unresolved_endpoints <- c(
      sides$unresolved_endpoints, lower_unresolved, upper_unresolved
    )
    sides
  }
  domain
}

logvar_fitted_vol_adapter <- function(est, x_mat, labels,
                                      source_cache = NULL,
                                      expected_sample_id = NULL) {
  stopifnot(
    is.matrix(x_mat), is.numeric(x_mat), all(is.finite(x_mat)),
    length(labels) == nrow(x_mat), !anyNA(labels), !anyDuplicated(labels),
    identical(colnames(x_mat), est$coef_labels),
    identical(est$metadata$response_scale, "variance"),
    is.null(expected_sample_id) ||
      identical(est$metadata$sample_id, expected_sample_id)
  )
  source <- logvar_fitted_vol_source(est, source_cache)
  meta <- est$metadata
  meta$target_functional <- "fitted_log_variance_path"
  meta$spec_id <- paste(
    meta$spec_id, "derived_functional=fitted-log-variance-path-v1",
    sep = "\n"
  )
  project_fit <- function(b, start = NULL, phase = "scan") {
    fit <- source$fit(b, start = start, phase = phase)
    fit$functional_source_coef <- fit$coef
    if (!logvar_fit_ok(fit)) {
      return(fit)
    }
    eta <- drop(x_mat %*% fit$functional_source_coef)
    if (length(eta) != nrow(x_mat) || any(!is.finite(eta))) {
      fit$fit_status <- "nonfinite_fitted_log_variance"
      fit$converged <- FALSE
      fit$coef <- stats::setNames(rep(NA_real_, nrow(x_mat)), labels)
      return(fit)
    }
    fit$coef <- stats::setNames(eta, labels)
    fit
  }
  project_jacobian <- NULL
  if (!is.null(est$jacobian_at_b)) {
    project_jacobian <- function(b, fit = NULL) {
      if (is.null(fit) || is.null(fit$functional_source_coef)) {
        return(NULL)
      }
      source_fit <- fit
      source_fit$coef <- fit$functional_source_coef
      jac <- est$jacobian_at_b(b, source_fit)
      if (is.null(jac)) {
        return(NULL)
      }
      out <- x_mat %*% jac
      rownames(out) <- labels
      out
    }
  }
  domain <- logvar_fitted_vol_domain(est, labels)
  list(
    metadata = meta, coef_labels = labels, fit_at_b = project_fit,
    jacobian_at_b = project_jacobian, analyze_domain = domain,
    source_budget_state = source$budget
  )
}
