lv_set_polish <- function(quadratic, direction, start, guard_scale, fn, gr, control) {
  if (!is.finite(guard_scale)) guard_scale <- 1
  sign_mult <- if (direction == "min") 1 else -1
  dimension <- ncol(quadratic$A_i[[1L]])
  delta <- tryCatch(profile_theta_scale(quadratic), hetid_error_solver = function(e) NULL)
  bounds <- if (!is.null(delta)) {
    tryCatch(profile_scaled_bounds(delta, control$sets$SOLVER_BOXES[[1L]], dimension),
      hetid_error_solver = function(e) NULL
    )
  }
  if (is.null(bounds)) {
    return(list(bound = NULL, par = NULL, suspect = FALSE))
  }
  result <- profile_solve_checked(quadratic, dimension, start,
    objective = function(b) sign_mult * fn(b),
    gradient = function(b) sign_mult * gr(b),
    lower = bounds$lower, upper = bounds$upper,
    objective_scale = "none", control = control$sets, delta = delta
  )
  out <- function(bound, par, suspect) list(bound = bound, par = par, suspect = suspect)
  if (any(!is.finite(result$theta))) {
    return(out(NULL, NULL, FALSE))
  }
  residual <- result$feasibility_residual
  if (!is.finite(residual) || residual > control$sets$FEASIBILITY_TOLERANCE) {
    return(out(NULL, result$theta, FALSE))
  }
  bound <- fn(result$theta)
  if (!is.finite(bound) ||
    abs(bound) > control$search$POLISH_BLOW_FACTOR * max(1, guard_scale)) {
    return(out(NULL, result$theta, TRUE))
  }
  out(bound, result$theta, FALSE)
}

lv_set_objective <- function(estimator, j, evaluate, budget_hit) {
  force(j)
  objective <- if (!is.null(estimator$coef_objective)) {
    estimator$coef_objective(j)
  } else {
    list(
      fn = function(b) {
        fit <- evaluate(b, phase = "polish")
        if (log_variance_fit_ok(fit)) unname(fit$coef[[j]]) else NaN
      },
      gr = function(b) {
        fit <- evaluate(b, phase = "polish")
        if (log_variance_fit_ok(fit)) {
          jacobian <- lv_set_checked_jacobian(estimator, b, fit)
          stats::setNames(jacobian[j, ], colnames(jacobian))
        } else {
          rep(NaN, length(b))
        }
      }
    )
  }
  assert_bad_argument_ok(is.list(objective) && is.function(objective$fn) &&
    is.function(objective$gr), "coef_objective must supply fn and gr", "estimator")
  assert_bad_argument_ok(
    is.null(objective$admit) || is.function(objective$admit),
    "objective admission must be a function", "estimator"
  )
  list(
    admit = objective$admit,
    fn = lv_set_guard_callback(objective$fn, budget_hit, NULL),
    gr = lv_set_guard_callback(objective$gr, budget_hit, estimator$theta_labels)
  )
}

lv_set_box_escape <- function(arg, bounds) {
  if (is.null(arg) || anyNA(arg) || !all(is.finite(c(bounds$lower, bounds$upper)))) {
    return(NA_real_)
  }
  norms <- pmax(bounds$upper - bounds$lower, abs(bounds$lower), abs(bounds$upper), 1)
  max(pmax(bounds$lower - arg, arg - bounds$upper) / norms)
}

lv_set_result <- function(coefs, lower, upper, lower_status, upper_status,
                          lower_source, upper_source, arg_lower, arg_upper, metadata,
                          tau, quadratic, omega, n_failed, n_feasible, diagnostics) {
  statuses <- c("bounded", "unbounded", "unreliable")
  lv_set_assert(all(lower_status %in% statuses), all(upper_status %in% statuses))
  residual_at <- function(arg, value) {
    if (anyNA(arg) || !is.finite(value)) NA_real_ else profile_residual(quadratic, arg, omega)
  }
  n <- length(coefs)
  schema <- data.frame(
    coef = coefs, lower = lower, upper = upper,
    lower_status = lower_status, upper_status = upper_status,
    fit_failure_count = rep(n_failed, n),
    lower_constraint_residual = vapply(seq_len(n), function(j) {
      residual_at(arg_lower[j, ], lower[j])
    }, numeric(1)),
    upper_constraint_residual = vapply(seq_len(n), function(j) {
      residual_at(arg_upper[j, ], upper[j])
    }, numeric(1)),
    estimator = metadata$estimator, target_functional = metadata$target_functional,
    sample_id = metadata$sample_id, tau = tau,
    lower_source = lower_source, upper_source = upper_source,
    row.names = NULL, stringsAsFactors = FALSE
  )
  schema$arg_lower <- I(lapply(seq_len(n), function(j) arg_lower[j, ]))
  schema$arg_upper <- I(lapply(seq_len(n), function(j) arg_upper[j, ]))
  list(schema = schema, n_feasible = n_feasible, diagnostics = diagnostics)
}

lv_set_result_closed <- function(coefs, status, metadata, tau, quadratic, omega,
                                 n_failed, n_feasible, diagnostics) {
  n <- length(coefs)
  missing_arg <- matrix(NA_real_, n, length(quadratic$b_i[[1L]]))
  lv_set_result(
    coefs, rep(NA_real_, n), rep(NA_real_, n), rep(status, n),
    rep(status, n), rep(NA_character_, n), rep(NA_character_, n), missing_arg, missing_arg,
    metadata, tau, quadratic, omega, n_failed, n_feasible, diagnostics
  )
}

lv_set_cold_check <- function(metadata, coefs, lower, upper, arg_lower, arg_upper,
                              lower_open, upper_open, lower_bad, upper_bad, evaluate, control) {
  rtol <- metadata$cold_start_rtol
  if (is.null(rtol)) rtol <- control$search$COLD_START_RTOL_FALLBACK
  records <- list()
  for (j in seq_along(coefs)) {
    for (side in c("min", "max")) {
      is_lower <- side == "min"
      value <- if (is_lower) lower[j] else upper[j]
      arg <- if (is_lower) arg_lower[j, ] else arg_upper[j, ]
      skip <- if (is_lower) lower_open[j] || lower_bad[j] else upper_open[j] || upper_bad[j]
      record <- lv_set_cold_side(skip, value, arg, coefs[j], side, j, evaluate, rtol)
      if (is.null(record)) next
      if (is_lower) lower_bad[j] <- TRUE else upper_bad[j] <- TRUE
      records[[length(records) + 1L]] <- record
    }
  }
  list(records = records, lower_bad = lower_bad, upper_bad = upper_bad)
}

lv_set_cold_side <- function(skip, value, arg, label, side, j, evaluate, rtol) {
  if (skip || !is.finite(value) || anyNA(arg)) {
    return(NULL)
  }
  fit <- evaluate(arg, phase = "cold_start", start = NULL, use_cache = FALSE)
  cold <- if (log_variance_fit_ok(fit)) unname(fit$coef[[j]]) else NaN
  if (is.finite(cold) && abs(cold - value) <= rtol * max(1, abs(value))) {
    return(NULL)
  }
  list(coef = label, side = side, value = value, cold_value = cold)
}
