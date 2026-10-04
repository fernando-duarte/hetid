lv_set_assert <- function(...) {
  values <- list(...)
  for (i in seq_along(values)) {
    value <- values[[i]]
    msg <- names(values)[i]
    if (is.null(msg) || is.na(msg) || !nzchar(msg)) {
      msg <- "the log-variance search contract is not satisfied"
    }
    assert_bad_argument_ok(is.logical(value) && !anyNA(value) && all(value), msg)
  }
  invisible(TRUE)
}

lv_set_stop <- function(...) stop_hetid(paste0(..., collapse = ""))

lv_set_solver_control <- function() {
  c(QUADRATIC_PROFILE_CONTROL, list(GRID_POINTS_LIMIT = HETID_CONSTANTS$GRID_POINTS_LIMIT))
}

lv_set_method <- function(method) {
  tryCatch(match.arg(method, c("ppml", "harvey", "logols")),
    error = function(e) stop_bad_argument(conditionMessage(e), "method")
  )
}

lv_set_fixed_rng <- function(code) {
  with_rng_scope(code, kind = c("Mersenne-Twister", "Inversion", "Rejection"))
}

lv_set_point_feasible <- function(quadratic, theta) {
  !anyNA(theta) && max(make_system_checker(quadratic)(theta)) <= 0
}

lv_set_quadratic <- function(path, tau) {
  path$quadratics[[profile_tau_key(tau)]]
}

lv_set_hash <- function(value) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(value, path, version = 3L)
  unname(tools::md5sum(path))
}

lv_set_positive <- function(value, arg, integer = FALSE, infinite = FALSE) {
  valid <- is.numeric(value) && length(value) == 1L && !is.na(value) && value > 0
  valid <- valid && (is.finite(value) || infinite)
  if (valid && integer && is.finite(value)) valid <- value == floor(value)
  assert_bad_argument_ok(valid, paste(arg, "must be a positive scalar"), arg)
}

lv_set_validate_budget_limit <- function(max_fit_evals) {
  assert_bad_argument_ok(
    is.numeric(max_fit_evals) && length(max_fit_evals) == 1L &&
      !is.na(max_fit_evals) && max_fit_evals >= 0 &&
      (is.infinite(max_fit_evals) || max_fit_evals == floor(max_fit_evals)),
    "max_fit_evals must be a nonnegative integer or Inf", "max_fit_evals"
  )
}

lv_set_validate_search <- function(estimator, quadratic, theta_table, seed,
                                   max_grid_points, max_fit_evals, starts, control) {
  quadratic_validate_system(quadratic)
  lv_set_validate_control(control)
  dimension <- length(quadratic$b_i[[1L]])
  assert_bad_argument_ok(
    is.data.frame(theta_table) && nrow(theta_table) == dimension &&
      all(c("coef", "status", "outer_lower", "outer_upper") %in% names(theta_table)),
    "theta_table must carry one containing-bound row per parameter", "theta_table"
  )
  lv_set_validate_estimator(estimator)
  lv_set_labels(theta_table$coef, "theta_table$coef")
  assert_bad_argument_ok(
    is.character(theta_table$status) && !anyNA(theta_table$status) &&
      all(theta_table$status %in% c("bounded", "unbounded", "unreliable")),
    "theta_table has invalid statuses", "theta_table"
  )
  if (!is.null(estimator$theta_labels)) {
    assert_bad_argument_ok(
      identical(estimator$theta_labels, theta_table$coef),
      "estimator and theta_table axes must agree", "theta_table"
    )
  }
  if (!is.null(seed)) lv_set_axis(seed, theta_table$coef, "seed")
  if (!is.null(max_grid_points)) lv_set_positive(max_grid_points, "max_grid_points", TRUE)
  lv_set_validate_budget_limit(max_fit_evals)
  if (!is.null(starts)) lv_set_positive(starts, "starts_per_side", TRUE)
  invisible(TRUE)
}

lv_set_validate_estimator <- function(estimator) {
  assert_bad_argument_ok(is.list(estimator), "estimator must be a map list", "estimator")
  assert_bad_argument_ok(
    is.null(estimator$analyze_domain),
    "use explicit precheck and sides hooks for this search protocol", "estimator"
  )
  lv_set_labels(estimator$coef_labels, "estimator$coef_labels")
  meta <- estimator$metadata
  assert_bad_argument_ok(is.list(meta), "estimator metadata must be a list", "estimator")
  for (name in c("estimator", "target_functional", "sample_id", "spec_id", "smoothness")) {
    value <- meta[[name]]
    assert_bad_argument_ok(is.character(value) && length(value) == 1L &&
      !is.na(value) && nzchar(value), paste("invalid estimator metadata", name), "estimator")
  }
  invisible(TRUE)
}

lv_set_validate_control <- function(control) {
  assert_bad_argument_ok(is.list(control), "control must be a list", "control")
  assert_bad_argument_ok(is.list(control$sets), "sets control must be a list", "control")
  assert_bad_argument_ok(is.list(control$search), "search control must be a list", "control")
  validate_profile_control(control$sets)
  limit <- control$sets$GRID_POINTS_LIMIT
  assert_bad_argument_ok(
    is.numeric(limit) && !is.complex(limit) && is.null(dim(limit)),
    "GRID_POINTS_LIMIT must be a real numeric scalar", "GRID_POINTS_LIMIT"
  )
  lv_set_positive(limit, "GRID_POINTS_LIMIT", integer = TRUE)
  defaults <- lv_set_search_control()
  assert_bad_argument_ok(
    identical(names(control$search), names(defaults)),
    "search controls must have the default names and order", "control"
  )
  for (name in setdiff(names(defaults), "COLD_START_CHECK")) {
    lv_set_positive(control$search[[name]], name, is.integer(defaults[[name]]))
  }
  assert_flag(control$search$COLD_START_CHECK, "COLD_START_CHECK")
  invisible(TRUE)
}

lv_set_validate_harvey_start <- function(ppml, sample, logols_coef) {
  assert_bad_argument_ok(
    is.list(ppml) && is.list(ppml$metadata) && identical(ppml$metadata$estimator, "ppml") &&
      identical(ppml$metadata$sample_id, sample$sample_id),
    "ppml must use the same sample", "ppml"
  )
  assert_bad_argument_ok(
    is.numeric(logols_coef) &&
      identical(names(logols_coef), colnames(sample$x_mat)) && all(is.finite(logols_coef)),
    "logols_coef must match the design columns", "logols_coef"
  )
  invisible(TRUE)
}
