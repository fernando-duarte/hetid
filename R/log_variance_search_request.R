lv_set_request_id <- function(sample, quadratics, theta_tables, taus, point, control) {
  keys <- profile_tau_key(taus)
  lv_set_hash(list(
    sample_id = sample$sample_id, quadratics = quadratics[keys],
    theta_tables = theta_tables[keys], taus = taus, point = unname(point), control = control
  ))
}

lv_set_bind_request <- function(sets, id, point) {
  sets$request <- list(id = id, point = point, retained_point = sets$point)
  sets
}

lv_set_check_request <- function(sets, sample, quadratics, theta_tables, taus, point, control) {
  lv_set_validate_aggregate(sets)
  lv_set_validate_path(sample, quadratics, theta_tables, taus, point)
  id <- lv_set_request_id(sample, quadratics, theta_tables, taus, point, control)
  assert_bad_argument_ok(
    is.list(sets$request) && identical(sets$request$id, id) &&
      identical(sets$taus, taus) && identical(sets$request$retained_point, sets$point) &&
      identical(names(sets$results), profile_tau_key(taus)),
    "completed searches do not match the requested systems, tables, taus, point and control",
    "sets"
  )
  invisible(TRUE)
}

lv_set_validate_aggregate <- function(sets) {
  assert_bad_argument_ok(is.list(sets), "sets must be a completed aggregate list", "sets")
  assert_bad_argument_ok(
    is.list(sets$request) && is.list(sets$results),
    "sets must retain request and results lists", "sets"
  )
  lv_set_validate_estimator(sets$estimator)
  for (result in sets$results) {
    assert_bad_argument_ok(
      is.list(result) && is.data.frame(result$schema) && is.list(result$diagnostics),
      "each retained result must contain a schema data frame and diagnostics list", "sets"
    )
  }
  invisible(TRUE)
}

lv_set_build_map <- function(method, sample, path, bounds, tau_control, control, ppml) {
  if (method == "logols") {
    return(lv_set_logols_sets(sample, path, bounds, tau_control, control))
  }
  context <- lv_set_map_context(
    sample, path, bounds$theta, tau_control,
    control$search$PRIMARY_GRID_CAP, control
  )
  if (is.null(ppml)) ppml <- lv_set_ppml_sets(sample, context, path, bounds, tau_control, control)
  if (method == "ppml") {
    return(ppml)
  }
  assert_bad_argument_ok(
    !is.null(sample$ols_residuals),
    "the Harvey path requires benchmark OLS residuals", "sample"
  )
  lv_set_harvey_sets(sample, context, path, bounds, tau_control, control, ppml)
}
