lv_set_harvey_sets <- function(sample, context, path, bounds, tau_control, control,
                               ppml) {
  ctrl <- control$search
  harvey_control <- lv_set_harvey_control()
  x_mat <- sample$x_mat
  intercept_only <- function(y) c(log(mean(y)), rep(0, ncol(x_mat) - 1L))
  reference <- sample$ols_residuals^2
  anchor <- drop(sample$w1 - sample$w2 %*% context$anchor)^2
  bundle <- ppml$estimator$start_bundle
  starts <- c(
    list(ref_intercept = list(y = reference, start = intercept_only(reference))),
    if (!is.null(bundle)) list(anchor_ppml = list(y = anchor, start = bundle$coef_original)),
    list(anchor_intercept = list(y = anchor, start = intercept_only(anchor)))
  )
  failed <- precheck_harvey_starts(starts, x_mat)
  recoverable <- failed %in% c(
    "nonfinite_start_eval", "nonpositive_mu", "nonfinite_info", "proposal_nonfinite"
  )
  fatal <- !is.na(failed) & !recoverable
  if (any(fatal)) {
    lv_set_stop(
      "The Harvey stability precheck failed: ",
      paste(names(failed)[fatal], failed[fatal], collapse = ", "), "."
    )
  }
  if (any(recoverable)) {
    message(
      "The Harvey stability precheck reported: ",
      paste(names(failed)[recoverable], failed[recoverable], collapse = ", "),
      ". Continuing with the existing fitter's start recovery and line search."
    )
  }
  # the log-OLS coefficients at the OLS residuals, the naive two-step fit
  lv_set_assert(all(is.finite(log(reference))))
  logols_coef <- stats::lm.fit(x_mat, log(reference))$coefficients
  map_obj <- lv_set_harvey_estimator(sample,
    point = context$point,
    ppml = ppml$estimator, logols_coef = logols_coef, control = harvey_control
  )
  # no set is searched unless the Harvey fit at the OLS residuals exists
  reference_fit <- lv_set_harvey_fitter(x_mat, harvey_control)(reference)
  if (!log_variance_fit_ok(reference_fit)) {
    lv_set_stop(
      "The Harvey fit at the OLS residuals failed (", reference_fit$fit_status, "/",
      reference_fit$diagnostics$error_class, ")."
    )
  }
  cache <- new.env(parent = emptyenv())
  primary <- lv_set_display_map(
    map_obj, path, bounds$theta, tau_control$display,
    context$seed, ctrl$PRIMARY_GRID_CAP, ctrl$PRIMARY_FIT_BUDGET, cache, control
  )
  # the same boxes searched again from more starts, over a fresh cache
  audit <- lv_set_audit_run(map_obj, path, bounds$theta, tau_control$display,
    context$seed, ctrl$PRIMARY_GRID_CAP, ctrl$SENSITIVITY_FIT_BUDGET,
    control = control
  )
  reconciled <- lv_set_audit_apply(primary, audit, control)
  list(
    key = "harvey", estimator = map_obj, sample = sample, seed = context$seed,
    point = context$point, cache = cache, grid_cap = ctrl$PRIMARY_GRID_CAP,
    results = reconciled$results, primary = primary,
    audit = reconciled$audit, ppml = ppml, taus = tau_control$display,
    stability_precheck = list(passed = all(is.na(failed)), reasons = failed)
  )
}
