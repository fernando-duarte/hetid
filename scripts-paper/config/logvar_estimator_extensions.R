# Extension log-variance estimators: registry entries for estimators outside
# the PPML/Harvey pair. Sourced by logvar_estimators.R after
# .paper_logvar_spec and the shared display list are defined.

PAPER_LOGVAR_ESTIMATOR_EXTENSIONS <- list(
  lad = .paper_logvar_spec(
    key = "lad",
    display_name = "LAD",
    result_object = "log_var_eq_lad",
    builder = "logvar_lad_estimator",
    response_scale = PAPER_LOGVAR_RESPONSE_SCALES[["log"]],
    display = list(
      title_quantity =
        "fitted conditional residual scale (median)",
      y_label = paste(
        "Conditional median |consumption-growth residual|",
        "(percentage points)"
      )
    ),
    artifacts = c(
      table = "structural_var_estimators_table",
      bounds = "lad_bounds_figure",
      fitted_volatility =
        "lad_fitted_volatility_figure"
    ),
    capabilities = c(
      "bounds_by_tau", "table", "fitted_volatility"
    ),
    budget_policy = "lad_control"
  ),
  # regularized log projections of squared residuals (hetid's
  # evaluate_log_projection methods; the key is the package method): in the
  # set bootstrap, with every output the engine-envelope estimators get
  log_plus = .paper_logvar_spec(
    key = "log_plus",
    display_name = "Regularized log-OLS (additive)",
    result_object = "log_var_eq_log_plus",
    builder = "logvar_log_projection_estimator",
    response_scale = PAPER_LOGVAR_RESPONSE_SCALES[["log"]],
    display = PAPER_LOGVAR_VOLATILITY_DISPLAY,
    artifacts = c(
      table = "structural_var_estimators_table",
      bounds = "log_plus_bounds_figure",
      fitted_volatility = "log_plus_fitted_volatility_figure"
    ),
    capabilities = c(
      "bounds_by_tau", "table", "set_bootstrap", "fitted_volatility",
      "engine_envelope", "log_projection"
    ),
    budget_policy = "log_projection_control"
  ),
  log_fuller = .paper_logvar_spec(
    key = "log_fuller",
    display_name = "Regularized log-OLS (Fuller)",
    result_object = "log_var_eq_log_fuller",
    builder = "logvar_log_projection_estimator",
    response_scale = PAPER_LOGVAR_RESPONSE_SCALES[["log"]],
    display = PAPER_LOGVAR_VOLATILITY_DISPLAY,
    artifacts = c(
      table = "structural_var_estimators_table",
      bounds = "log_fuller_bounds_figure",
      fitted_volatility = "log_fuller_fitted_volatility_figure"
    ),
    capabilities = c(
      "bounds_by_tau", "table", "set_bootstrap", "fitted_volatility",
      "engine_envelope", "log_projection"
    ),
    budget_policy = "log_projection_control"
  )
)
