# Extension log-variance estimators: registry entries for estimators outside
# the primary set bootstrap. Sourced by logvar_estimators.R after
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
  )
)
