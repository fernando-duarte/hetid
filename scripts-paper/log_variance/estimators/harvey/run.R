# Harvey consumes the primary-estimator budget policy registered in config.
logvar_harvey_grid_cap <-
  paper_logvar_budget("harvey", "grid_cap")
logvar_harvey_fit_budget <-
  paper_logvar_budget("harvey", "fit_budget")
logvar_harvey_sensitivity_fit_budget <-
  paper_logvar_budget("harvey", "sensitivity_fit_budget")
paper_source_once(paper_path("log_variance", "estimators", "harvey", "run_sets.R"))
paper_source_once(paper_path("log_variance", "estimators", "harvey", "standard_errors.R"))
# The dedicated Harvey table is not sourced here: it publishes a bootstrap
# tau = 0 statistic, so run_pipeline.R publishes the Harvey page after the
# bootstrap stage. Estimation and the analytic standard errors stay here,
# because the bootstrap stage reads both.
