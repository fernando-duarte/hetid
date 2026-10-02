# Artifact records for the regularized log projections (log_plus, log_fuller):
# their bounds-by-tau and fitted-volatility figures, the endpoint-diagnostics
# CSV, and the tuning appendix table. Sourced from artifact_manifest_data.R
# between the literal spec vectors and the manifest assembly; extends the spec,
# producer, and variant vectors in place, as the sweep records do.

# [<- would silently reassign a code already spoken for
stopifnot(
  "log-projection producer codes are already claimed" =
    !any(c("ag", "ah") %in% names(.artifact_producers))
)
.artifact_producers["ag"] <-
  "log_variance/tables/render_log_projection_tuning_table.R"
.artifact_producers["ah"] <- "log_variance/estimators/log_projection/run_sets.R"
.artifact_specs <- c(
  .artifact_specs,
  "log_plus_bounds_figure|log_var_eq_bounds_tau_log_plus.svg|3|m|B|r",
  "log_fuller_bounds_figure|log_var_eq_bounds_tau_log_fuller.svg|3|m|B|r",
  "log_plus_fitted_volatility_figure|log_var_eq_fitted_volatility_log_plus.svg|3|n|B|r",
  "log_fuller_fitted_volatility_figure|log_var_eq_fitted_volatility_log_fuller.svg|3|n|B|r",
  "log_projection_endpoint_diagnostics|log_var_eq_log_projection_endpoints.csv|6|ah|M|r",
  "log_projection_tuning_table|log_var_eq_log_projection_tuning.tex|2|ag|B|r",
  "log_projection_tuning_standalone_tex|log_var_eq_log_projection_tuning_standalone.tex|2|ag|B|r",
  "log_projection_tuning_standalone_pdf|log_var_eq_log_projection_tuning_standalone.pdf|2|ag|B|r"
)
.artifact_variant_specs <- c(
  .artifact_variant_specs,
  "log_plus_bounds_figure|logvar_bounds_tau|log_plus",
  "log_fuller_bounds_figure|logvar_bounds_tau|log_fuller",
  "log_plus_fitted_volatility_figure|fitted_volatility|log_plus",
  "log_fuller_fitted_volatility_figure|fitted_volatility|log_fuller"
)
