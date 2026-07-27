# Family/variant labels for the manifest records that come in interchangeable
# flavours, so a consumer can ask for one by (family, variant) instead of
# hard-coding an id. Sourced from artifact_manifest_data.R before the manifest
# is assembled; every id here must name a record in .artifact_specs.

.artifact_variant_specs <- c(
  "log_ols_bounds_figure|logvar_bounds_tau|logols",
  "ppml_bounds_figure|logvar_bounds_tau|ppml",
  "harvey_bounds_figure|logvar_bounds_tau|harvey",
  "lad_bounds_figure|logvar_bounds_tau|lad",
  "egarch_bounds_figure|logvar_bounds_tau|egarch_x",
  "ppml_fitted_volatility_figure|fitted_volatility|ppml",
  "harvey_fitted_volatility_figure|fitted_volatility|harvey",
  "lad_fitted_volatility_figure|fitted_volatility|lad",
  "log_variance_panels_table|logvar_panels|conservative",
  "log_variance_inference_table|logvar_panels|inference",
  "structural_equation_inference_table|structural_equation|inference"
)
