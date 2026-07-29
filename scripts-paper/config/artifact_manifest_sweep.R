# Artifact records for the fitted-volatility slack sweep, generated from the tau
# contract. Sourced from artifact_manifest_data.R between the literal spec
# vectors and the manifest assembly, and extends both vectors in place so the
# swept taus stay a single source of truth: adding one to the contract creates
# its figure record, and no manifest row can survive a tau being dropped.
#
# The producer iterates the primary fitted-volatility estimators from the
# logvar estimator config, which is not loaded this early; the estimator ids are
# named here instead, and a mismatch fails fast when the driver looks up a
# variant path that no record defines.

.artifact_producers["ac"] <-
  "log_variance/figures/fitted_volatility/run_tau_sweep.R"
.sweep_estimators <- c("ppml", "harvey")
# scalar format() per tau: formatting the vector would pad 0.1 to "0.10" and
# rename the figure that the sweep actually writes
.sweep_tails <- c(
  vapply(
    PAPER_ANALYSIS_CONTRACT$tau$fitted_volatility_sweep,
    function(tau) sprintf("tau%s", sub(".", "p", format(tau), fixed = TRUE)),
    character(1)
  ),
  "combined", "combined_log",
  # normalized endpoint panels derived from the combined one, one per side
  "combined_lower_normalized", "combined_upper_normalized"
)
.sweep_grid <- expand.grid(
  tail = .sweep_tails,
  estimator = .sweep_estimators,
  stringsAsFactors = FALSE
)
.sweep_ids <- paste0(
  .sweep_grid$estimator, "_fitted_vol_", .sweep_grid$tail
)
.sweep_basenames <- sprintf(
  "log_var_eq_fitted_volatility_%s_%s.svg",
  .sweep_grid$estimator,
  sub("^combined", "tau_combined", .sweep_grid$tail)
)
.artifact_specs <- c(
  .artifact_specs,
  sprintf("%s|%s|3|ac|B|r", .sweep_ids, .sweep_basenames)
)
.artifact_variant_specs <- c(
  .artifact_variant_specs,
  sprintf(
    "%s|fitted_volatility_sweep|%s",
    .sweep_ids,
    paste(.sweep_grid$estimator, .sweep_grid$tail, sep = "_")
  )
)
rm(
  .sweep_estimators, .sweep_tails, .sweep_grid, .sweep_ids, .sweep_basenames
)
