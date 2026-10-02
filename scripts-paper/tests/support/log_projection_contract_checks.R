# Registry contract checks for the engine-envelope estimators. Sourced by
# test_statistics.R after analysis_contract_checks.R, which supplies check().

sweep_estimators <- sub(
  "_fitted_vol_combined$", "",
  artifact_manifest$id[grepl("_fitted_vol_combined$", artifact_manifest$id)]
)
check(
  "the sweep manifest covers exactly the engine-envelope estimators",
  setequal(sweep_estimators, paper_logvar_estimator_ids(capability = "engine_envelope")) &&
    !anyDuplicated(sweep_estimators)
)
