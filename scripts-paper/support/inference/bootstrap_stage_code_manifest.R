BOOTSTRAP_STAGE_CODE_DIRECTORIES <- c(
  "support/identification",
  "log_variance/core",
  "log_variance/engine",
  "log_variance/estimators/ppml",
  "log_variance/estimators/harvey"
)

BOOTSTRAP_STAGE_CODE_FILES <- c(
  paste0("config/", c(
    "analysis_contract.R",
    "artifacts.R",
    "inference_search_control.R",
    "logvar_estimators.R",
    "logvar_estimator_access.R",
    "reporting.R"
  )),
  "support/runtime/core.R",
  "support/artifacts/typed_artifacts.R",
  paste0("support/statistics/", c(
    "bootstrap_and_stationarity.R",
    "normalizations.R",
    "mbb_rng_state.R",
    "mbb_index_family.R",
    "mbb_execution_core.R",
    "mbb_runner.R",
    "boot_freshness.R",
    "boot_cache.R"
  )),
  paste0("support/inference/", c(
    "bootstrap_stage_spec_assertions.R",
    "bootstrap_stage_logvar_contract.R",
    "bootstrap_stage_provenance_validation.R",
    "bootstrap_stage_cache_validation.R",
    "bootstrap_stage_execution.R",
    "bootstrap_stage_logvar_cache.R",
    "bootstrap_stage_mean_cache.R",
    "bootstrap_stage_provenance.R",
    "bootstrap_stage_result_inputs.R",
    "bootstrap_stage_mean_result_inputs.R",
    "bootstrap_stage_result_helpers.R"
  )),
  "support/reporting/inference.R",
  paste0("log_variance/inference/", c(
    "set_bootstrap_core.R",
    "set_bootstrap_draw.R",
    "set_bootstrap_builders.R",
    "set_bootstrap_gate.R",
    "set_bootstrap_artifacts.R",
    "set_envelope.R",
    "standard_error_estimators.R"
  )),
  "mean_equation/inference/boot_results.R",
  "inference/bootstrap_stage_specs.R",
  "inference/bootstrap_stage_draw.R",
  "inference/bootstrap_stage_cache.R",
  "inference/bootstrap_stage_results.R",
  "inference/run_bootstrap_stage.R"
)

bootstrap_stage_code_manifest <- function() {
  bootstrap_stage_manifest_expand(
    BOOTSTRAP_STAGE_CODE_DIRECTORIES,
    BOOTSTRAP_STAGE_CODE_FILES,
    paper_path
  )
}
