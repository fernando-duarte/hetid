# Single top-level invocation for the unified bootstrap stage.
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("config", "reporting.R"))
paper_source_once(paper_path("config", "logvar_estimators.R"))
paper_source_once(paper_path("support", "identification", "api.R"))
paper_source_once(paper_path(
  "support", "identification", "profile_solver_core.R"
))
paper_source_once(paper_path(
  "support", "identification", "profile_bounds_api.R"
))
paper_source_once(paper_path("support", "identification", "tau_star.R"))
paper_source_once(paper_path("support", "statistics", "api.R"))
paper_source_once(paper_path(
  "support", "artifacts", "typed_artifacts.R"
))
paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path(
  "support", "identification", "identified_set_inference.R"
))
paper_source_once(paper_path(
  "support", "identification", "identified_set_bootstrap.R"
))
paper_source_once(paper_path(
  "support", "identification", "identified_set_bootstrap_collect.R"
))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_cache_validation.R"
))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_provenance_validation.R"
))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_mean_cache.R"
))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_logvar_cache.R"
))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_code_manifest.R"
))
paper_source_once(paper_path(
  "mean_equation", "inference", "boot_results.R"
))
paper_source_once(paper_path(
  "log_variance", "inference", "set_envelope.R"
))
paper_source_once(paper_path(
  "log_variance", "inference", "set_bootstrap_core.R"
))
paper_source_once(paper_path(
  "log_variance", "inference", "set_bootstrap_draw.R"
))
paper_source_once(paper_path(
  "log_variance", "inference", "set_bootstrap_builders.R"
))
paper_source_once(paper_path(
  "log_variance", "inference", "set_bootstrap_gate.R"
))
paper_source_once(paper_path(
  "log_variance", "inference", "set_bootstrap_artifacts.R"
))
paper_source_once(paper_path(
  "inference", "bootstrap_stage_specs.R"
))
paper_source_once(paper_path(
  "inference", "bootstrap_stage_draw.R"
))
paper_source_once(paper_path(
  "inference", "bootstrap_stage_cache.R"
))
paper_source_once(paper_path(
  "inference", "bootstrap_stage_results.R"
))

run_bootstrap_stage <- function(
  mean_eq, logvar_eq, lag_pc, estimator_results,
  n_draws, seed, cores, mode, z_name, impose_null,
  grid_cap, fit_budget, cache_path
) {
  started_at <- Sys.time()
  stage_spec <- bootstrap_stage_spec(
    mean_eq, logvar_eq, lag_pc, estimator_results,
    n_draws, seed, z_name, impose_null, grid_cap, fit_budget
  )
  design <- stage_spec$design
  primary_family <- paper_mbb_index_family(
    design$primary$n_draws, stage_spec$frame$sample_size,
    design$primary$block_length, design$seed,
    design$primary$family
  )
  sensitivity_family <- paper_mbb_index_family(
    design$sensitivity$n_draws, stage_spec$frame$sample_size,
    design$sensitivity$block_length, design$seed,
    design$sensitivity$family
  )
  provenance <- function() {
    bootstrap_stage_provenance(
      stage_spec, primary_family, sensitivity_family
    )
  }
  dispatched <- bootstrap_stage_cached_or_run(
    cache_path, mode, stage_spec, provenance,
    function() {
      bootstrap_stage_candidate(
        stage_spec, primary_family, sensitivity_family, cores
      )
    }
  )
  bootstrap_stage_remove_legacy_caches(
    cache_path,
    function(value) {
      bootstrap_stage_cache_validate(
        value, stage_spec, provenance()
      )
    }
  )
  bootstrap_stage_results(
    dispatched$stage, stage_spec, mean_eq,
    estimator_results, dispatched$source,
    as.numeric(difftime(
      Sys.time(), started_at,
      units = "mins"
    ))
  )
}

.bootstrap_stage_estimators <- paper_logvar_estimator_ids(
  capability = "set_bootstrap", primary = TRUE
)
.bootstrap_stage_estimator_results <- stats::setNames(
  lapply(.bootstrap_stage_estimators, paper_logvar_result),
  .bootstrap_stage_estimators
)
.bootstrap_stage_output <- run_bootstrap_stage(
  set_id_mean_eq, log_var_eq, lag_asset_return_pc,
  .bootstrap_stage_estimator_results, boot_reps, boot_seed,
  boot_cores, PAPER_BOOT_MODE, z_col, impose_beta2r_null,
  logvar_boot_grid_cap, logvar_boot_fit_budget,
  artifact_path("bootstrap_stage_draws")
)
set_id_boot <- .bootstrap_stage_output$set_id_boot
log_var_eq_set_boot <- .bootstrap_stage_output$log_var_eq_set_boot
rm(
  .bootstrap_stage_estimators,
  .bootstrap_stage_estimator_results,
  .bootstrap_stage_output
)
