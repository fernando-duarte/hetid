# Offline checks for the vol-equation set-endpoint bootstrap inference layer.
# A thin entrypoint mirroring the PPML suite: source the envelope module
# (which sources the paper-owned robust-scale and minimum-repetition functions),
# the paper-owned mean-equation support recipe (estimate_set_id_system /
# coef_interval_tables /
# tau_quadratic_system) and the log-var engine/estimator chain (map/engine/
# log-OLS/PPML/Harvey) the per-draw re-estimator drives, then the per-draw
# core modules themselves. Define the shared check() counter and source the
# branch-local collection, draw, gate, and characterization checks. Run from root:
#   Rscript scripts-paper/tests/inference/test_set_bootstrap.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("log_variance", "inference", "set_envelope.R"))
paper_source_once(paper_path("support", "identification", "api.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("support", "identification", "tau_star.R"))
paper_source_once(paper_path("support", "identification", "identified_set_bootstrap.R"))
paper_source_once(paper_path(
  "support", "identification", "identified_set_bootstrap_collect.R"
))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path("log_variance", "engine", "api.R"))
paper_source_once(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))
paper_source_once(paper_path("log_variance", "estimators", "ppml", "estimator.R"))
paper_source_once(paper_path("log_variance", "estimators", "harvey", "estimator.R"))
paper_source_once(paper_path("inference", "bootstrap_stage_specs.R"))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_logvar_contract.R"
))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_core.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_draw.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_builders.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_gate.R"))
paper_source_once(paper_path("inference", "bootstrap_stage_draw.R"))
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
  "support", "inference", "bootstrap_stage_execution.R"
))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_provenance.R"
))
paper_source_once(paper_path("inference", "bootstrap_stage_cache.R"))
# The MBB compatibility runner supports the frozen one-core/two-core branch check.
paper_source_once(paper_path("support", "statistics", "api.R"))

paper_source_once(paper_path("tests", "support", "harness.R"))
paper_source_once(paper_path(
  "tests", "support", "scientific_comparison.R"
))
.test <- paper_test_harness()
check <- .test$check
skip <- .test$skip

paper_source_once(paper_path("tests", "inference", "set_envelope_checks.R"))
paper_source_once(paper_path("tests", "inference", "set_bootstrap_collection_checks.R"))
paper_source_once(paper_path("tests", "inference", "set_bootstrap_draw_checks.R"))
paper_source_once(paper_path(
  "tests", "inference", "bootstrap_legacy_characterization_helpers.R"
))
paper_source_once(paper_path(
  "tests", "inference", "bootstrap_legacy_characterization_checks.R"
))
paper_source_once(paper_path(
  "tests", "inference", "bootstrap_legacy_failure_characterization_checks.R"
))
paper_source_once(paper_path(
  "tests", "inference", "bootstrap_stage_real_primary_fixture.R"
))
paper_source_once(paper_path(
  "tests", "inference", "bootstrap_stage_real_primary_checks.R"
))
paper_source_once(paper_path(
  "tests", "inference", "bootstrap_stage_real_failure_boundary_checks.R"
))
paper_source_once(paper_path(
  "tests", "inference", "bootstrap_stage_cache_roundtrip_checks.R"
))
paper_source_once(paper_path("tests", "inference", "set_bootstrap_cores_checks.R"))
paper_source_once(paper_path("tests", "inference", "set_bootstrap_gate_checks.R"))

.test$finish()
