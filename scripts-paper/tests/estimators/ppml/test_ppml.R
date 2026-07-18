# Offline checks for the PPML (quasi-Poisson log-link) log-variance estimator:
# the fit module (scripts-paper/log_variance/estimators/ppml/fit.R via estimator.R),
# the b-wrapper/estimator constructor (scripts-paper/log_variance/estimators/ppml/estimator.R), the
# engine seam refinement (start pools and grid_selector), the
# Morton coverage selector (scripts-paper/log_variance/estimators/ppml/pilot_and_grid.R),
# and the panel-ordering helper. A thin entry point mirroring
# the engine suite: load the map/engine/estimator layers, source the PPML
# module, define the shared check()
# counter, and source the focused check files. Run from the worktree root:
#   Rscript scripts-paper/tests/estimators/ppml/test_ppml.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path("log_variance", "engine", "api.R"))
paper_source_once(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))
paper_source_once(paper_path("log_variance", "figures", "bounds_by_tau_test_support.R"))
paper_source_once(paper_path("log_variance", "estimators", "ppml", "estimator.R"))
paper_source_once(paper_path("log_variance", "estimators", "ppml", "pilot_and_grid.R"))
paper_source_once(paper_path("log_variance", "tables", "table_formatting.R"))
paper_source_once(paper_path("log_variance", "tables", "ppml_captions.R"))
# the shared SE scaffolding before the SE module (which routes through it), and
# the SE module before the check files so LOGVAR_PPML_SE_TYPES / the note helper
# resolve when a builder is called with se_type set (the driver block is guarded)
paper_source_once(paper_path("log_variance", "inference", "standard_error_estimators.R"))
paper_source_once(paper_path("log_variance", "estimators", "ppml", "standard_errors.R"))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

paper_source_once(paper_path("tests", "inference", "standard_error_estimators_checks.R"))
paper_source_once(paper_path("tests", "estimators", "ppml", "fixtures.R"))
paper_source_once(paper_path("tests", "estimators", "ppml", "fit_checks.R"))
paper_source_once(paper_path("tests", "estimators", "ppml", "specification_checks.R"))
paper_source_once(paper_path(
  "tests", "estimators", "ppml", "control_identity_checks.R"
))
paper_source_once(paper_path("tests", "estimators", "ppml", "engine_checks.R"))
paper_source_once(paper_path("tests", "estimators", "ppml", "table_checks.R"))
paper_source_once(paper_path("tests", "support", "envelope_cell_checks.R"))
paper_source_once(paper_path("tests", "estimators", "ppml", "standard_error_checks.R"))
paper_source_once(paper_path("tests", "estimators", "ppml", "coverage_checks.R"))
paper_source_once(paper_path(
  "tests", "estimators", "ppml", "coverage_provenance_checks.R"
))

.test$finish()
