# Offline checks for the estimator-generic set engine
# (scripts-paper/log_variance/engine/api.R with its scan/context modules, the
# generalized polish in scripts-paper/log_variance/core/endpoint_polish.R, and
# the log-OLS estimator object in log_variance/estimators/log_ols/estimator.R):
# a thin entrypoint
# that loads the map and engine layers, defines the shared check() counter,
# and sources the focused check files. Run from the package root:
#   Rscript scripts-paper/tests/engine/test_engine.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path("log_variance", "engine", "api.R"))
paper_source_once(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))
paper_source_once(paper_path(
  "log_variance", "estimators", "set_orchestration.R"
))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

paper_source_once(paper_path("tests", "engine", "map_checks.R"))
paper_source_once(paper_path("tests", "engine", "oracle_checks.R"))
paper_source_once(paper_path("tests", "engine", "schema_checks.R"))
paper_source_once(paper_path("tests", "engine", "service_checks.R"))
paper_source_once(paper_path("tests", "engine", "context_checks.R"))
paper_source_once(paper_path("tests", "engine", "hook_checks.R"))
paper_source_once(paper_path("tests", "engine", "seam_checks.R"))
paper_source_once(paper_path("tests", "engine", "orchestration_checks.R"))
paper_source_once(paper_path(
  "tests", "engine", "audit_orchestration_checks.R"
))
paper_source_once(paper_path("tests", "engine", "search_control_checks.R"))

.test$finish()
