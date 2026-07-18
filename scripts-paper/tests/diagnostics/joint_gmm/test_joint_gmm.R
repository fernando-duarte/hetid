# Offline checks for the joint moment-compatibility layer:
# the stacked log/PPML/z moment functions and their scales and Jacobians
# (moments.R), the representation-stable residualized-z basis
# (basis.R), the profiled intercepts and their spelled-out
# derivatives (profiles.R), the canonical joint_input_id
# (identity.R), the GMM-owned budget state (budget.R),
# candidate/sign-pattern accounting (candidates.R), the epigraph
# statuses and pinned constants (epigraph.R), the gated ratified-projection
# projection (projection.R), the artifact schemas
# (artifact_schema.R), and the gate-branching driver core
# (specification_and_replication.R). This feature entrypoint loads
# the map/log-OLS layers and the committed decision helpers, source the required
# joint modules, build the deterministic
# fixtures, then source the moment, search, and integration check files. Run from the
# worktree root:
#   Rscript scripts-paper/tests/diagnostics/joint_gmm/test_joint_gmm.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
paper_source_once(paper_path("log_variance", "core", "residual_map.R"))
paper_source_once(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))
# The ratification gate is committed, so its pure helpers and the checked-in
# record source cleanly and the decision-layer checks may pass.
paper_source_once(paper_path("log_variance", "diagnostics", "joint_gmm", "decision_schema.R"))
paper_source_once(paper_path("config", "decisions", "joint_gmm.R"))

# The required numerical and artifact-schema modules.
jg_modules <- vapply(c(
  "moments.R", "basis.R", "profiles.R", "identity.R", "budget.R",
  "candidates.R", "epigraph.R", "projection.R", "artifact_schema.R",
  "specification_and_replication.R"
), function(file) {
  paper_path("log_variance", "diagnostics", "joint_gmm", file)
}, character(1))
for (file in jg_modules) {
  source(file)
}

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check
# Fail a check closed when a required joint-GMM function is absent or errors.
jg_try <- function(expr) tryCatch(isTRUE(expr), error = function(e) FALSE)
# Assert a required symbol exists, so an absent module fails the check for the
# right reason (function not found) rather than passing on an unrelated coincidence.
jg_need <- function(...) stopifnot(vapply(c(...), exists, logical(1)))

paper_source_once(paper_path("tests", "diagnostics", "joint_gmm", "fixtures.R"))
paper_source_once(paper_path("tests", "diagnostics", "joint_gmm", "moment_checks.R"))
paper_source_once(paper_path("tests", "diagnostics", "joint_gmm", "profile_checks.R"))
paper_source_once(paper_path("tests", "diagnostics", "joint_gmm", "search_checks.R"))
paper_source_once(paper_path("tests", "diagnostics", "joint_gmm", "integration_checks.R"))
paper_source_once(paper_path(
  "tests", "diagnostics", "joint_gmm", "schema_contract_checks.R"
))

.test$finish()
