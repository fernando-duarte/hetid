# Offline checks for the joint moment-compatibility layer (Plan 4, logvar-joint-gmm):
# the stacked log/PPML/z moment functions and their scales and Jacobians
# (log_var_eq_moments.R), the representation-stable residualized-z basis
# (log_var_eq_joint_basis.R), the profiled intercepts and their spelled-out
# derivatives (log_var_eq_joint_profiles.R), the canonical joint_input_id
# (log_var_eq_joint_identity.R), the GMM-owned budget state (log_var_eq_joint_budget.R),
# candidate/sign-pattern accounting (log_var_eq_joint_candidates.R), the epigraph
# statuses and pinned constants (log_var_eq_joint_epigraph.R), the gated Stage C
# projection (log_var_eq_joint_projection.R), the artifact schemas
# (log_var_eq_joint_artifacts.R), and the gate-branching driver core
# (log_var_eq_joint_gmm.R). A thin entry point mirroring test_logvar_harvey.R: load
# the map/log-OLS layers and the committed Task 1 decision helpers, guard-source the
# not-yet-built joint modules so the suite runs to completion with module-dependent
# checks failing closed (red until Tasks 3 and 4 land), build the deterministic
# fixtures, then source the moment, search, and integration check files. Run from the
# worktree root:
#   Rscript scripts/utils/tests/test_logvar_joint_gmm.R

source(here::here("scripts/utils/profile_bounds_core.R"))
source(here::here("scripts/utils/profile_bounds.R"))
source(here::here("scripts-paper/log_var_eq_map.R"))
source(here::here("scripts-paper/log_var_eq_logols.R"))
# The Task 1 ratification gate is committed, so its pure helpers and the checked-in
# record source cleanly and the decision-layer checks may pass.
source(here::here("scripts-paper/log_var_eq_joint_decision.R"))
source(here::here("scripts-paper/logvar_joint_gmm_decision.R"))

# The Stage A and Stage B/C modules do not exist yet; source only what is present so
# the suite runs to completion with the module-dependent checks failing closed.
jg_modules <- c(
  "scripts-paper/log_var_eq_moments.R",
  "scripts-paper/log_var_eq_joint_basis.R",
  "scripts-paper/log_var_eq_joint_profiles.R",
  "scripts-paper/log_var_eq_joint_identity.R",
  "scripts-paper/log_var_eq_joint_budget.R",
  "scripts-paper/log_var_eq_joint_candidates.R",
  "scripts-paper/log_var_eq_joint_epigraph.R",
  "scripts-paper/log_var_eq_joint_projection.R",
  "scripts-paper/log_var_eq_joint_artifacts.R",
  "scripts-paper/log_var_eq_joint_gmm.R"
)
for (f in jg_modules) if (file.exists(here::here(f))) source(here::here(f))

.pass <- 0L
.fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}
# Fail a check closed when a not-yet-defined joint-gmm function is absent or errors.
jg_try <- function(expr) tryCatch(isTRUE(expr), error = function(e) FALSE)
# Assert a not-yet-built symbol exists, so an absent module fails the check for the
# right reason (function not found) rather than passing on an unrelated coincidence.
jg_need <- function(...) stopifnot(vapply(c(...), exists, logical(1)))

source(here::here("scripts/utils/tests/logvar_joint_gmm_fixtures.R"))
source(here::here("scripts/utils/tests/logvar_joint_gmm_moment_checks.R"))
source(here::here("scripts/utils/tests/logvar_joint_gmm_profile_checks.R"))
source(here::here("scripts/utils/tests/logvar_joint_gmm_search_checks.R"))
source(here::here("scripts/utils/tests/logvar_joint_gmm_integration_checks.R"))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
