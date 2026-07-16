# Offline checks for the vol-equation set-endpoint bootstrap inference layer.
# A thin entrypoint mirroring the PPML suite: source the envelope module
# (which itself sources set_id_inference.R for robust_scale/boot_min_reps), the
# mean-eq recipe (identification_utils/profile_bounds/tau_star_utils/
# set_id_bootstrap_core -- estimate_set_id_system/coef_interval_tables/
# tau_quadratic_system) and the log-var engine/estimator chain (map/engine/
# log-OLS/PPML/Harvey) the per-draw re-estimator drives, then the per-draw
# core module itself. Define the shared check() counter, and source the
# focused check files. Run from the worktree root:
#   Rscript scripts-paper/tests/inference/test_set_bootstrap.R

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("config", "artifacts.R"))
source(paper_path("log_variance", "inference", "set_envelope.R"))
source(repo_path("scripts", "utils", "identification_utils.R"))
source(repo_path("scripts", "utils", "profile_bounds_core.R"))
source(repo_path("scripts", "utils", "profile_bounds.R"))
source(repo_path("scripts", "utils", "tau_star_utils.R"))
source(repo_path("scripts", "utils", "set_id_bootstrap_core.R"))
source(paper_path("log_variance", "core", "residual_map.R"))
source(paper_path("log_variance", "engine", "api.R"))
source(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))
source(paper_path("log_variance", "estimators", "ppml", "estimator.R"))
source(paper_path("log_variance", "estimators", "harvey", "estimator.R"))
source(paper_path("log_variance", "inference", "set_bootstrap_core.R"))

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

source(paper_path("tests", "inference", "set_envelope_checks.R"))
source(paper_path("tests", "inference", "set_bootstrap_core_checks.R"))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
