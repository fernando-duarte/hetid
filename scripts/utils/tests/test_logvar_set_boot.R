# Offline checks for the vol-equation set-endpoint bootstrap inference layer.
# A thin entry point mirroring test_logvar_ppml.R: source the envelope module
# (which itself sources set_id_inference.R for robust_scale/boot_min_reps), the
# mean-eq recipe (identification_utils/profile_bounds/tau_star_utils/
# set_id_bootstrap_core -- estimate_set_id_system/coef_interval_tables/
# tau_quadratic_system) and the log-var engine/estimator chain (map/engine/
# log-OLS/PPML/Harvey) the per-draw re-estimator drives, then the per-draw
# core module itself. Define the shared check() counter, and source the
# focused check files. Run from the worktree root:
#   Rscript scripts/utils/tests/test_logvar_set_boot.R

source(here::here("scripts/utils/logvar_set_envelope.R"))
source(here::here("scripts/utils/identification_utils.R"))
source(here::here("scripts/utils/profile_bounds_core.R"))
source(here::here("scripts/utils/profile_bounds.R"))
source(here::here("scripts/utils/tau_star_utils.R"))
source(here::here("scripts/utils/set_id_bootstrap_core.R"))
source(here::here("scripts-paper/log_var_eq_map.R"))
source(here::here("scripts-paper/log_var_eq_engine.R"))
source(here::here("scripts-paper/log_var_eq_logols.R"))
source(here::here("scripts-paper/log_var_eq_ppml.R"))
source(here::here("scripts-paper/log_var_eq_harvey.R"))
source(here::here("scripts/utils/logvar_set_bootstrap_core.R"))

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

source(here::here("scripts/utils/tests/logvar_set_envelope_checks.R"))
source(here::here("scripts/utils/tests/logvar_set_bootstrap_core_checks.R"))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
