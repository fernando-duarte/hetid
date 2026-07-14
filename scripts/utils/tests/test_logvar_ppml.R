# Offline checks for the PPML (quasi-Poisson log-link) log-variance estimator:
# the fit module (scripts-paper/log_var_eq_ppml_fit.R via log_var_eq_ppml.R),
# the b-wrapper/estimator constructor (scripts-paper/log_var_eq_ppml.R), the
# Task 4 engine seam refinement (start pools + grid_selector), the Task 5
# Morton coverage selector (scripts-paper/log_var_eq_ppml_driver_helpers.R),
# and the Task 6 panel-ordering helper. A thin entry point mirroring
# test_logvar_engine.R: load the map/engine/estimator layers, source the PPML
# module (the red target until Task 3 lands), define the shared check()
# counter, and source the focused check files. Run from the worktree root:
#   Rscript scripts/utils/tests/test_logvar_ppml.R

source(here::here("scripts/utils/profile_bounds_core.R"))
source(here::here("scripts/utils/profile_bounds.R"))
source(here::here("scripts-paper/log_var_eq_map.R"))
source(here::here("scripts-paper/log_var_eq_engine.R"))
source(here::here("scripts-paper/log_var_eq_logols.R"))
source(here::here("scripts/utils/logvar_bounds_plot_data.R"))
source(here::here("scripts-paper/log_var_eq_ppml.R"))
if (file.exists(here::here("scripts-paper/log_var_eq_ppml_driver_helpers.R"))) {
  source(here::here("scripts-paper/log_var_eq_ppml_driver_helpers.R"))
}
source(here::here("scripts-paper/log_var_eq_table_utils.R"))
source(here::here("scripts-paper/log_var_eq_ppml_notes.R"))
# the SE module before the check files so LOGVAR_PPML_SE_TYPES / the note helper
# resolve when a builder is called with se_type set (the driver block is guarded)
source(here::here("scripts-paper/log_var_eq_ppml_se.R"))

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

source(here::here("scripts/utils/tests/logvar_ppml_fixtures.R"))
source(here::here("scripts/utils/tests/test_logvar_ppml_fit.R"))
if (file.exists(here::here("scripts/utils/tests/test_logvar_ppml_spec.R"))) {
  source(here::here("scripts/utils/tests/test_logvar_ppml_spec.R"))
}
source(here::here("scripts/utils/tests/test_logvar_ppml_engine.R"))
source(here::here("scripts/utils/tests/test_logvar_ppml_table.R"))
source(here::here("scripts/utils/tests/test_logvar_ppml_se.R"))
if (file.exists(here::here("scripts-paper/log_var_eq_ppml_driver_helpers.R"))) {
  source(here::here("scripts/utils/tests/test_logvar_ppml_coverage.R"))
}

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
