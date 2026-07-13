# Offline checks for the estimator-generic set engine
# (scripts-paper/log_var_eq_engine.R with its scan/context modules, the
# generalized polish in scripts-paper/log_var_eq_polish.R, and the log-OLS
# estimator object in scripts-paper/log_var_eq_logols.R): a thin entry point
# that loads the map and engine layers, defines the shared check() counter,
# and sources the focused check files. Run from the package root:
#   Rscript scripts/utils/tests/test_logvar_engine.R

source(here::here("scripts/utils/profile_bounds_core.R"))
source(here::here("scripts/utils/profile_bounds.R"))
source(here::here("scripts-paper/log_var_eq_map.R"))
source(here::here("scripts-paper/log_var_eq_engine.R"))
source(here::here("scripts-paper/log_var_eq_logols.R"))

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

source(here::here("scripts/utils/tests/logvar_engine_map_checks.R"))
source(here::here("scripts/utils/tests/logvar_engine_oracle_checks.R"))
source(here::here("scripts/utils/tests/logvar_engine_schema_checks.R"))
source(here::here("scripts/utils/tests/logvar_engine_service_checks.R"))
source(here::here("scripts/utils/tests/logvar_engine_context_checks.R"))
source(here::here("scripts/utils/tests/logvar_engine_hook_checks.R"))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
