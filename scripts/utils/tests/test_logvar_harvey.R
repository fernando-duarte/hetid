# Offline checks for the Harvey multiplicative-heteroskedasticity variance
# estimator: the math primitives (scripts-paper/log_var_eq_harvey_math.R), the
# normalized recession certificate (log_var_eq_harvey_recession.R), the zero-safe
# scoring solver (log_var_eq_harvey_solver.R), and the Plan 7 estimator object
# (log_var_eq_harvey.R). A thin entry point mirroring test_logvar_ppml.R: load
# the map/engine/log-OLS/PPML layers, source the Harvey module (the red target
# until Task 3 lands, which itself sources math/recession/solver), define the
# shared check() counter, and source the focused check files. The PPML module is
# loaded because the normalization block compares Harvey and PPML slopes. Run from
# the worktree root:
#   Rscript scripts/utils/tests/test_logvar_harvey.R

source(here::here("scripts/utils/profile_bounds_core.R"))
source(here::here("scripts/utils/profile_bounds.R"))
source(here::here("scripts-paper/log_var_eq_map.R"))
source(here::here("scripts-paper/log_var_eq_engine.R"))
source(here::here("scripts-paper/log_var_eq_logols.R"))
source(here::here("scripts-paper/log_var_eq_ppml.R"))
source(here::here("scripts-paper/log_var_eq_harvey.R"))

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

source(here::here("scripts/utils/tests/logvar_harvey_fixtures.R"))
source(here::here("scripts/utils/tests/logvar_harvey_math_checks.R"))
source(here::here("scripts/utils/tests/logvar_harvey_solver_checks.R"))
source(here::here("scripts/utils/tests/logvar_harvey_recession_checks.R"))
source(here::here("scripts/utils/tests/logvar_harvey_engine_checks.R"))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
