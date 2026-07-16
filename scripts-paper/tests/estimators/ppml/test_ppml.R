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
source(paper_path("config", "artifacts.R"))
source(paper_path("support", "identification", "profile_solver_core.R"))
source(paper_path("support", "identification", "profile_bounds_api.R"))
source(paper_path("log_variance", "core", "residual_map.R"))
source(paper_path("log_variance", "engine", "api.R"))
source(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))
source(paper_path("log_variance", "figures", "bounds_by_tau_test_support.R"))
source(paper_path("log_variance", "estimators", "ppml", "estimator.R"))
source(paper_path("log_variance", "estimators", "ppml", "pilot_and_grid.R"))
source(paper_path("log_variance", "tables", "table_formatting.R"))
source(paper_path("log_variance", "tables", "ppml_captions.R"))
# the shared SE scaffolding before the SE module (which routes through it), and
# the SE module before the check files so LOGVAR_PPML_SE_TYPES / the note helper
# resolve when a builder is called with se_type set (the driver block is guarded)
source(paper_path("log_variance", "inference", "standard_error_estimators.R"))
source(paper_path("log_variance", "estimators", "ppml", "standard_errors.R"))

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

source(paper_path("tests", "inference", "standard_error_estimators_checks.R"))
source(paper_path("tests", "estimators", "ppml", "fixtures.R"))
source(paper_path("tests", "estimators", "ppml", "fit_checks.R"))
source(paper_path("tests", "estimators", "ppml", "specification_checks.R"))
source(paper_path("tests", "estimators", "ppml", "engine_checks.R"))
source(paper_path("tests", "estimators", "ppml", "table_checks.R"))
source(paper_path("tests", "support", "envelope_cell_checks.R"))
source(paper_path("tests", "estimators", "ppml", "standard_error_checks.R"))
source(paper_path("tests", "estimators", "ppml", "coverage_checks.R"))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
