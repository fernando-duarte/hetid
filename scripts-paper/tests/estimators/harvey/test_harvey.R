# Offline checks for the Harvey multiplicative-heteroskedasticity variance
# estimator: the math primitives (scripts-paper/log_variance/estimators/harvey/likelihood.R), the
# normalized recession certificate (recession_certificate.R), the zero-safe
# scoring solver and the shared-engine estimator object
# (estimator.R). This feature entrypoint mirrors the PPML suite: load
# the map/engine/log-OLS/PPML layers, source the Harvey module (the red target
# which sources math, recession, and solver modules), define the
# shared check() counter, and source the focused check files. The PPML module is
# loaded because the normalization block compares Harvey and PPML slopes. Run from
# the worktree root:
#   Rscript scripts-paper/tests/estimators/harvey/test_harvey.R

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("config", "artifacts.R"))
source(paper_path("support", "identification", "profile_solver_core.R"))
source(paper_path("support", "identification", "profile_bounds_api.R"))
source(paper_path("log_variance", "core", "residual_map.R"))
source(paper_path("log_variance", "engine", "api.R"))
source(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))
source(paper_path("log_variance", "estimators", "ppml", "estimator.R"))
source(paper_path("log_variance", "estimators", "harvey", "estimator.R"))
source(paper_path("log_variance", "inference", "standard_error_estimators.R"))
source(paper_path("log_variance", "estimators", "harvey", "standard_errors.R"))
source(paper_path("support", "latex", "table_pipeline.R"))
source(paper_path("support", "latex", "simple_table.R"))
source(paper_path("log_variance", "tables", "table_formatting.R"))
source(paper_path("log_variance", "tables", "harvey_panel.R"))

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

source(paper_path("tests", "estimators", "harvey", "fixtures.R"))
source(paper_path("tests", "estimators", "harvey", "objective_checks.R"))
source(paper_path("tests", "estimators", "harvey", "solver_checks.R"))
source(paper_path("tests", "estimators", "harvey", "recession_checks.R"))
source(paper_path("tests", "estimators", "harvey", "engine_checks.R"))
source(paper_path("tests", "estimators", "harvey", "table_checks.R"))
source(paper_path("tests", "support", "envelope_cell_checks.R"))
source(paper_path("tests", "estimators", "harvey", "standard_error_checks.R"))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
