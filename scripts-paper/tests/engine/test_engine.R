# Offline checks for the estimator-generic set engine
# (scripts-paper/log_variance/engine/api.R with its scan/context modules, the
# generalized polish in scripts-paper/log_variance/core/endpoint_polish.R, and
# the log-OLS estimator object in log_variance/estimators/log_ols/estimator.R):
# a thin entrypoint
# that loads the map and engine layers, defines the shared check() counter,
# and sources the focused check files. Run from the package root:
#   Rscript scripts-paper/tests/engine/test_engine.R

source(file.path("scripts-paper", "config", "paths.R"))
source(paper_path("config", "artifacts.R"))
source(repo_path("scripts", "utils", "profile_bounds_core.R"))
source(repo_path("scripts", "utils", "profile_bounds.R"))
source(paper_path("log_variance", "core", "residual_map.R"))
source(paper_path("log_variance", "engine", "api.R"))
source(paper_path("log_variance", "estimators", "log_ols", "estimator.R"))

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

source(paper_path("tests", "engine", "map_checks.R"))
source(paper_path("tests", "engine", "oracle_checks.R"))
source(paper_path("tests", "engine", "schema_checks.R"))
source(paper_path("tests", "engine", "service_checks.R"))
source(paper_path("tests", "engine", "context_checks.R"))
source(paper_path("tests", "engine", "hook_checks.R"))
source(paper_path("tests", "engine", "seam_checks.R"))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
