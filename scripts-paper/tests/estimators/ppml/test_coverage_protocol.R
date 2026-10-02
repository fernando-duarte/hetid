# Fresh-process check that the coverage apply (coverage.R) carries its own
# definitions: sourcing it without the PPML pilot module must still define the
# coverage protocol and provenance helpers, so callers other than PPML (Harvey,
# the log projections) can reconcile an audit. Run from the package root:
#   Rscript scripts-paper/tests/estimators/ppml/test_coverage_protocol.R

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))
paper_source_once(paper_path("log_variance", "estimators", "controls.R"))
paper_source_once(paper_path("log_variance", "estimators", "ppml", "coverage.R"))

paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

check(
  "coverage.R brings its protocol and provenance helpers with it",
  exists("LOGVAR_PPML_COVERAGE_PROTOCOL") &&
    exists(".logvar_ppml_selector_provenance") && exists(".logvar_prov_suffix")
)
applied <- tryCatch(
  logvar_ppml_apply_coverage(list(), list()),
  error = function(e) e
)
check(
  "the apply runs without the PPML pilot module",
  !inherits(applied, "error") && is.list(applied$results)
)

.test$finish()
