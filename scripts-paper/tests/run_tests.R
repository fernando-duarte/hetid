#!/usr/bin/env Rscript
# Run every genuine paper-analysis test suite in a fresh R process.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("config", "artifacts.R"))

suite_manifest <- data.frame(
  id = c(
    "engine", "engine_lad_seam", "residual_map", "harvey", "lad_crossing",
    "lad_dependency_gate", "lad_inner_fit", "lad_outer_map", "ppml",
    "dynamics_gate", "egarch_approval", "decision_clean_checkout",
    "joint_gmm", "joint_gmm_epigraph_solver",
    "joint_null", "fitted_volatility", "fitted_volatility_contracts",
    "set_bootstrap", "support_statistics", "support_heteroskedasticity",
    "support_identification_diagnostics", "boot_freshness", "variance_bounds",
    "variance_bounds_quoted"
  ),
  path = c(
    "engine/test_engine.R",
    "engine/test_lad_seam.R",
    "engine/test_residual_map.R",
    "estimators/harvey/test_harvey.R",
    "estimators/lad/test_crossing.R",
    "estimators/lad/test_dependency_gate.R",
    "estimators/lad/test_inner_fit.R",
    "estimators/lad/test_outer_map.R",
    "estimators/ppml/test_ppml.R",
    "diagnostics/dynamics/test_gate.R",
    "diagnostics/egarch/test_approval.R",
    "diagnostics/egarch/test_clean_checkout.R",
    "diagnostics/joint_gmm/test_joint_gmm.R",
    "diagnostics/joint_gmm/test_epigraph_solver.R",
    "diagnostics/joint_null/test_joint_null.R",
    "figures/fitted_volatility/test_fitted_volatility.R",
    "figures/fitted_volatility/test_contracts.R",
    "inference/test_set_bootstrap.R",
    "support/test_statistics.R",
    "support/test_heteroskedasticity.R",
    "support/test_identification_diagnostics.R",
    "support/boot_freshness_checks.R",
    "variance_bounds/test_variance_bounds.R",
    "variance_bounds/test_quoted_numbers.R"
  ),
  stringsAsFactors = FALSE
)

stopifnot(
  nrow(suite_manifest) > 0L,
  !anyDuplicated(suite_manifest$id),
  !anyDuplicated(suite_manifest$path)
)

checks <- rbind(
  data.frame(
    id = c("topology", "contract_ownership"),
    path = c(
      "support/check_topology.R",
      "support/check_contract_ownership.R"
    ),
    stringsAsFactors = FALSE
  ),
  suite_manifest
)
checks$path <- vapply(
  checks$path,
  function(path) paper_path("tests", path),
  character(1)
)
stopifnot(all(file.exists(checks$path)))

rscript <- file.path(R.home("bin"), "Rscript")
statuses <- integer(nrow(checks))
for (i in seq_len(nrow(checks))) {
  cat(sprintf("\n=== %s ===\n", checks$id[[i]]))
  status <- system2(rscript, checks$path[[i]])
  if (is.null(status)) status <- 0L
  statuses[[i]] <- as.integer(status)
}

failed <- checks$id[statuses != 0L]
if (length(failed)) {
  cat("\nFailed checks:", paste(failed, collapse = ", "), "\n")
  quit(status = 1L)
}
cat(sprintf(
  "\nAll %d suites and structural checks passed.\n",
  nrow(suite_manifest)
))
