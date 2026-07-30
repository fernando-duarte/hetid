# Checks for the six named full-reset compartments and their coverage of the manifest.

local({
  original_manifest <- artifact_manifest
  root <- tempfile("paper-artifact-reset-compartments-")
  dir.create(root, recursive = TRUE)
  on.exit(
    {
      artifact_manifest <<- original_manifest
      unlink(root, recursive = TRUE)
    },
    add = TRUE
  )

  test_manifest <- artifact_manifest
  test_manifest$new_path <- file.path(root, test_manifest$group, test_manifest$basename)
  assign("artifact_manifest", test_manifest, envir = .GlobalEnv)

  write_all_fixtures <- function() {
    invisible(vapply(artifact_manifest$new_path, function(path) {
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      writeLines("fixture", path)
      file.exists(path)
    }, logical(1)))
  }

  write_all_fixtures()
  bootstrap_audit <- cleanup_bootstrap_cache()
  check(
    "cleanup_bootstrap_cache removes only bootstrap_stage_draws",
    identical(bootstrap_audit$ids, "bootstrap_stage_draws") &&
      bootstrap_audit$all_absent &&
      all(file.exists(artifact_manifest$new_path[artifact_manifest$id != "bootstrap_stage_draws"]))
  )

  others_survive <- function(removed_ids) {
    sibling_paths <- artifact_manifest$new_path[!artifact_manifest$id %in% removed_ids]
    all(file.exists(sibling_paths))
  }

  write_all_fixtures()
  gate_audit <- cleanup_gate_state()
  expected_gate_ids <- c(
    "dynamics_gate", "egarch_status", "conditional_route_status", "egarch_pilot_state"
  )
  check(
    "cleanup_gate_state removes exactly the gate/decision ids and no siblings",
    setequal(gate_audit$ids, expected_gate_ids) &&
      gate_audit$all_absent &&
      others_survive(expected_gate_ids)
  )

  write_all_fixtures()
  diagnostics_ids <- artifact_manifest$id[artifact_manifest$group == "diagnostics"]
  diagnostics_audit <- cleanup_diagnostics()
  check(
    "cleanup_diagnostics removes every diagnostics-group row and no siblings",
    setequal(diagnostics_audit$ids, diagnostics_ids) &&
      diagnostics_audit$all_absent &&
      others_survive(diagnostics_ids)
  )

  write_all_fixtures()
  tables_ids <- artifact_manifest$id[
    artifact_manifest$group %in% c("tables", "tables/descriptive_statistics")
  ]
  tables_audit <- cleanup_tables(include_tracked = TRUE)
  check(
    "cleanup_tables(include_tracked = TRUE) removes every tables-group row and no siblings",
    setequal(tables_audit$ids, tables_ids) &&
      tables_audit$all_absent &&
      others_survive(tables_ids)
  )

  audits <- reset_pipeline_state(include_tracked = TRUE)
  check(
    "reset_pipeline_state returns exactly the seven documented compartments",
    setequal(
      names(audits),
      c(
        "bootstrap_cache", "gate_state", "diagnostics",
        "tables", "figures", "reports", "latex_sidecars"
      )
    )
  )
})

local({
  original_manifest <- artifact_manifest
  root <- tempfile("paper-artifact-reset-coverage-")
  dir.create(root, recursive = TRUE)
  on.exit(
    {
      artifact_manifest <<- original_manifest
      unlink(root, recursive = TRUE)
    },
    add = TRUE
  )

  test_manifest <- artifact_manifest
  test_manifest$new_path <- file.path(root, test_manifest$group, test_manifest$basename)
  assign("artifact_manifest", test_manifest, envir = .GlobalEnv)

  invisible(vapply(artifact_manifest$new_path, function(path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("fixture", path)
    file.exists(path)
  }, logical(1)))

  check(
    "the six compartments cover every manifest row",
    setequal(
      c(
        cleanup_bootstrap_cache()$ids,
        cleanup_gate_state()$ids,
        cleanup_diagnostics()$ids,
        cleanup_tables(include_tracked = TRUE)$ids,
        cleanup_figures(include_tracked = TRUE)$ids,
        cleanup_reports(include_tracked = TRUE)$ids
      ),
      artifact_manifest$id
    )
  )
})
