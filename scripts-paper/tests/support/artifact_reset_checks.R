# Checks for the generalized id-keyed cleanup primitive and the named
# full-reset compartments built on top of it.

local({
  original_manifest <- artifact_manifest
  root <- tempfile("paper-artifact-reset-")
  dir.create(root, recursive = TRUE)
  on.exit(
    {
      artifact_manifest <<- original_manifest
      unlink(root, recursive = TRUE)
    },
    add = TRUE
  )

  test_manifest <- artifact_manifest
  test_manifest$new_path <- file.path(
    root,
    test_manifest$group,
    test_manifest$basename
  )
  assign("artifact_manifest", test_manifest, envir = .GlobalEnv)

  write_fixture <- function(id) {
    path <- artifact_manifest$new_path[artifact_manifest$id == id]
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("fixture", path)
    file.exists(path)
  }

  ids <- c("bootstrap_stage_draws", "dynamics_gate")
  created <- vapply(ids, write_fixture, logical(1))
  audit <- cleanup_artifacts_by_ids(ids)
  check(
    "cleanup_artifacts_by_ids removes exactly the requested ids",
    all(created) &&
      identical(audit$ids, ids) &&
      audit$all_absent &&
      audit$n_existed == length(ids) &&
      audit$n_deleted == length(ids) &&
      !any(file.exists(
        artifact_manifest$new_path[artifact_manifest$id %in% ids]
      ))
  )

  check(
    "cleanup_artifacts_by_ids rejects an unknown id",
    inherits(
      try(cleanup_artifacts_by_ids("not_a_real_id"), silent = TRUE),
      "try-error"
    )
  )
})

local({
  check(
    "artifact_records_by_group returns only that group's rows",
    all(artifact_records_by_group("diagnostics")$group == "diagnostics") &&
      nrow(artifact_records_by_group("diagnostics")) > 0L
  )
  check(
    "artifact_records_by_group rejects an unknown group",
    inherits(try(artifact_records_by_group("not_a_group"), silent = TRUE), "try-error")
  )

  tracked_path <- repo_path("README.Rmd")
  gitignored_path <- file.path(
    repo_root, "scripts-paper", "output", "state",
    "bootstrap_stage_draws.rds"
  )
  gitignored_flags <- .artifact_gitignored(
    c(tracked_path, gitignored_path)
  )
  check(
    "a tracked file is not reported gitignored and an .rds output path is",
    identical(gitignored_flags, c(FALSE, TRUE))
  )

  state_group_ids <- artifact_manifest$id[artifact_manifest$group == "state"]
  check(
    "reset_group_ids on the state group with include_tracked=FALSE keeps every row",
    setequal(.reset_group_ids("state", include_tracked = FALSE), state_group_ids)
  )
})

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
  reset_cli_output <- function(arguments = character(0)) {
    system2(
      file.path(R.home("bin"), "Rscript"),
      args = c("--vanilla", paper_path("reset_pipeline_state.R"), arguments),
      stdout = TRUE, stderr = TRUE
    )
  }
  reset_cli_status <- function(output) {
    status <- attr(output, "status")
    if (is.null(status)) 0L else as.integer(status)
  }

  output <- reset_cli_output()
  check(
    "reset_pipeline_state.R runs to completion with no fixtures present",
    identical(reset_cli_status(output), 0L)
  )
  check(
    "reset_pipeline_state.R reports every compartment by name",
    all(vapply(
      c(
        "bootstrap_cache", "gate_state", "diagnostics",
        "tables", "figures", "reports", "latex_sidecars"
      ),
      function(name) any(grepl(name, output, fixed = TRUE)),
      logical(1)
    ))
  )

  keep_tracked_output <- reset_cli_output("--keep-tracked")
  check(
    "reset_pipeline_state.R --keep-tracked also runs to completion",
    identical(reset_cli_status(keep_tracked_output), 0L)
  )
})
