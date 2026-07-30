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
