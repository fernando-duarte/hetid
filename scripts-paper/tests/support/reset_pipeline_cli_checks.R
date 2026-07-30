# Subprocess test for the reset_pipeline_state.R CLI entrypoint.
# Backup/restore guards against destructive test deleting real tracked files.

local({
  output_dir <- paper_path("output")
  backup_dir <- tempfile("reset_cli_backup_")
  backup_exists <- dir.exists(output_dir)

  if (backup_exists) {
    dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(output_dir, backup_dir, recursive = TRUE)
  }

  restore_output <- function() {
    unlink(output_dir, recursive = TRUE)
    if (backup_exists) {
      file.copy(file.path(backup_dir, "output"), file.path(repo_root, "scripts-paper"),
        recursive = TRUE
      )
    }
    unlink(backup_dir, recursive = TRUE)
  }

  on.exit(restore_output(), add = TRUE)

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

  if (backup_exists) {
    unlink(output_dir, recursive = TRUE)
    file.copy(file.path(backup_dir, "output"), file.path(repo_root, "scripts-paper"),
      recursive = TRUE
    )
  }

  keep_tracked_output <- reset_cli_output("--keep-tracked")
  check(
    "reset_pipeline_state.R --keep-tracked also runs to completion",
    identical(reset_cli_status(keep_tracked_output), 0L)
  )
})
