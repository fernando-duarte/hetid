# Subprocess test for the reset_pipeline_state.R CLI entrypoint.
#
# The CLI is destructive by design, so it is pointed at a throwaway package root
# rather than at the real tree. out_dir is "scripts-paper/output" relative to the
# working directory (config/paths.R), so a subprocess launched with wd set to a
# skeleton root deletes only that skeleton's output. Everything else in the
# skeleton is a symlink to the real source tree, which the CLI never writes to.
#
# This replaces a backup-and-restore of the live scripts-paper/output. That
# guarded the destructive step with on.exit, which does not run when the suite is
# killed -- and the window it left open included the deletion of the bootstrap
# cache, five hours of draws that no git operation can restore because the whole
# tree is gitignored.

local({
  skeleton <- tempfile("reset_cli_root_")
  dir.create(file.path(skeleton, "scripts-paper"), recursive = TRUE)
  on.exit(unlink(skeleton, recursive = TRUE), add = TRUE)

  # paths.R refuses to load outside a hetid package root, and .artifact_gitignored
  # shells out to git check-ignore, which errors outside a repository. Both read
  # from the working directory, so the skeleton has to satisfy them itself.
  file.copy(file.path(repo_root, "DESCRIPTION"), skeleton)
  file.copy(file.path(repo_root, ".gitignore"), skeleton)
  stopifnot(identical(system2(
    "git", c("-C", skeleton, "init", "-q"),
    stdout = FALSE, stderr = FALSE
  ), 0L))

  real_paper <- file.path(repo_root, "scripts-paper")
  for (entry in setdiff(list.files(real_paper, all.files = TRUE, no.. = TRUE), "output")) {
    stopifnot(file.symlink(
      file.path(real_paper, entry),
      file.path(skeleton, "scripts-paper", entry)
    ))
  }

  # every manifest artifact as an empty file, so each compartment has real work to
  # do and the tracked/gitignored split is exercised rather than assumed
  populate <- function() {
    paths <- file.path(skeleton, artifact_manifest$new_path)
    for (dir in unique(dirname(paths))) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    file.create(paths)
    paths
  }
  tracked <- !.artifact_gitignored(artifact_manifest$new_path)

  reset_cli_output <- function(arguments = character(0)) {
    system2(
      file.path(R.home("bin"), "Rscript"),
      args = c("--vanilla", file.path("scripts-paper", "reset_pipeline_state.R"), arguments),
      stdout = TRUE, stderr = TRUE
    )
  }
  reset_cli_status <- function(output) {
    status <- attr(output, "status")
    if (is.null(status)) 0L else as.integer(status)
  }
  in_skeleton <- function(...) {
    previous <- setwd(skeleton)
    on.exit(setwd(previous), add = TRUE)
    reset_cli_output(...)
  }

  paths <- populate()
  output <- in_skeleton()
  check(
    "reset_pipeline_state.R runs to completion and clears every artifact",
    identical(reset_cli_status(output), 0L) && !any(file.exists(paths))
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

  paths <- populate()
  keep_tracked_output <- in_skeleton("--keep-tracked")
  check(
    "reset_pipeline_state.R --keep-tracked spares the tracked artifacts alone",
    identical(reset_cli_status(keep_tracked_output), 0L) &&
      all(file.exists(paths[tracked])) &&
      !any(file.exists(paths[!tracked]))
  )
})
