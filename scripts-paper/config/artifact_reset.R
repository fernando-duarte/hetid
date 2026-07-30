# Group/id-based selection helpers for the six named full-reset compartments.

artifact_records_by_group <- function(group) {
  group <- .artifact_scalar(group, "Artifact group")
  if (!group %in% unname(PAPER_ARTIFACT_GROUPS)) {
    stop(sprintf("Unknown artifact group: %s", group), call. = FALSE)
  }
  artifact_manifest[artifact_manifest$group == group, , drop = FALSE]
}

.artifact_gitignored <- function(paths) {
  vapply(paths, function(path) {
    status <- system2(
      "git",
      c("-C", repo_root, "check-ignore", "-q", "--", path),
      stdout = FALSE, stderr = FALSE
    )
    if (!status %in% c(0L, 1L)) {
      stop(
        sprintf("git check-ignore failed on %s (status %d)", path, status),
        call. = FALSE
      )
    }
    status == 0L
  }, logical(1), USE.NAMES = FALSE)
}

.reset_group_ids <- function(groups, include_tracked) {
  rows <- artifact_manifest[
    artifact_manifest$group %in% groups, ,
    drop = FALSE
  ]
  if (!include_tracked) {
    rows <- rows[.artifact_gitignored(rows$new_path), , drop = FALSE]
  }
  rows$id
}
