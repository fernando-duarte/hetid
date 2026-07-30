# Group/id-based selection helpers for the six named full-reset compartments.

paper_source_once(paper_path("support", "latex", "table_pipeline.R"))

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

cleanup_bootstrap_cache <- function() {
  cleanup_artifacts_by_ids("bootstrap_stage_draws")
}

cleanup_gate_state <- function() {
  cleanup_artifacts_by_ids(c(
    "dynamics_gate", "egarch_status",
    "conditional_route_status", "egarch_pilot_state"
  ))
}

cleanup_diagnostics <- function() {
  cleanup_artifacts_by_ids(artifact_records_by_group("diagnostics")$id)
}

cleanup_tables <- function(include_tracked = TRUE) {
  cleanup_artifacts_by_ids(.reset_group_ids(
    c("tables", "tables/descriptive_statistics"), include_tracked
  ))
}

cleanup_figures <- function(include_tracked = TRUE) {
  cleanup_artifacts_by_ids(.reset_group_ids(
    c("figures", "figures/descriptive_statistics"), include_tracked
  ))
}

cleanup_reports <- function(include_tracked = TRUE) {
  cleanup_artifacts_by_ids(.reset_group_ids("reports", include_tracked))
}

reset_pipeline_state <- function(include_tracked = TRUE) {
  audits <- list(
    bootstrap_cache = cleanup_bootstrap_cache(),
    gate_state = cleanup_gate_state(),
    diagnostics = cleanup_diagnostics(),
    tables = cleanup_tables(include_tracked),
    figures = cleanup_figures(include_tracked),
    reports = cleanup_reports(include_tracked)
  )
  audits$latex_sidecars <- clean_latex_sidecars(out_dir)
  audits
}
