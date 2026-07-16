# Base-R unconditional cleanup of the four dynamic-only EGARCH-X artifacts.
# run_pipeline.R sources this before the gate and calls the cleanup
# on every run so a stale pilot / CSV / RDS / bounds PDF from an earlier dynamic
# run can never masquerade as a fresh result behind a non-rejecting gate. This is
# the single source of truth for the four dynamic-only paths (the route driver
# reuses the path helper for its absence proof). Base R only, no heavy dependency
# named. Definitions only on source; the deletion happens when the cleanup is
# called, so sourcing this module is side-effect free.
#
# It deletes exactly the four named paths and nothing else: it never touches the
# committed decision file, the gate record RDS, or the status manifest.

# The four explicitly typed paths span state, diagnostics, and figures.
logvar_egarch_dynamic_artifacts <- function(
  pilot_state = artifact_path("egarch_pilot_state"),
  results_csv = artifact_path("egarch_results_csv"),
  results_rds = artifact_path("egarch_results_rds"),
  bounds_figure = artifact_path("egarch_bounds_figure")
) {
  unname(c(pilot_state, results_csv, results_rds, bounds_figure))
}

# Delete the four dynamic-only artifacts unconditionally and return a typed audit
# of existed/deleted flags. `existed` is measured before deletion; `deleted` is
# TRUE only for a path that existed and is gone afterward, so an absent path
# reports existed = FALSE, deleted = FALSE (never a spurious success). unlink is
# called once over the vector; the decision, gate, and status files are not in
# the set and so are never at risk.
logvar_egarch_cleanup <- function(
  pilot_state = artifact_path("egarch_pilot_state"),
  results_csv = artifact_path("egarch_results_csv"),
  results_rds = artifact_path("egarch_results_rds"),
  bounds_figure = artifact_path("egarch_bounds_figure")
) {
  paths <- logvar_egarch_dynamic_artifacts(
    pilot_state, results_csv, results_rds, bounds_figure
  )
  existed <- file.exists(paths)
  unlink(paths)
  gone <- !file.exists(paths)
  deleted <- existed & gone
  list(
    out_dir = out_dir,
    artifacts = data.frame(
      path = paths, existed = existed, deleted = deleted,
      stringsAsFactors = FALSE
    ),
    n_existed = sum(existed),
    n_deleted = sum(deleted),
    all_absent = all(gone)
  )
}
