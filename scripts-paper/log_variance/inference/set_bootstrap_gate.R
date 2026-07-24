# Post-collection quality control for the vol-equation set-endpoint bootstrap:
# the structured-status failure gate applied to both the primary and the
# sensitivity run. A solver
# that returns an all-"failed" endpoint schema never surfaces as a character
# (error) draw, so it stays invisible to a transport-level failed-draw count;
# this gate reads the collected status matrices directly instead. Consumed by
# scripts-paper/inference/run_bootstrap_stage.R.

# Failed-status count and share in one estimator/tau cell, pooled across every
# draw, coefficient, and side.
logvar_cell_failure <- function(cell) {
  failed <- c(cell$lower_status, cell$upper_status) == PAPER_ENDPOINT_STATUS[["failed"]]
  c(count = sum(failed), share = mean(failed))
}

logvar_boot_failure_cells <- function(collected, estimator_ids) {
  cells <- lapply(estimator_ids, function(estimator_id) {
    t(vapply(collected[[estimator_id]], logvar_cell_failure, numeric(2)))
  })
  names(cells) <- estimator_ids
  cells
}

logvar_boot_failure_bad_count <- function(cells, failure_control) {
  limit <- failure_control$fatal_failure_share
  sum(vapply(cells, function(value) {
    sum(value[, "share"] > limit)
  }, integer(1)))
}

# Per-estimator/per-tau structured-failure gate: stop()s the run by name when
# any one cell's failed share exceeds the canonical fatal-failure threshold
# (the same field paper_bootstrap_failure_limit reads for the transport-level
# count), and otherwise returns the per-cell count/share matrices so the
# caller can persist them to diagnostics.
logvar_boot_failure_gate <- function(
  collected, ests, run_label,
  failure_control = PAPER_INFERENCE_SEARCH_CONTROL$bootstrap
) {
  cells <- logvar_boot_failure_cells(collected, ests)
  limit <- failure_control$fatal_failure_share
  n_bad <- logvar_boot_failure_bad_count(cells, failure_control)
  if (n_bad > 0L) {
    stop(sprintf(
      "%s vol set-endpoint bootstrap: %d estimator/tau cell(s) exceed a %.0f%% failure share",
      run_label, n_bad, 100 * limit
    ))
  }
  cells
}
