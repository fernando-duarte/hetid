# Post-collection quality control for the vol-equation set-endpoint bootstrap:
# the doubled-block full-B sensitivity rerun, and the structured-status
# failure gate applied to both the primary and the sensitivity run. A solver
# that returns an all-"failed" endpoint schema never surfaces as a character
# (error) draw, so it stays invisible to a transport-level failed-draw count;
# this gate reads the collected status matrices directly instead. Consumed by
# scripts-paper/log_variance/inference/run_set_bootstrap.R.

# Doubled-block robustness diagnostic at the unified full-B convention: same
# reps/seed/cores as the primary run, just a longer block -- a knob nobody
# turns, run so the report can state whether the envelope is stable to it.
logvar_boot_sensitivity_run <- function(
  draw_logvar, n, sens_block, sens_reps, boot_seed, boot_cores
) {
  paper_run_mbb_draws(
    n_draws = sens_reps,
    sample_size = n,
    block_length = sens_block,
    cores = boot_cores,
    seed = boot_seed,
    draw = draw_logvar,
    progress = paper_mbb_console_progress(
      PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$progress_report_every,
      "vol set-endpoint sensitivity bootstrap"
    )
  )
}

# Failed-status count and share in one estimator/tau cell, pooled across every
# draw, coefficient, and side.
logvar_cell_failure <- function(cell) {
  failed <- c(cell$lower_status, cell$upper_status) == PAPER_ENDPOINT_STATUS[["failed"]]
  c(count = sum(failed), share = mean(failed))
}

# Per-estimator/per-tau structured-failure gate: stop()s the run by name when
# any one cell's failed share exceeds the canonical fatal-failure threshold
# (the same field paper_bootstrap_failure_limit reads for the transport-level
# count), and otherwise returns the per-cell count/share matrices so the
# caller can persist them to diagnostics.
logvar_boot_failure_gate <- function(collected, ests, run_label) {
  cells <- lapply(ests, function(est) {
    t(vapply(collected[[est]], logvar_cell_failure, numeric(2)))
  })
  names(cells) <- ests
  limit <- PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$fatal_failure_share
  n_bad <- sum(vapply(cells, function(m) sum(m[, "share"] > limit), integer(1)))
  if (n_bad > 0L) {
    stop(sprintf(
      "%s vol set-endpoint bootstrap: %d estimator/tau cell(s) exceed a %.0f%% failure share",
      run_label, n_bad, 100 * limit
    ))
  }
  cells
}
