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

# TRUE, or a reason string, for whether a cached vol set-endpoint bootstrap
# payload carries what a reuse needs to stand in for a fresh resample: the
# primary and sensitivity collected draw sets (the expensive per-draw
# endpoint estimates), matched across the same estimators and tau axis, with
# a closed status vocabulary and the row counts the provenance records. full,
# anchors, and envelopes are recomputed on reuse, so they are not required here.
logvar_boot_cache_validate <- function(cached) {
  need <- c("collected", "sens_collected", "provenance")
  miss <- setdiff(need, names(cached))
  if (length(miss)) {
    return(sprintf("missing cache fields: %s", paste(miss, collapse = ", ")))
  }
  ests <- names(cached$collected)
  if (!length(ests) || !identical(names(cached$sens_collected), ests)) {
    return("primary and sensitivity collections disagree on estimators")
  }
  reps_ok <- function(coll, reps) {
    if (!is.numeric(reps) || length(reps) != 1L || is.na(reps)) {
      return(FALSE)
    }
    all(vapply(ests, function(est) {
      cells <- coll[[est]]
      length(cells) > 0L && all(vapply(cells, function(cell) {
        is.matrix(cell$lower) && nrow(cell$lower) == reps
      }, logical(1)))
    }, logical(1)))
  }
  if (!reps_ok(cached$collected, cached$provenance$b_reps)) {
    return("primary draws do not match b_reps")
  }
  if (!reps_ok(cached$sens_collected, cached$provenance$sens_reps)) {
    return("sensitivity draws do not match sens_reps")
  }
  same_tau <- all(vapply(ests, function(est) {
    length(cached$collected[[est]]) == length(cached$sens_collected[[est]])
  }, logical(1)))
  if (!same_tau) {
    return("primary and sensitivity tau axes disagree")
  }
  status_ok <- tryCatch(
    {
      for (coll in list(cached$collected, cached$sens_collected)) {
        for (est in ests) {
          for (cell in coll[[est]]) {
            paper_endpoint_status_validate(c(cell$lower_status, cell$upper_status))
          }
        }
      }
      TRUE
    },
    error = function(e) FALSE
  )
  if (!status_ok) {
    return("endpoint status vocabulary invalid")
  }
  TRUE
}
