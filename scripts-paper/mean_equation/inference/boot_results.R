# Cheap results layer for the mean-equation endpoint bootstrap: turns a
# collected draw set (set_id_boot_collect's output) into the set_id_boot
# object the pipeline reports and writes to the typed state directory --
# per-tau endpoint inference, the tau = 0 point interval, and the percentile
# bands. Resampling itself stays in run_bootstrap.R; everything here is a
# deterministic function of `collected`, so a later cache reuse can rebuild
# set_id_boot from a validated cached payload without resampling.
# endpoint_inference, point_inference, boot_band, PAPER_ANALYSIS_CONTRACT, and
# PAPER_INFERENCE_SEARCH_CONTROL are sourced by the runner's local() prologue
# before this file's functions are called.

# Assemble set_id_boot from one bootstrap's collected draws. `provenance` is
# built by the caller (it needs the resample's rng_kind and draw indices,
# which only exist after the resample) and passed through unchanged; b_reps,
# block, and seed are read off provenance rather than off the runner's own
# locals, so the assembly has no dependency on anything but its arguments.
mean_boot_results <- function(collected, set_id_mean_eq, inference_alpha,
                              control, provenance) {
  names(collected$endpoint_draws) <- names(set_id_mean_eq$set_tables)

  inference <- lapply(seq_along(set_id_mean_eq$tau_display), function(j) {
    st <- set_id_mean_eq$set_tables[[j]]
    endpoint_inference(
      collected$endpoint_draws[[j]]$lower, collected$endpoint_draws[[j]]$upper,
      rbind(st$beta1, st$theta),
      alpha = inference_alpha,
      control = control
    )
  })
  names(inference) <- names(set_id_mean_eq$set_tables)

  # robust tau = 0 point inference (point_inference, identified_set_inference.R):
  # closed-form point plus/minus the two-sided normal quantile times the robust
  # scale of the point draws, gated by the same half-the-draws rule as the
  # set cells
  point_hat <- c(set_id_mean_eq$beta1_table$point, set_id_mean_eq$theta_table$point)
  point_tab <- point_inference(
    point_hat,
    collected$point_draws,
    alpha = inference_alpha
  )
  point_se <- stats::setNames(point_tab$se, point_tab$coef)
  point_ci <- point_tab[c("coef", "lower", "upper")]

  c(
    list(
      b_reps = provenance$b_reps, block = provenance$block, seed = provenance$seed,
      inference_contract = PAPER_ANALYSIS_CONTRACT$inference,
      point_se = point_se, point_ci = point_ci,
      point_band = apply(
        collected$point_draws,
        2,
        boot_band,
        alpha = inference_alpha
      ),
      tau_star_band = boot_band(collected$tau_star_draws, inference_alpha),
      tau_star_share_bounded = mean(
        collected$tau_star_draws > set_id_mean_eq$tau_baseline,
        na.rm = TRUE
      ),
      inference = inference,
      provenance = provenance,
      cache_schema_version = 1L
    ),
    collected
  )
}

# TRUE, or a reason string, for whether a cached mean-bootstrap payload has
# everything a reuse needs to stand in for a fresh resample: the draw
# matrices and failure/cap counts mean_boot_results would otherwise recompute
# from `collected`, the provenance the console/diagnostics reporting reads,
# and a schema version pinned so an older cache never silently masquerades as
# the current shape.
mean_boot_cache_validate <- function(cached) {
  need <- c(
    "point_draws", "endpoint_draws", "tau_star_draws", "n_failed",
    "n_capped", "n_point_deficient", "cache_schema_version", "provenance"
  )
  miss <- setdiff(need, names(cached))
  if (length(miss)) {
    return(sprintf("missing cache fields: %s", paste(miss, collapse = ", ")))
  }
  if (!identical(cached$cache_schema_version, 1L)) {
    return("cache schema version mismatch")
  }
  b <- cached$provenance$b_reps
  ok_rows <- is.matrix(cached$point_draws) && nrow(cached$point_draws) == b
  if (!ok_rows) {
    return("point_draws row count does not match b_reps")
  }
  TRUE
}
