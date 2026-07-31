# Cheap results layer for the mean-equation endpoint bootstrap: turns a
# collected draw set (set_id_boot_collect's output) into the set_id_boot
# object the pipeline reports --
# per-tau endpoint targets, the tau = 0 t statistics, and the percentile
# bands. Resampling belongs to the unified bootstrap stage; everything here is
# a deterministic function of `collected`.
# boot_band and PAPER_ANALYSIS_CONTRACT are sourced by the runner's local()
# prologue before this file's functions are called; the two shared construction
# modules are sourced here because both panels must reach the same code, not a
# per-panel copy of it.
paper_source_once(paper_path(
  "support", "inference_post", "endpoint_target_cells.R"
))
paper_source_once(paper_path(
  "support", "inference_post", "endpoint_point_statistic.R"
))

# Assemble set_id_boot from one bootstrap's collected draws. Provenance is
# projected from the canonical stage record and passed through unchanged.
mean_boot_results <- function(collected, set_id_mean_eq, inference_alpha,
                              provenance) {
  names(collected$endpoint_draws) <- names(set_id_mean_eq$set_tables)

  # the shared reference distribution, identical to the one the volatility
  # panel quantiles, so the two panels' parentheses are comparable
  inference <- lapply(seq_along(set_id_mean_eq$tau_display), function(j) {
    st <- set_id_mean_eq$set_tables[[j]]
    endpoint_target_table(
      collected$endpoint_draws[[j]],
      rbind(st$beta1, st$theta),
      alpha = inference_alpha,
      tau = set_id_mean_eq$tau_display[[j]]
    )
  })
  names(inference) <- names(set_id_mean_eq$set_tables)

  # the tau = 0 cell (point_t_statistic, endpoint_point_statistic.R): the
  # closed-form point over the robust scale of the point draws, gated by the
  # same absolute-count and stability rules as the set cells
  point_hat <- c(set_id_mean_eq$beta1_table$point, set_id_mean_eq$theta_table$point)
  point_t <- point_t_statistic(
    point_hat,
    collected$point_draws,
    collected$point_status
  )
  point_se <- stats::setNames(point_t$se, point_t$coef)

  c(
    list(
      b_reps = provenance$b_reps, block = provenance$block, seed = provenance$seed,
      inference_contract = PAPER_ANALYSIS_CONTRACT$inference,
      point_se = point_se, point_t = point_t,
      point_band = apply(
        collected$point_draws,
        2,
        boot_band,
        alpha = inference_alpha
      ),
      inference = inference,
      provenance = provenance
    ),
    collected
  )
}
