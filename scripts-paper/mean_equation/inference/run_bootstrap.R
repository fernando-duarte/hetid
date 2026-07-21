# Moving-block bootstrap for the set-identified mean equation: resample the
# aligned estimation frame of estimate_identified_set.R in block-quarter blocks,
# re-run the full estimation per draw using the paper-owned bootstrap core,
# and summarize with robust endpoint
# scales, Stoye-calibrated nominal intervals, a robust tau = 0 point
# interval, and a tau* percentile range. Deterministic given boot_seed
# (run_pipeline.R constants). Every display tau is evaluated in every draw; the
# per-draw tau* uses a coarse 0.05-step bracket (cap forced onto the grid)
# plus a short bisection. Writes the draw matrices and a per-cell diagnostics
# table to the typed state and diagnostics directories.
# Run after mean-set estimation.

set_id_boot <- local({
  paper_source_once(paper_path("support", "identification", "api.R"))
  paper_source_once(paper_path("support", "identification", "profile_solver_core.R"))
  paper_source_once(paper_path("support", "identification", "profile_bounds_api.R"))
  paper_source_once(paper_path("support", "identification", "tau_star.R"))
  paper_source_once(paper_path("support", "statistics", "api.R"))
  paper_source_once(paper_path("support", "reporting", "inference.R"))
  paper_source_once(paper_path("support", "identification", "identified_set_inference.R"))
  paper_source_once(paper_path("support", "identification", "identified_set_bootstrap.R"))

  stopifnot(is.finite(boot_reps), boot_reps >= 2L)
  inference_alpha <- PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha
  # moving blocks assume a gapless quarterly index; a dropped interior quarter
  # would silently stitch non-adjacent periods into one block. as.numeric(), not
  # as.integer(): tsibble's yearquarter has no as.integer method in the pinned
  # tsibble version, so as.integer() errors on the vctrs default cast.
  stopifnot(all(diff(as.numeric(set_id_mean_eq$data$qtr)) == 1L))
  block <- paper_mbb_block_len(set_id_mean_eq$sample$n)

  boot_spec <- list(
    coefs = c(set_id_mean_eq$beta1_table$coef, set_id_mean_eq$theta_table$coef),
    gamma = set_id_mean_eq$gamma,
    taus = set_id_mean_eq$tau_display,
    tau_grid = unique(
      c(
        seq(
          0,
          set_id_mean_eq$tau_cap,
          by = PAPER_ANALYSIS_CONTRACT$tau$bootstrap_step
        ),
        set_id_mean_eq$tau_cap
      )
    ),
    y1_col = set_id_mean_eq$y1_col, x_cols = set_id_mean_eq$x_cols,
    y2_cols = set_id_mean_eq$y2_cols, z_col = z_col,
    impose_null = impose_beta2r_null
  )

  # indices drawn up front: the resampling stream cannot be perturbed if a
  # solver ever consumes random numbers mid-draw, and a parallel lapply over
  # boot_idx stays a drop-in change
  boot_run <- paper_run_mbb_draws(
    n_draws = boot_reps,
    sample_size = set_id_mean_eq$sample$n,
    block_length = block,
    draw = function(index, draw_id) {
      set_id_boot_draw(
        set_id_mean_eq$data[index, ],
        boot_spec
      )
    },
    seed = boot_seed,
    cores = boot_cores,
    progress = paper_mbb_console_progress(
      PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$progress_report_every,
      "endpoint bootstrap"
    )
  )
  boot_idx <- boot_run$indices
  boot_t0 <- boot_run$started_at
  boot_raw <- boot_run$draws

  collected <- set_id_boot_collect(boot_raw, boot_spec)
  # a large failure share means the resampling itself is broken, not
  # statistical unboundedness, so the pipeline stops rather than reporting
  # scales built from the surviving minority
  if (collected$n_failed > 0L) {
    cat("  failed draws by cause:\n")
    print(collected$failure_causes)
  }
  if (collected$n_failed > paper_bootstrap_failure_limit(boot_reps)) {
    stop(
      "endpoint bootstrap: ", collected$n_failed, " of ", boot_reps,
      " draws failed"
    )
  }
  names(collected$endpoint_draws) <- names(set_id_mean_eq$set_tables)

  inference <- lapply(seq_along(boot_spec$taus), function(j) {
    st <- set_id_mean_eq$set_tables[[j]]
    endpoint_inference(
      collected$endpoint_draws[[j]]$lower, collected$endpoint_draws[[j]]$upper,
      rbind(st$beta1, st$theta),
      alpha = inference_alpha,
      control = PAPER_INFERENCE_SEARCH_CONTROL
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

  # referee-facing reproducibility metadata: the exact resampler, block rule,
  # and draw-index fingerprint behind every number this file reports
  provenance <- list(
    resampler = "circular_mbb",
    sample_size = set_id_mean_eq$sample$n,
    b_reps = boot_reps, block = block, seed = boot_seed,
    rng_kind = boot_run$rng_kind,
    block_rule = "ceiling(1.5*T^(1/3))",
    index_sha256 = paper_sha256_object(boot_idx)
  )

  set_id_boot <- c(
    list(
      b_reps = boot_reps, block = block, seed = boot_seed,
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
      provenance = provenance
    ),
    collected
  )

  diagnostics <- set_id_boot_diagnostics(
    collected, inference, set_id_mean_eq$set_tables, boot_spec$taus
  )
  diagnostics <- cbind(
    paper_inference_metadata_frame(nrow(diagnostics)),
    diagnostics
  )
  paper_write_typed_csv(
    diagnostics,
    artifact_path("mean_inference_diagnostics"),
    "mean_inference_diagnostics"
  )
  paper_write_exact_rds(
    set_id_boot[c(
      "b_reps", "block", "seed", "inference_contract", "point_draws",
      "endpoint_draws", "tau_star_draws", "provenance"
    )],
    artifact_path("mean_bootstrap_draws"),
    "mean_bootstrap_draws"
  )

  cat(sprintf(
    "endpoint bootstrap: B = %d, block = %d, %.1f min; tau* range [%s, %s] (n = %d)\n",
    boot_reps, block,
    as.numeric(difftime(Sys.time(), boot_t0, units = "mins")),
    paper_format_general(
      set_id_boot$tau_star_band[["lower"]],
      PAPER_REPORTING_CONTROL$precision$console_significant
    ),
    paper_format_general(
      set_id_boot$tau_star_band[["upper"]],
      PAPER_REPORTING_CONTROL$precision$console_significant
    ),
    set_id_boot$tau_star_band[["n"]]
  ))
  cat(sprintf(
    "  %d failed, %d capped, %d point-deficient; bounded at baseline in %.0f%% of draws\n",
    set_id_boot$n_failed, set_id_boot$n_capped, set_id_boot$n_point_deficient,
    100 * set_id_boot$tau_star_share_bounded
  ))
  print(
    set_id_boot$inference[[1]],
    digits =
      PAPER_REPORTING_CONTROL$precision$console_significant
  )

  set_id_boot
})
