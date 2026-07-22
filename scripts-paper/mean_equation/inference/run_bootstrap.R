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
  paper_source_once(paper_path("mean_equation", "inference", "boot_results.R"))

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

  freshness <- mean_boot_freshness(set_id_mean_eq, boot_spec, boot_reps, block, boot_seed)
  t0 <- Sys.time()
  disp <- paper_boot_cached_or_run(
    mode = PAPER_BOOT_MODE,
    artifact_key = "mean_bootstrap_draws",
    freshness = freshness,
    fields = c(
      "index_sha", "input_sha", "draw_spec_sha", "code_sha",
      "runtime_sha", "cache_schema_version"
    ),
    run_fn = function() {
      # indices drawn up front: the resampling stream cannot be perturbed if a
      # solver ever consumes random numbers mid-draw, and parallelizing the
      # per-draw work stays a drop-in change
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
      collected <- set_id_boot_collect(boot_run$draws, boot_spec)
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
      collected # gate passes BEFORE this is cached
    },
    validate_fn = mean_boot_cache_validate, warn_label = "endpoint bootstrap"
  )
  collected <- disp$draws
  # referee-facing reproducibility metadata: the exact resampler, block rule,
  # and draw-index fingerprint behind every number this file reports;
  # recomputed rather than read off the resample so reuse and rerun agree
  # byte for byte
  provenance <- mean_boot_provenance(set_id_mean_eq, boot_reps, block, boot_seed)

  set_id_boot <- mean_boot_results(
    collected, set_id_mean_eq, inference_alpha,
    PAPER_INFERENCE_SEARCH_CONTROL, provenance
  )

  diagnostics <- set_id_boot_diagnostics(
    set_id_boot, set_id_boot$inference, set_id_mean_eq$set_tables, boot_spec$taus
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
  cat(sprintf(
    "endpoint bootstrap [%s]: B = %d, block = %d, %.1f min; tau* range [%s, %s] (n = %d)\n",
    disp$source, boot_reps, block,
    as.numeric(difftime(Sys.time(), t0, units = "mins")),
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
