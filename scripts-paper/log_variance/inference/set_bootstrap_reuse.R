# Reuse machinery for the vol-equation set-endpoint bootstrap: the one cacheable
# bundle of expensive resamples, the recomputable referee provenance, the
# expensive-path code manifest, and the freshness fingerprint the dispatcher
# matches. Sourced by run_set_bootstrap.R.

# The two expensive resamples as one bundle, in the same order the runner used
# before (primary transport-failure gate, primary structured gate, then the
# doubled-block sensitivity run and its structured gate) so a broken fresh run
# stop()s before anything is cached. Returns the two collected draw sets and the
# primary transport-failure count the console summary reports. The per-cell gates
# and every envelope are recomputed by the caller (for the diagnostics CSV, and
# to re-gate a reuse); running the gate here too is cheap and keeps the failure
# ordering intact.
logvar_boot_run_bundle <- function(draw_logvar, spec, ests, n, block, sens_block,
                                   boot_reps, sens_reps, boot_seed, boot_cores) {
  boot_run <- paper_run_mbb_draws(
    n_draws = boot_reps, sample_size = n, block_length = block,
    cores = boot_cores, seed = boot_seed, draw = draw_logvar,
    progress = paper_mbb_console_progress(
      PAPER_INFERENCE_SEARCH_CONTROL$bootstrap$progress_report_every,
      "vol set-endpoint bootstrap"
    )
  )
  n_failed <- sum(vapply(boot_run$draws, is.character, logical(1)))
  if (n_failed > paper_bootstrap_failure_limit(boot_reps)) {
    stop("vol set-endpoint bootstrap: ", n_failed, " of ", boot_reps, " draws failed")
  }
  collected <- logvar_set_boot_collect(boot_run$draws, spec)
  logvar_boot_failure_gate(collected, ests, "primary")
  sens_run <- logvar_boot_sensitivity_run(
    draw_logvar, n, sens_block, sens_reps, boot_seed, boot_cores
  )
  sens_collected <- logvar_set_boot_collect(sens_run$draws, spec)
  logvar_boot_failure_gate(sens_collected, ests, "sensitivity")
  list(collected = collected, sens_collected = sens_collected, n_failed = n_failed)
}

# Referee-facing reproducibility metadata, built without a resample so reuse and
# rerun agree byte for byte: the pinned RNG triple and seed/protocol give the
# same primary draw indices, hence the same index_sha256, whether this run
# resampled or loaded a cache. Equals the old inline provenance field-for-field.
logvar_boot_provenance <- function(n, boot_reps, block, sens_block, sens_reps, boot_seed) {
  list(
    resampler = "circular_mbb", sample_size = n,
    b_reps = boot_reps, block = block, seed = boot_seed,
    rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"),
    block_rule = "ceiling(1.5*T^(1/3))",
    index_sha256 = paper_boot_index_sha(n, block, boot_reps, boot_seed),
    sens_block = sens_block, sens_reps = sens_reps
  )
}

# Every source file whose code a per-draw re-estimation executes. The estimator
# and engine subtrees are globbed so a new solver/likelihood/contract file is
# caught automatically; over-inclusion (a report-only file, or the identification
# inference layer the vol draw does not touch) is the safe direction -- a
# needless rerun reproduces identical numbers, while a missed file would
# silently reuse stale draws. Three named files sit outside the globbed
# subtrees but still run on every draw: log_ols/estimator.R's logvar_sample_id
# (the md5 sample guard both ppml and harvey stamp into their metadata),
# normalizations.R's Harvey normal-log gap constant, and logvar_estimators.R's
# per-estimator dependency lookup.
logvar_boot_code_manifest <- function() {
  glob_dirs <- c(
    "support/identification",
    "log_variance/core", "log_variance/engine",
    "log_variance/estimators/ppml", "log_variance/estimators/harvey"
  )
  globbed <- unlist(lapply(glob_dirs, function(rel) {
    parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    dir_abs <- do.call(paper_path, as.list(parts))
    file.path(rel, list.files(dir_abs, pattern = "\\.R$"))
  }), use.names = FALSE)
  named <- c(
    "log_variance/estimators/shared.R",
    "log_variance/estimators/controls.R",
    "log_variance/estimators/set_orchestration.R",
    "log_variance/estimators/log_ols/estimator.R",
    "log_variance/inference/set_bootstrap_core.R",
    "log_variance/inference/set_bootstrap_builders.R",
    "config/analysis_contract.R",
    "config/logvar_estimators.R",
    "support/statistics/normalizations.R"
  )
  c(globbed, named)
}

# Freshness fingerprint the dispatcher matches to decide reuse. The primary and
# sensitivity index hashes, the estimation frame, the serializable draw spec (the
# builders' scale/log-OLS parameters stand in for the non-hashable closures), the
# expensive-path code, and the runtime each gate reuse. b_reps, sens_reps, block,
# seed, and sample_size ride along as informational fields the cache validator
# reads back (logvar_boot_cache_validate sizes the draw matrices off
# provenance$b_reps / provenance$sens_reps); they are not matched fields.
logvar_boot_freshness <- function(prep, spec, scale_val, logols_val,
                                  n, block, sens_block, boot_reps, sens_reps, boot_seed) {
  list(
    index_sha = paper_boot_index_sha(n, block, boot_reps, boot_seed),
    sens_index_sha = paper_boot_index_sha(n, sens_block, sens_reps, boot_seed),
    input_sha = paper_sha256_object(prep$data),
    draw_spec_sha = paper_sha256_object(list(
      spec = spec[c(
        "coefs", "gamma", "taus", "x_cols", "y1_col", "y2_cols", "z_col",
        "impose_null", "pc_cols", "grid_cap", "fit_budget", "estimator_ids"
      )],
      scale_val = scale_val, logols_val = logols_val
    )),
    code_sha = paper_boot_code_sha(logvar_boot_code_manifest()),
    runtime_sha = paper_boot_runtime_sha(),
    cache_schema_version = 1L,
    b_reps = boot_reps, sens_reps = sens_reps, block = block,
    seed = boot_seed, sample_size = n
  )
}
