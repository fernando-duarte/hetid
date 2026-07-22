paper_source_once(paper_path("support", "reporting", "inference.R"))
paper_source_once(paper_path("support", "statistics", "api.R"))
paper_source_once(paper_path("log_variance", "inference", "set_envelope.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_core.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_builders.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_gate.R"))
paper_source_once(paper_path("log_variance", "inference", "set_bootstrap_reuse.R"))
paper_source_once(paper_path(
  "log_variance", "inference", "set_bootstrap_artifacts.R"
))

log_var_eq_set_boot <- local({
  stopifnot(is.finite(boot_reps), boot_reps >= 2L)
  inference_alpha <- PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha
  ests <- paper_logvar_estimator_ids(
    capability = "set_bootstrap",
    primary = TRUE
  )
  estimator_results <- stats::setNames(
    lapply(ests, paper_logvar_result),
    ests
  )
  scale_val <-
    estimator_results[["ppml"]]$estimator$metadata$response_scale_value
  logols_val <- stats::setNames(log_var_eq$table$ols, log_var_eq$table$coef)
  builders <- logvar_set_boot_builders(scale_val, logols_val)
  prep <- logvar_set_boot_prepare(set_id_mean_eq, lag_asset_return_pc)
  display_taus <- set_id_mean_eq$tau_display
  disp_key <- vapply(display_taus, paper_tau_key, character(1))
  disp_idx <- seq_along(display_taus) + 1L # slots in spec$taus for the display taus
  spec <- list(
    coefs = log_var_eq$table$coef, gamma = set_id_mean_eq$gamma,
    taus = c(0, display_taus), # tau = 0 first (point acceptance diagnostic)
    x_cols = set_id_mean_eq$x_cols, y1_col = set_id_mean_eq$y1_col,
    y2_cols = set_id_mean_eq$y2_cols, z_col = z_col,
    impose_null = impose_beta2r_null, pc_cols = prep$pc_cols,
    grid_cap = logvar_boot_grid_cap, fit_budget = logvar_boot_fit_budget,
    estimator_ids = ests, builders = builders
  )
  anchor <- logvar_set_boot_draw(prep$data, spec)
  anchor_live <- function(est) {
    any(unlist(lapply(anchor[[est]], function(r) {
      c(r$lower_status, r$upper_status) != PAPER_ENDPOINT_STATUS[["failed"]]
    })))
  }
  stopifnot(
    all(vapply(ests, anchor_live, logical(1))),
    all(vapply(ests, function(est) {
      identical(
        estimator_results[[est]]$sets[[
          paper_tau_key(set_id_mean_eq$tau_baseline)
        ]]$coef,
        spec$coefs
      )
    }, logical(1)))
  )
  full <- lapply(ests, function(est) {
    lapply(anchor[[est]], function(rec) {
      data.frame(
        coef = spec$coefs, set_lower = rec$lower, set_upper = rec$upper,
        lower_status = rec$lower_status, upper_status = rec$upper_status,
        stringsAsFactors = FALSE
      )
    })
  })
  names(full) <- ests
  n <- nrow(prep$data)
  block <- paper_mbb_block_len(n)
  sens_block <- 2L * block # doubled-block robustness diagnostic; a knob nobody turns
  sens_reps <- boot_reps # full B -- no reduced-rep exception
  draw_logvar <- function(index, draw_id) {
    logvar_set_boot_draw(
      prep$data[index, , drop = FALSE],
      spec
    )
  }
  freshness <- logvar_boot_freshness(
    prep, spec, scale_val, logols_val,
    n, block, sens_block, boot_reps, sens_reps, boot_seed
  )
  t0 <- Sys.time()
  disp <- paper_boot_cached_or_run(
    mode = PAPER_BOOT_MODE,
    artifact_key = "log_variance_bootstrap_draws",
    freshness = freshness,
    fields = c(
      "index_sha", "sens_index_sha", "input_sha", "draw_spec_sha",
      "code_sha", "runtime_sha", "cache_schema_version"
    ),
    run_fn = function() {
      logvar_boot_run_bundle(
        draw_logvar, spec, ests, n, block, sens_block,
        boot_reps, sens_reps, boot_seed, boot_cores
      )
    },
    validate_fn = logvar_boot_cache_validate,
    warn_label = "vol set-endpoint bootstrap"
  )
  collected <- disp$draws$collected
  sens_collected <- disp$draws$sens_collected
  n_failed <- disp$draws$n_failed
  prim_cells <- logvar_boot_failure_gate(collected, ests, "primary")
  sens_cells <- logvar_boot_failure_gate(sens_collected, ests, "sensitivity")
  endpoint_stability <-
    PAPER_INFERENCE_SEARCH_CONTROL$logvar_endpoint$stability_share
  envelope <- function(coll) {
    e <- lapply(ests, function(est) {
      ee <- lapply(seq_along(display_taus), function(d) {
        logvar_endpoint_envelope(
          coll[[est]][[disp_idx[d]]], full[[est]][[disp_idx[d]]],
          alpha = inference_alpha, stability = endpoint_stability
        )
      })
      names(ee) <- disp_key
      ee
    })
    names(e) <- ests
    e
  }
  prim_env <- envelope(collected)
  c_sim <- lapply(ests, function(est) {
    stats::setNames(vapply(seq_along(display_taus), function(d) {
      logvar_simultaneous_critical(
        collected[[est]][[disp_idx[d]]], full[[est]][[disp_idx[d]]],
        alpha = inference_alpha, stability = endpoint_stability
      )
    }, numeric(1)), disp_key)
  })
  names(c_sim) <- ests
  n_reported <- sum(vapply(ests, function(est) {
    sum(vapply(prim_env[[est]], function(df) sum(df$reason == "reported"), integer(1)))
  }, integer(1)))
  se_type <- stats::setNames(
    vapply(
      ests,
      function(est) PAPER_REPORTING_CONTROL[[est]]$se_type,
      character(1)
    ),
    ests
  )
  se_obj <- estimator_results
  tau0 <- logvar_boot_tau0_diagnostics(ests, collected, se_obj, se_type, spec)
  sens_env <- envelope(sens_collected)
  provenance <- logvar_boot_provenance(
    n, boot_reps, block, sens_block, sens_reps, boot_seed
  )
  log_var_eq_set_boot <- c(
    prim_env,
    list(
      b_reps = boot_reps, block = block,
      seed = boot_seed,
      sens_block = sens_block, sens_reps = sens_reps,
      inference_contract =
        PAPER_ANALYSIS_CONTRACT$inference,
      coverage_target = "whole-set outer envelope",
      c_sim = c_sim, tau0 = tau0, provenance = provenance
    )
  )
  write_logvar_set_boot_artifacts(
    ests = ests,
    display_taus = display_taus,
    disp_key = disp_key,
    disp_idx = disp_idx,
    prim_env = prim_env,
    c_sim = c_sim,
    full = full,
    se_obj = se_obj,
    sens_env = sens_env,
    tau0 = tau0,
    spec = spec,
    prim_cells = prim_cells,
    sens_cells = sens_cells
  )
  cat(sprintf(
    "vol set-endpoint bootstrap [%s]: B = %d, block = %d, %d failed, %d reported",
    disp$source, boot_reps, block, n_failed, n_reported
  ))
  cat(sprintf(
    " cells, %.1f min\n",
    as.numeric(difftime(Sys.time(), t0, units = "mins"))
  ))
  log_var_eq_set_boot
})
