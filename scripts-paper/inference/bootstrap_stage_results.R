# Deterministic reconstruction of the two public bootstrap result objects.
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_result_helpers.R"
))

bootstrap_stage_mean_result <- function(
  stage, stage_spec, set_id_mean_eq, source, elapsed_minutes
) {
  alpha <- PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha
  result <- mean_boot_results(
    stage$mean, set_id_mean_eq, alpha,
    PAPER_INFERENCE_SEARCH_CONTROL,
    bootstrap_stage_mean_provenance(stage)
  )
  diagnostics <- set_id_boot_diagnostics(
    result, result$inference, set_id_mean_eq$set_tables,
    stage_spec$tau$display
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
  digits <- PAPER_REPORTING_CONTROL$precision$console_significant
  cat(sprintf(
    paste0(
      "endpoint bootstrap [%s]: B = %d, block = %d, %.1f min; ",
      "tau* range [%s, %s] (n = %d)\n"
    ),
    source, result$b_reps, result$block, elapsed_minutes,
    paper_format_general(result$tau_star_band[["lower"]], digits),
    paper_format_general(result$tau_star_band[["upper"]], digits),
    result$tau_star_band[["n"]]
  ))
  cat(sprintf(
    paste0(
      "  %d failed, %d capped, %d point-deficient; ",
      "bounded at baseline in %.0f%% of draws\n"
    ),
    result$n_failed, result$n_capped,
    result$n_point_deficient,
    100 * result$tau_star_share_bounded
  ))
  print(result$inference[[1L]], digits = digits)
  result
}

bootstrap_stage_logvar_result <- function(
  stage, stage_spec, estimator_results, source, elapsed_minutes
) {
  spec <- stage_spec$log_variance
  ids <- spec$estimator_ids
  layout <- bootstrap_stage_display_layout(stage_spec)
  control <- stage_spec$design$failure_control
  primary_cells <- logvar_boot_failure_gate(
    stage$volatility_primary, ids, "primary", control
  )
  sensitivity_cells <- logvar_boot_failure_gate(
    stage$volatility_sensitivity, ids, "sensitivity", control
  )
  full <- bootstrap_stage_anchor_frames(stage$anchor, spec)
  stopifnot(all(vapply(ids, function(id) {
    identical(
      estimator_results[[id]]$sets[[
        paper_tau_key(stage_spec$tau$baseline)
      ]]$coef,
      spec$coefs
    )
  }, logical(1))))
  alpha <- PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha
  stability <- PAPER_INFERENCE_SEARCH_CONTROL$
    logvar_endpoint$stability_share
  primary <- bootstrap_stage_envelopes(
    stage$volatility_primary, full, ids,
    layout, alpha, stability
  )
  simultaneous <- lapply(ids, function(id) {
    stats::setNames(vapply(seq_along(layout$taus), function(index) {
      slot <- layout$slots[[index]]
      logvar_simultaneous_critical(
        stage$volatility_primary[[id]][[slot]],
        full[[id]][[slot]],
        alpha = alpha,
        stability = stability
      )
    }, numeric(1)), layout$keys)
  })
  names(simultaneous) <- ids
  se_type <- spec$se_types
  tau0 <- logvar_boot_tau0_diagnostics(
    ids, stage$volatility_primary,
    estimator_results, se_type, spec
  )
  sensitivity <- bootstrap_stage_envelopes(
    stage$volatility_sensitivity, full, ids,
    layout, alpha, stability
  )
  provenance <- bootstrap_stage_logvar_provenance(stage)
  result <- c(primary, list(
    b_reps = provenance$b_reps,
    block = provenance$block,
    seed = provenance$seed,
    sens_block = provenance$sens_block,
    sens_reps = provenance$sens_reps,
    inference_contract = PAPER_ANALYSIS_CONTRACT$inference,
    coverage_target = "whole-set outer envelope",
    c_sim = simultaneous,
    tau0 = tau0,
    provenance = provenance
  ))
  write_logvar_set_boot_artifacts(
    ids, layout$taus, layout$keys, layout$slots,
    primary, simultaneous, full, estimator_results,
    sensitivity, tau0, spec, primary_cells,
    sensitivity_cells
  )
  n_reported <- sum(vapply(ids, function(id) {
    sum(vapply(primary[[id]], function(value) {
      sum(value$reason == "reported")
    }, integer(1)))
  }, integer(1)))
  cat(sprintf(
    paste0(
      "vol set-endpoint bootstrap [%s]: B = %d, block = %d, ",
      "%d failed, %d reported cells, %.1f min\n"
    ),
    source, result$b_reps, result$block,
    stage$volatility_primary_n_failed,
    n_reported, elapsed_minutes
  ))
  result
}

bootstrap_stage_results <- function(
  stage, stage_spec, set_id_mean_eq,
  estimator_results, source, elapsed_minutes
) {
  mean_inputs <- bootstrap_stage_mean_result_inputs_current(
    set_id_mean_eq, stage_spec
  )
  logvar_inputs <- bootstrap_stage_result_inputs_current(
    estimator_results, stage_spec
  )
  list(
    set_id_boot = bootstrap_stage_mean_result(
      stage, stage_spec, mean_inputs, source, elapsed_minutes
    ),
    log_var_eq_set_boot = bootstrap_stage_logvar_result(
      stage, stage_spec, logvar_inputs, source, elapsed_minutes
    )
  )
}
