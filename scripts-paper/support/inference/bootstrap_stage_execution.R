bootstrap_stage_candidate <- function(
  stage_spec, primary_family, sensitivity_family, cores
) {
  spec <- stage_spec
  mean_collect_spec <- list(
    coefs = spec$mean$coefs,
    taus = spec$tau$display
  )
  logvar_collect_spec <- list(
    coefs = spec$log_variance$coefs,
    taus = spec$tau$union,
    estimator_ids = spec$log_variance$estimator_ids
  )
  control <- spec$design$failure_control
  report_every <- control$progress_report_every
  primary <- paper_run_indexed_draws(
    primary_family,
    function(index, draw_id) {
      bootstrap_stage_primary_indexed_draw(
        index, draw_id, spec
      )
    },
    cores,
    paper_mbb_console_progress(
      report_every, "bootstrap stage primary"
    )
  )
  mean <- set_id_boot_collect(
    bootstrap_stage_project_raw(primary$draws, "mean"),
    mean_collect_spec
  )
  bootstrap_stage_mean_transport_gate(
    mean, primary_family$n_draws, control
  )
  anchor <- bootstrap_stage_volatility_anchor(
    spec$frame$data, spec
  )
  bootstrap_stage_anchor_gate(anchor, spec)
  volatility_raw <- bootstrap_stage_project_raw(
    primary$draws, "volatility"
  )
  primary_n_failed <- as.integer(sum(vapply(
    volatility_raw, is.character, logical(1)
  )))
  bootstrap_stage_volatility_transport_gate(
    primary_n_failed, primary_family$n_draws, control
  )
  volatility_primary <- logvar_set_boot_collect(
    volatility_raw, logvar_collect_spec
  )
  logvar_boot_failure_gate(
    volatility_primary, spec$log_variance$estimator_ids,
    "primary", control
  )
  sensitivity <- paper_run_indexed_draws(
    sensitivity_family,
    function(index, draw_id) {
      bootstrap_stage_volatility_indexed_draw(
        index, draw_id, spec
      )
    },
    cores,
    paper_mbb_console_progress(
      report_every,
      "vol set-endpoint sensitivity bootstrap"
    )
  )
  sensitivity_n_failed <- as.integer(sum(vapply(
    sensitivity$draws, is.character, logical(1)
  )))
  volatility_sensitivity <- logvar_set_boot_collect(
    sensitivity$draws, logvar_collect_spec
  )
  logvar_boot_failure_gate(
    volatility_sensitivity, spec$log_variance$estimator_ids,
    "sensitivity", control
  )
  list(
    anchor = anchor,
    mean = mean,
    volatility_primary = volatility_primary,
    volatility_primary_n_failed = primary_n_failed,
    volatility_sensitivity = volatility_sensitivity,
    volatility_sensitivity_n_failed = sensitivity_n_failed
  )
}
