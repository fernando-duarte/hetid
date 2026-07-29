# Fitted-volatility envelopes across the contract's slack sweep, for the same
# primary estimators the baseline figure covers. run.R pins the slack at the
# mean-equation baseline; this driver repeats that object at every swept tau and
# adds one combined panel per estimator (linear and log y) so the nesting of the
# identified sets in tau is visible in a single exhibit.
# Run via run_pipeline.R after fitted_volatility/run.R.

paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "adapter.R"
))
paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "envelope.R"
))
paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "tau_sweep_context.R"
))
paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "tau_sweep_plot.R"
))
paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "tau_sweep_normalized_plot.R"
))

tau_sweep_taus <- logvar_tau_sweep_feasible(
  PAPER_ANALYSIS_CONTRACT$tau$fitted_volatility_sweep,
  set_id_mean_eq$tau_star
)
stopifnot(length(tau_sweep_taus) > 0L)
tau_sweep_boxes <- logvar_tau_sweep_boxes(set_id_mean_eq, tau_sweep_taus)
tau_sweep_x <- logvar_design_matrix(
  log_var_eq$inputs$pcr,
  PAPER_ANALYSIS_CONTRACT$model$return_pc_cols
)
tau_sweep_sample_id <- logvar_sample_id(
  log_var_eq$inputs$qtr, log_var_eq$inputs$w1,
  log_var_eq$inputs$w2, log_var_eq$inputs$pcr
)
stopifnot(identical(tau_sweep_sample_id, log_var_eq$sample_id))
tau_sweep_estimators <- paper_logvar_estimator_ids(
  capability = "fitted_volatility",
  primary = TRUE
)

log_var_eq_fitted_volatility_sweep <- list()
for (tau_sweep_tau in tau_sweep_taus) {
  tau_sweep_ctx <- logvar_tau_sweep_context(
    set_id_mean_eq, tau_sweep_boxes, tau_sweep_tau
  )
  for (tau_sweep_estimator in tau_sweep_estimators) {
    tau_sweep_entry <- logvar_tau_sweep_entry(
      logvar_bounds_tau_registry, tau_sweep_estimator
    )
    tau_sweep_envelope <- logvar_fitted_vol_envelope(
      tau_sweep_entry$estimator, log_var_eq$inputs$qtr, tau_sweep_x,
      tau_sweep_ctx$qs, tau_sweep_ctx$b_tab,
      b_seed = tau_sweep_entry$b_seed, b_point = tau_sweep_ctx$b_point,
      tau = tau_sweep_tau,
      source_cache = tau_sweep_entry$engine_opts$cache,
      expected_sample_id = tau_sweep_sample_id,
      max_grid_points = tau_sweep_entry$engine_opts$max_grid_points,
      max_fit_evals = logvar_fitted_vol_fit_budget,
      starts_per_side = LOGVAR_SEARCH_CONTROL$fitted_vol_starts_per_side
    )
    log_var_eq_fitted_volatility_sweep[[tau_sweep_estimator]][[
      paper_tau_key(tau_sweep_tau)
    ]] <- tau_sweep_envelope
    tau_sweep_path <- logvar_tau_sweep_path(
      tau_sweep_estimator,
      tau = tau_sweep_tau
    )
    logvar_fitted_vol_render(tau_sweep_envelope, tau_sweep_path)
    cat(sprintf(
      "fitted-volatility envelope (%s, tau = %s): %d dates, %d two-sided\n",
      tau_sweep_estimator, format(tau_sweep_tau),
      nrow(tau_sweep_envelope$data),
      sum(
        tau_sweep_envelope$data$lower_status ==
          PAPER_ENDPOINT_STATUS[["bounded"]] &
          tau_sweep_envelope$data$upper_status ==
            PAPER_ENDPOINT_STATUS[["bounded"]]
      )
    ))
  }
}

# combined panels: only meaningful once the sweep carries more than one slack
if (length(tau_sweep_taus) > 1L) {
  for (tau_sweep_estimator in tau_sweep_estimators) {
    for (tau_sweep_log in c(FALSE, TRUE)) {
      tau_sweep_widths <- logvar_tau_sweep_render(
        log_var_eq_fitted_volatility_sweep[[tau_sweep_estimator]],
        logvar_tau_sweep_path(
          tau_sweep_estimator,
          suffix = if (tau_sweep_log) "combined_log" else "combined"
        ),
        log_scale = tau_sweep_log
      )
      cat(sprintf(
        "combined fitted-volatility envelopes (%s, %s y): %d slacks, widths %s\n",
        tau_sweep_estimator, if (tau_sweep_log) "log" else "linear",
        length(tau_sweep_widths),
        paste(sprintf("%.4f", tau_sweep_widths), collapse = "/")
      ))
    }
    # the same envelopes with each endpoint standardized, so the panels compare
    # the shape of the swept endpoints against the point fit rather than their
    # level and width
    for (tau_sweep_side in LOGVAR_TAU_SWEEP_SIDES) {
      tau_sweep_cors <- logvar_tau_sweep_normalized_render(
        log_var_eq_fitted_volatility_sweep[[tau_sweep_estimator]],
        logvar_tau_sweep_path(
          tau_sweep_estimator,
          suffix = paste0("combined_", tau_sweep_side, "_normalized")
        ),
        side = tau_sweep_side
      )
      cat(sprintf(
        "normalized %s envelopes (%s): correlation with the tau = 0 fit %s\n",
        tau_sweep_side, tau_sweep_estimator,
        paste(sprintf("%.3f", tau_sweep_cors), collapse = "/")
      ))
    }
  }
}

rm(
  tau_sweep_taus, tau_sweep_boxes, tau_sweep_x, tau_sweep_sample_id,
  tau_sweep_estimators, tau_sweep_tau, tau_sweep_ctx, tau_sweep_estimator,
  tau_sweep_entry, tau_sweep_envelope, tau_sweep_path, tau_sweep_side,
  tau_sweep_cors
)
