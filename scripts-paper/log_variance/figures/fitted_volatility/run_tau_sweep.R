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
paper_source_once(paper_path(
  "log_variance", "figures", "fitted_volatility", "tau_sweep_extra_panels.R"
))

tau_sweep_taus <- logvar_tau_sweep_feasible(
  PAPER_ANALYSIS_CONTRACT$tau$fitted_volatility_sweep,
  set_id_mean_eq$tau_star
)
stopifnot(length(tau_sweep_taus) > 0L)
tau_sweep_boxes <- logvar_tau_sweep_boxes(set_id_mean_eq, tau_sweep_taus)
tau_sweep_x <- logvar_fitted_vol_design(log_var_eq$inputs$pcr)
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
    # one panel per family: the exponent PC_R' theta_R and its exponential
    for (tau_sweep_logvar in c(FALSE, TRUE)) {
      tau_sweep_widths <- logvar_tau_sweep_render(
        log_var_eq_fitted_volatility_sweep[[tau_sweep_estimator]],
        logvar_tau_sweep_path(
          tau_sweep_estimator,
          suffix = if (tau_sweep_logvar) "combined_log" else "combined"
        ),
        log_variance = tau_sweep_logvar
      )
      cat(sprintf(
        "combined fitted-volatility envelopes (%s, %s): %d slacks, widths %s\n",
        tau_sweep_estimator,
        if (tau_sweep_logvar) "log variance" else "variance",
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

# The swept combined panel again, with the OLS benchmark drawn over the bands:
# the two-step log-OLS map evaluated at the mean equation's OLS news
# coefficients rather than anywhere in the identified set. theta_hat there is
# the OLS coefficient vector of log(eps_hat^2) on (1, PC_R) with eps_hat =
# w1 - W2 b_ols, and tau_sweep_x zeroes the intercept, so the line is the same
# PC_R' theta_R the bands bound.
tau_sweep_ols_line <- logvar_tau_sweep_ols_line(
  set_id_mean_eq, log_var_eq$inputs, tau_sweep_x
)
tau_sweep_ols_widths <- logvar_tau_sweep_render(
  log_var_eq_fitted_volatility_sweep[["ppml"]],
  logvar_tau_sweep_path("ppml", suffix = "combined_log_ols"),
  log_variance = TRUE,
  extra_line = tau_sweep_ols_line
)
cat(sprintf(
  "OLS-benchmark combined panel (ppml): %d slacks, OLS path in [%.3f, %.3f]\n",
  length(tau_sweep_ols_widths),
  min(tau_sweep_ols_line$value), max(tau_sweep_ols_line$value)
))
rm(tau_sweep_ols_line, tau_sweep_ols_widths)

# One panel for the slacks nearest tau*, on its own list so the swept panels
# keep their scale. Same renderer and same log-variance family as the combined
# panel it is modelled on; ppml only, per its single manifest record.
tau_sweep_high <- logvar_tau_sweep_feasible(
  PAPER_FIGURE_RENDER_CONTROL$fitted_volatility_high$taus,
  set_id_mean_eq$tau_star
)
if (length(tau_sweep_high) > 0L) {
  tau_sweep_high_envs <- logvar_tau_sweep_extra_envelopes(
    set_id_mean_eq, logvar_bounds_tau_registry, log_var_eq$inputs,
    tau_sweep_x, tau_sweep_sample_id, tau_sweep_high,
    logvar_fitted_vol_fit_budget
  )
  tau_sweep_high_widths <- logvar_tau_sweep_render(
    tau_sweep_high_envs,
    logvar_tau_sweep_path("ppml", suffix = "combined_high_log"),
    log_variance = TRUE
  )
  cat(sprintf(
    "high-slack combined panel (ppml): %d slacks, widths %s\n",
    length(tau_sweep_high_widths),
    paste(sprintf("%.4f", tau_sweep_high_widths), collapse = "/")
  ))
  rm(tau_sweep_high_envs, tau_sweep_high_widths)
}

rm(
  tau_sweep_high,
  tau_sweep_taus, tau_sweep_boxes, tau_sweep_x, tau_sweep_sample_id,
  tau_sweep_estimators, tau_sweep_tau, tau_sweep_ctx, tau_sweep_estimator,
  tau_sweep_entry, tau_sweep_envelope, tau_sweep_path, tau_sweep_side,
  tau_sweep_cors
)
