# Fitted-volatility envelopes for the PPML and Harvey variance estimators at the
# baseline mean-equation slack. The computation and rendering stay generic;
# this driver only registers the two already-computed estimator objects.

paper_source_once(paper_path("log_variance", "figures", "fitted_volatility", "adapter.R"))
paper_source_once(paper_path("log_variance", "figures", "fitted_volatility", "envelope.R"))
paper_source_once(paper_path("log_variance", "figures", "fitted_volatility", "plot.R"))

fitted_vol_tau <- set_id_mean_eq$tau_baseline
fitted_vol_key <- paper_tau_key(fitted_vol_tau)
fitted_vol_b_tab <- mean_eq_bounds_tau[[fitted_vol_key]]
stopifnot(!is.null(fitted_vol_b_tab))
fitted_vol_qs <- tau_quadratic_system(
  set_id_mean_eq$gamma, fitted_vol_tau, set_id_mean_eq$moments
)
fitted_vol_x <- logvar_design_matrix(
  log_var_eq$inputs$pcr,
  PAPER_ANALYSIS_CONTRACT$model$return_pc_cols
)
fitted_vol_sample_id <- logvar_sample_id(
  log_var_eq$inputs$qtr, log_var_eq$inputs$w1,
  log_var_eq$inputs$w2, log_var_eq$inputs$pcr
)
stopifnot(
  identical(fitted_vol_sample_id, log_var_eq$sample_id),
  identical(log_var_eq$inputs$qtr, log_var_eq$sample_contract$qtr)
)
fitted_vol_point <- set_id_mean_eq$theta_table$point
if (anyNA(fitted_vol_point) ||
  .feasibility_residual(
    fitted_vol_qs, fitted_vol_point,
    rep(1, length(fitted_vol_qs$A_i))
  ) > 0) {
  fitted_vol_point <- NULL
}

fitted_vol_entry <- function(estimator) {
  hit <- vapply(logvar_bounds_tau_registry, function(entry) {
    identical(entry$estimator$metadata$estimator, estimator)
  }, logical(1))
  stopifnot(sum(hit) == 1L)
  logvar_bounds_tau_registry[[which(hit)]]
}

fitted_vol_estimators <- paper_logvar_estimator_ids(
  capability = "fitted_volatility",
  primary = TRUE
)
log_var_eq_fitted_volatility <- stats::setNames(
  lapply(fitted_vol_estimators, function(estimator) {
    entry <- fitted_vol_entry(estimator)
    stopifnot(identical(entry$estimator$metadata$sample_id, fitted_vol_sample_id))
    envelope <- logvar_fitted_vol_envelope(
      entry$estimator, log_var_eq$inputs$qtr, fitted_vol_x,
      fitted_vol_qs, fitted_vol_b_tab,
      b_seed = entry$b_seed, b_point = fitted_vol_point,
      tau = fitted_vol_tau,
      source_cache = entry$engine_opts$cache,
      expected_sample_id = fitted_vol_sample_id,
      max_grid_points = entry$engine_opts$max_grid_points,
      max_fit_evals = logvar_fitted_vol_fit_budget,
      starts_per_side =
        LOGVAR_SEARCH_CONTROL$fitted_vol_starts_per_side
    )
    path <- logvar_fitted_vol_path(estimator)
    logvar_fitted_vol_render(envelope, path)
    envelope$output_path <- path
    cat(sprintf(
      "fitted-volatility envelope (%s): %d dates, %d two-sided; wrote %s\n",
      estimator, nrow(envelope$data),
      sum(
        envelope$data$lower_status == "bounded" &
          envelope$data$upper_status == "bounded"
      ),
      path
    ))
    envelope
  }),
  fitted_vol_estimators
)

rm(
  fitted_vol_tau, fitted_vol_key, fitted_vol_b_tab, fitted_vol_qs,
  fitted_vol_x, fitted_vol_sample_id, fitted_vol_point, fitted_vol_entry,
  fitted_vol_estimators
)
