# Regularized log projections of squared residuals (registry ids log_plus and
# log_fuller, the hetid evaluate_log_projection methods) mapped over the mean
# equation's warm-refined display-tau news sets through the shared set engine:
# reference and Lewbel-point columns with analytic SEs, a full-lattice primary
# scan, an independent five-start audit, nesting demotion, the tuning
# sensitivity at the baseline slack, and the endpoint-diagnostics CSV. The
# bootstrap inference runs later, in the unified stage. The modules are
# sourced here so run_pipeline.R gains one line; run after the Harvey stage and
# before the residual diagnostics, which read each estimator's point fit.

paper_source_once(paper_path("log_variance", "estimators", "ppml", "input_contract.R"))
paper_source_once(paper_path("log_variance", "estimators", "set_orchestration.R"))
paper_source_once(paper_path("log_variance", "estimators", "audit_orchestration.R"))
paper_source_once(paper_path("log_variance", "estimators", "ppml", "coverage.R"))
paper_source_once(paper_path("log_variance", "tables", "console_formatting.R"))
paper_source_once(paper_path(
  "log_variance", "estimators", "log_projection", "estimator.R"
))
paper_source_once(paper_path(
  "log_variance", "estimators", "log_projection", "set_mapping.R"
))
paper_source_once(paper_path(
  "log_variance", "estimators", "log_projection", "diagnostics.R"
))
paper_source_once(paper_path("log_variance", "estimators", "log_projection", "tuning.R"))
paper_source_once(paper_path(
  "log_variance", "estimators", "log_projection", "standard_errors.R"
))

# Guarded orchestration: runs only with the upstream benchmark objects present, so
# sourcing this offline (the estimator test suites) defines the helpers only.
if (exists("log_var_eq") && exists("set_id_mean_eq") && exists("mean_eq_bounds_tau")) {
  lp_map_ctx <- logvar_prepare_map_context(
    log_var_eq$inputs, log_var_eq$sample_contract, set_id_mean_eq,
    mean_eq_bounds_tau, LOGVAR_LOG_PROJECTION_CONTROL$registry_grid_cap
  )
  lp_ctx <- list(
    prep = log_var_eq$log_projection_prep, sample_id = log_var_eq$sample_id,
    # the exogenous-news OLS coefficients: by Frisch-Waugh-Lovell their
    # residuals are the naive OLS residuals of the reference column
    b_ref = stats::setNames(
      set_id_mean_eq$theta_table$ols, set_id_mean_eq$theta_table$coef
    ),
    b_point = lp_map_ctx$b_point, point_feasible = lp_map_ctx$point_feasible,
    search_seed = logvar_map_anchor(
      lp_map_ctx$point_feasible, lp_map_ctx$b_point, lp_map_ctx$grid_base
    ),
    qs_fn = mean_quadratic_system_factory(set_id_mean_eq),
    bounds_tau = mean_eq_bounds_tau
  )
  stopifnot(identical(names(lp_ctx$b_ref), colnames(lp_ctx$prep$w2)))
  lp_taus <- set_id_mean_eq$tau_display
  lp_maps <- list()
  for (lp_id in paper_logvar_estimator_ids(capability = "log_projection")) {
    lp_mapped <- logvar_log_projection_sets(
      lp_id, hetid::LOG_PROJECTION_CONTROL$MULTIPLIER, lp_taus, lp_ctx
    )
    lp_maps[[lp_id]] <- lp_mapped
    lp_result <- c(
      logvar_set_result_core(
        qtr = lp_map_ctx$qtr, sample_id = log_var_eq$sample_id,
        coef_labels = lp_mapped$estimator$coef_labels,
        reference = lp_mapped$reference, point = lp_mapped$point,
        baseline_tau = set_id_mean_eq$tau_baseline,
        primary_results = lp_mapped$primary, final_results = lp_mapped$final,
        w1 = lp_map_ctx$w1, w2 = lp_map_ctx$w2, search_seed = lp_ctx$search_seed
      ),
      list(
        estimator = lp_mapped$estimator, multiplier = lp_mapped$multiplier,
        audit = lp_mapped$audit, nesting = lp_mapped$nesting,
        endpoints = logvar_log_projection_endpoint_rows(
          lp_mapped, lp_ctx$prep, lp_ctx
        ),
        se = logvar_log_projection_se_columns(
          lp_mapped, lp_ctx$prep, lp_ctx,
          PAPER_REPORTING_CONTROL[[lp_id]]$hac_lags
        ),
        scale = list(
          n_mean = attr(lp_ctx$prep, "n_mean"), n_vol = attr(lp_ctx$prep, "n_vol"),
          s_hat = exp(lp_ctx$prep$log_scale_common / 2),
          scale_lower_certified = lp_ctx$prep$scale_lower_certified
        )
      )
    )
    assign(paper_logvar_estimator_spec(lp_id)$result_object, lp_result)
    logvar_bounds_tau_registry[[length(logvar_bounds_tau_registry) + 1L]] <-
      logvar_bounds_registry_entry(
        estimator = lp_mapped$estimator, results = lp_mapped$final,
        b_seed = lp_ctx$search_seed,
        engine_opts = list(
          max_grid_points = LOGVAR_LOG_PROJECTION_CONTROL$registry_grid_cap,
          cache = lp_mapped$cache
        )
      )
    logvar_print_map_summary(
      paper_logvar_estimator_spec(lp_id)$display_name, lp_result, lp_taus,
      census = log_var_eq$n_cross,
      census_label = "census comparability (benchmark n_cross by tau)"
    )
    logvar_print_audit_summary(lp_mapped$audit, "audit")
    logvar_se_report(
      lp_result$se, paper_logvar_estimator_spec(lp_id)$display_name,
      LOGVAR_LOG_PROJECTION_SE_TYPES, PAPER_REPORTING_CONTROL[[lp_id]]$se_type,
      PAPER_REPORTING_CONTROL[[lp_id]]$hac_lags, c("hac", "naive")
    )
  }
  lp_tuning <- logvar_log_projection_tuning(
    lp_maps, lp_ctx, set_id_mean_eq$tau_baseline
  )
  log_var_eq_log_projection_tuning <- lp_tuning$table
  log_var_eq_log_projection_endpoints <- do.call(rbind, c(
    lapply(names(lp_maps), function(id) {
      paper_logvar_result(id)$endpoints
    }),
    lapply(lp_tuning$maps, logvar_log_projection_endpoint_rows,
      prep = lp_ctx$prep, ctx = lp_ctx
    )
  ))
  paper_write_typed_csv(
    log_var_eq_log_projection_endpoints,
    artifact_path("log_projection_endpoint_diagnostics"),
    "log projection endpoints"
  )
  rm(
    lp_map_ctx, lp_ctx, lp_taus, lp_maps, lp_id, lp_mapped, lp_result,
    lp_tuning
  )
}
