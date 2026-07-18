# Typed row schema for the joint-GMM artifact.

.jg_csv_template <- function() {
  fields <- PAPER_ANALYSIS_CONTRACT$model$artifact_fields
  c(
    logvar_joint_gmm_schema_header()[c(
      "schema_version", "diagnostic"
    )],
    list(
      sample_id = NA_character_,
      joint_input_id = NA_character_,
      decision_spec_id = NA_character_,
      spec_id = NA_character_,
      inference_status = LOGVAR_JOINT_GMM_SCHEMA$inference_status,
      moment_block = NA_character_,
      tau_order = NA_integer_,
      tau_display = NA_real_,
      tau_internal = NA_real_,
      numerical_status = NA_character_,
      search_status = NA_character_,
      coverage_status = NA_character_,
      rho_scaled_linf = NA_real_
    ),
    paper_named_doubles(fields$mean),
    list(theta0 = NA_real_),
    paper_named_doubles(fields$return),
    list(
      a_l = NA_real_,
      a_p = NA_real_,
      gap_a_p_a_l = NA_real_
    ),
    paper_named_doubles(fields$variance),
    list(
      fresh_moment_residual = NA_real_,
      fresh_constraint_residual = NA_real_,
      crossing_count = NA_integer_,
      min_abs_eps = NA_real_,
      eps_floor = NA_real_,
      guard_slack = NA_real_,
      floor_active = NA,
      floor_sensitivity_status = NA_character_,
      patterns_observed = NA_integer_,
      patterns_polished = NA_integer_,
      argmin_source = NA_character_,
      winning_start_type = NA_character_,
      optimizer_status = NA_character_,
      box_half_width = NA_real_,
      box_active = NA,
      box_audit_status = NA_character_,
      n_obs = NA_integer_,
      n_added_moments = NA_integer_,
      n_search_parameters = NA_integer_,
      n_moments_unprofiled = NA_integer_,
      n_parameters_unprofiled = NA_integer_,
      n_moments_profiled = NA_integer_,
      n_parameters_profiled = NA_integer_,
      jac_rank_unprofiled = NA_integer_,
      jac_rank_profiled = NA_integer_,
      jac_min_sv_unprofiled = NA_real_,
      jac_min_sv_profiled = NA_real_,
      jac_status_unprofiled = NA_character_,
      jac_status_profiled = NA_character_,
      active_constraint_count = NA_integer_,
      active_constraint_rank = NA_integer_,
      moment_active_stacked_rank = NA_integer_,
      algebraic_moment_excess = NA_integer_,
      param_xtol_rel = NA_real_,
      constraint_tol = NA_real_,
      root_tol = NA_real_,
      moment_delta = NA_real_,
      skip_reason = NA_character_
    )
  )
}

logvar_joint_gmm_csv_fields <- names(.jg_csv_template())

logvar_joint_gmm_csv_row <- function(result) {
  row <- .jg_csv_template()
  fixed <- names(logvar_joint_gmm_schema_header())
  attempted <- intersect(names(result), fixed)
  if (length(attempted)) {
    stop(
      sprintf(
        "joint-GMM row cannot override fixed fields: %s",
        paste(attempted, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  for (nm in setdiff(names(row), fixed)) {
    v <- result[[nm]]
    if (!is.null(v) && length(v) == 1L) row[[nm]] <- v
  }
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}
