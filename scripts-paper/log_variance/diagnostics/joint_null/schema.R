# Fixed schema and generated parameter fields for joint-null rows.

paper_source_once(paper_path("config", "analysis_contract.R"))

LOGVAR_JOINT_NULL_SCHEMA <- list(
  schema_version = "1.0.0",
  estimator = "logols",
  diagnostic = "joint_null_theta_r",
  inference_status = "deferred"
)

logvar_joint_null_schema_header <- function() {
  LOGVAR_JOINT_NULL_SCHEMA
}

.jn_row_template <- function() {
  fields <- PAPER_ANALYSIS_CONTRACT$model$artifact_fields
  c(
    logvar_joint_null_schema_header()[c(
      "schema_version", "estimator", "diagnostic"
    )],
    list(
      sample_id = NA_character_,
      inference_status = LOGVAR_JOINT_NULL_SCHEMA$inference_status,
      tau_order = NA_integer_, tau_display = NA_real_,
      tau_internal = NA_real_,
      scaled_l2 = NA_real_, scaled_linf = NA_real_
    ),
    paper_named_doubles(fields$mean),
    list(theta0 = NA_real_),
    paper_named_doubles(fields$return),
    list(
      min_abs_eps = NA_real_, nearest_crossing_qtr = NA_character_,
      constraint_residual = NA_real_, raw_gradient_norm = NA_real_,
      grid_points = NA_integer_, n_unavailable_grid = NA_integer_,
      polish_starts = NA_integer_, successful_polishes = NA_integer_,
      argmin_source = NA_character_, winning_start_type = NA_character_,
      n_agreeing_starts = NA_integer_, start_agreement_max_scaled = NA_real_,
      basin_count = NA_integer_, replication_status = NA_character_,
      perturbation_status = NA_character_, stability_status = NA_character_,
      n_near_crossing_rows = NA_integer_, status = NA_character_,
      membership_result = NA_character_, root_tol = NA_real_
    ),
    paper_named_doubles(fields$scale),
    list(
      box_active = NA, sparse_grid = NA,
      metric_l2_source = NA_character_, metric_linf_source = NA_character_,
      l2_rank_under_linf = NA_integer_, linf_rank_under_l2 = NA_integer_,
      d_inf_l2 = NA_real_, d_inf_grid_best = NA_real_,
      linf_gap_abs = NA_real_, linf_gap_rel = NA_real_, linf_trigger = NA,
      linf_sensitivity_status = NA_character_
    )
  )
}

.jn_assemble_row <- function(vals) {
  row <- .jn_row_template()
  fixed <- intersect(names(vals), names(LOGVAR_JOINT_NULL_SCHEMA))
  if (length(fixed)) {
    stop(sprintf(
      "joint-null row: fixed field override(s) %s",
      paste(fixed, collapse = ", ")
    ))
  }
  unknown <- setdiff(names(vals), names(row))
  if (length(unknown)) {
    stop(sprintf(
      "joint-null row: unknown field(s) %s",
      paste(unknown, collapse = ", ")
    ))
  }
  for (nm in names(vals)) row[[nm]] <- unname(vals[[nm]])
  row
}

.jn_parameter_fields <- function(b, theta, scales) {
  fields <- PAPER_ANALYSIS_CONTRACT$model$artifact_fields
  index <- PAPER_ANALYSIS_CONTRACT$model$index
  c(
    paper_named_doubles(fields$mean, b),
    list(theta0 = theta[[index$theta_intercept]]),
    paper_named_doubles(fields$return, theta[index$theta_return]),
    paper_named_doubles(fields$scale, scales[index$scale])
  )
}
