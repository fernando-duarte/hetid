bootstrap_stage_assert <- function(checks) {
  passed <- vapply(checks, isTRUE, logical(1))
  if (!all(passed)) stop(names(checks)[which(!passed)[1L]], call. = FALSE)
  invisible(TRUE)
}
bootstrap_stage_stop <- function(message, subclass) {
  condition <- structure(
    list(message = message, call = NULL),
    class = c(subclass, "bootstrap_stage_error", "error", "condition")
  )
  stop(condition)
}
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_logvar_controls.R"
))

bootstrap_stage_search_control_guard <- function(logvar_spec) {
  if (!is.list(logvar_spec) ||
    !identical(logvar_spec$search_control, LOGVAR_SEARCH_CONTROL)) {
    bootstrap_stage_stop(
      "bootstrap stage search-control snapshot differs from its runtime owner",
      "bootstrap_stage_search_control_error"
    )
  }
  invisible(TRUE)
}

bootstrap_stage_static <- function(value) {
  forbidden <- c("closure", "environment", "externalptr", "weakref")
  if (typeof(value) %in% forbidden) {
    return(FALSE)
  }
  children <- if (is.list(value)) unclass(value) else list()
  all(vapply(c(children, attributes(value)), bootstrap_stage_static, logical(1)))
}

bootstrap_stage_count_ok <- function(value, minimum = 1L) {
  is.integer(value) && length(value) == 1L && !is.na(value) &&
    is.null(attributes(value)) && value >= minimum
}
bootstrap_stage_record_ok <- function(value, fields) {
  is.list(value) && identical(attributes(value), list(names = fields))
}
bootstrap_stage_matrix_attrs_ok <- function(value) {
  expected <- list(dim = dim(value))
  if (!is.null(dimnames(value))) expected$dimnames <- dimnames(value)
  identical(attributes(value), expected)
}
bootstrap_stage_axis_ok <- function(value) {
  is.character(value) && length(value) > 0L && !anyNA(value) &&
    is.null(attributes(value)) && all(nzchar(value)) && !anyDuplicated(value)
}
bootstrap_stage_scalar_ok <- function(value) {
  is.numeric(value) && length(value) == 1L && !is.na(value) &&
    is.null(attributes(value)) && is.finite(value)
}

bootstrap_stage_failure_control_ok <- function(value, fields) {
  bootstrap_stage_record_ok(value, fields) &&
    is.double(value$fatal_failure_share) &&
    length(value$fatal_failure_share) == 1L &&
    is.null(attributes(value$fatal_failure_share)) &&
    is.finite(value$fatal_failure_share) && value$fatal_failure_share > 0 &&
    value$fatal_failure_share < 1 &&
    bootstrap_stage_count_ok(value$progress_report_every)
}

bootstrap_stage_frame_ok <- function(frame) {
  fields <- c("data", "key_col", "sample_size")
  data_attrs <- names(attributes(frame$data))
  bootstrap_stage_record_ok(frame, fields) && is.data.frame(frame$data) &&
    identical(data_attrs, c("names", "row.names", "class")) &&
    is.character(frame$key_col) && length(frame$key_col) == 1L &&
    is.null(attributes(frame$key_col)) &&
    frame$key_col %in% names(frame$data) && identical(nrow(frame$data), frame$sample_size) &&
    !anyDuplicated(names(frame$data)) && !anyDuplicated(frame$data[[frame$key_col]]) &&
    all(diff(as.numeric(frame$data[[frame$key_col]])) == 1)
}

bootstrap_stage_system_ok <- function(system, frame) {
  roles <- c(system$y1_col, system$x_cols, system$y2_cols, system$z_col)
  gamma_names <- colnames(system$gamma)
  fields <- c("gamma", "y1_col", "x_cols", "y2_cols", "z_col", "impose_null")
  bootstrap_stage_record_ok(system, fields) &&
    all(vapply(system[c("y1_col", "x_cols", "y2_cols", "z_col")], function(value) {
      is.character(value) && is.null(attributes(value))
    }, logical(1))) && length(system$y1_col) == 1L && length(system$z_col) == 1L &&
    length(system$x_cols) > 0L && length(system$y2_cols) > 0L &&
    bootstrap_stage_axis_ok(roles) && all(roles %in% names(frame$data)) &&
    !anyDuplicated(c(frame$key_col, roles)) && is.matrix(system$gamma) &&
    is.numeric(system$gamma) && bootstrap_stage_matrix_attrs_ok(system$gamma) &&
    !anyNA(system$gamma) && all(is.finite(system$gamma)) &&
    nrow(system$gamma) == 1L && ncol(system$gamma) == length(system$y2_cols) &&
    (is.null(gamma_names) || identical(gamma_names, system$y2_cols)) &&
    is.logical(system$impose_null) && length(system$impose_null) == 1L &&
    !is.na(system$impose_null) && is.null(attributes(system$impose_null))
}

bootstrap_stage_tau_ok <- function(tau, mean) {
  is.double(tau$display) && is.null(attributes(tau$display)) &&
    length(tau$display) > 0L && !anyNA(tau$display) &&
    all(is.finite(tau$display)) && all(tau$display > 0) && !anyDuplicated(tau$display) &&
    identical(tau$baseline, tau$display[[1L]]) && identical(tau$union, c(0, tau$display)) &&
    is.double(mean$tau_star_grid) && is.null(attributes(mean$tau_star_grid)) &&
    !anyNA(mean$tau_star_grid) &&
    all(is.finite(mean$tau_star_grid)) && !is.unsorted(mean$tau_star_grid) &&
    !anyDuplicated(mean$tau_star_grid) && identical(mean$tau_star_grid[[1L]], 0) &&
    bootstrap_stage_count_ok(mean$tau_star_iterations)
}

bootstrap_stage_graph_ok <- function(ids, graph) {
  if (!bootstrap_stage_record_ok(graph, ids)) {
    return(FALSE)
  }
  all(vapply(seq_along(ids), function(index) {
    dependencies <- graph[[index]]
    prior <- if (index == 1L) character() else ids[seq_len(index - 1L)]
    is.character(dependencies) && is.null(attributes(dependencies)) &&
      !anyNA(dependencies) &&
      !anyDuplicated(dependencies) && all(dependencies %in% prior)
  }, logical(1)))
}

bootstrap_stage_logvar_axes_ok <- function(logvar, frame) {
  bootstrap_stage_axis_ok(logvar$coefs) && bootstrap_stage_axis_ok(logvar$pc_cols) &&
    all(logvar$pc_cols %in% names(frame$data)) &&
    bootstrap_stage_axis_ok(logvar$estimator_ids) &&
    bootstrap_stage_graph_ok(logvar$estimator_ids, logvar$estimator_dependencies) &&
    is.numeric(logvar$logols_coef) && all(is.finite(logvar$logols_coef)) &&
    identical(attributes(logvar$logols_coef), list(names = logvar$coefs)) &&
    bootstrap_stage_scalar_ok(logvar$response_scale) && logvar$response_scale > 0
}

bootstrap_stage_complete_case_policy_assert <- function(policy, expected) {
  bootstrap_stage_assert(list("complete-case policy drifted" = identical(policy, expected)))
}

bootstrap_stage_spec_validate <- function(spec, expected) {
  bootstrap_stage_search_control_guard(spec$log_variance)
  shape <- vapply(expected$roots, function(root) {
    bootstrap_stage_record_ok(spec[[root]], expected$fields[[root]])
  }, logical(1))
  design <- spec$design
  logvar <- spec$log_variance
  roundtrip <- unserialize(serialize(spec, NULL, version = expected$rds_version))
  bootstrap_stage_assert(list(
    "stage roots drifted" = bootstrap_stage_record_ok(spec, expected$roots),
    "nested stage fields drifted" = all(shape),
    "frame contract is invalid" = bootstrap_stage_frame_ok(spec$frame),
    "system contract is invalid" = bootstrap_stage_system_ok(spec$system, spec$frame),
    "tau contract is invalid" = bootstrap_stage_tau_ok(spec$tau, spec$mean),
    "mean coefficient axis is invalid" = bootstrap_stage_axis_ok(spec$mean$coefs),
    "failure-control snapshot is invalid" = bootstrap_stage_failure_control_ok(
      design$failure_control, expected$failure_control_fields
    ),
    "family design is invalid" =
      bootstrap_stage_record_ok(design$primary, expected$family_fields) &&
        bootstrap_stage_record_ok(design$sensitivity, expected$family_fields) &&
        identical(design$primary$family, expected$family_names[["primary"]]) &&
        identical(design$sensitivity$family, expected$family_names[["sensitivity"]]) &&
        bootstrap_stage_count_ok(design$seed) &&
        bootstrap_stage_count_ok(design$primary$n_draws, 2L) &&
        bootstrap_stage_count_ok(design$primary$block_length) &&
        bootstrap_stage_count_ok(design$sensitivity$block_length) &&
        identical(
          design$primary$block_length,
          paper_mbb_block_len(spec$frame$sample_size)
        ) &&
        identical(design$sensitivity$n_draws, design$primary$n_draws) &&
        identical(
          design$sensitivity$block_length,
          bootstrap_stage_sensitivity_block_length(
            design$primary$block_length
          )
        ),
    "volatility axes are invalid" = bootstrap_stage_logvar_axes_ok(logvar, spec$frame),
    "volatility controls are invalid" = bootstrap_stage_controls_ok(logvar),
    "complete-case policy is invalid" = identical(
      logvar$complete_case_policy,
      expected$complete_case_policy
    ),
    "canonical stage spec contains runtime state" = bootstrap_stage_static(spec),
    "stage spec is not serializable" = identical(roundtrip, spec)
  ))
  invisible(TRUE)
}

bootstrap_stage_estimator_result_axes_assert <- function(results, ids, tau, coefs) {
  bootstrap_stage_assert(list(
    "estimator results are missing" = all(ids %in% names(results)),
    "estimator axes drifted" = all(vapply(ids, function(id) {
      identical(results[[id]]$sets[[paper_tau_key(tau)]]$coef, coefs)
    }, logical(1)))
  ))
}
