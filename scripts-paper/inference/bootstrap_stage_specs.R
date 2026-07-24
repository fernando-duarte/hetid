paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_spec_assertions.R"
))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_result_inputs.R"
))
paper_source_once(paper_path(
  "support", "inference", "bootstrap_stage_mean_result_inputs.R"
))

BOOTSTRAP_STAGE_FIELDS <- list(
  frame = c("data", "key_col", "sample_size"),
  system = c("gamma", "y1_col", "x_cols", "y2_cols", "z_col", "impose_null"),
  tau = c("baseline", "display", "union"),
  design = c("seed", "failure_control", "primary", "sensitivity"),
  mean = c("coefs", "tau_star_grid", "tau_star_iterations", "result_inputs"),
  log_variance = c(
    "coefs", "pc_cols", "complete_case_policy", "grid_cap", "fit_budget",
    "estimator_ids", "estimator_dependencies", "response_scale", "logols_coef",
    "pc_preprocessing", "search_control", "ppml_control", "harvey_control",
    "normal_log_square_gap", "se_types", "result_inputs"
  )
)
BOOTSTRAP_STAGE_ROOTS <- names(BOOTSTRAP_STAGE_FIELDS)
BOOTSTRAP_STAGE_FAMILY_FIELDS <- c("family", "n_draws", "block_length")
BOOTSTRAP_STAGE_FAILURE_CONTROL_FIELDS <- c(
  "fatal_failure_share", "progress_report_every"
)
BOOTSTRAP_STAGE_COMPLETE_CASE_POLICY <- list(
  shared_rows = "all",
  timing = "after_shared_estimation",
  columns_role = "pc_cols",
  predicate_id = "stats::complete.cases",
  subset_roles = c("w1", "w2", "key", "pc_data")
)

bootstrap_stage_sensitivity_block_length <- function(primary_block) {
  stopifnot(bootstrap_stage_count_ok(primary_block))
  sensitivity_block <- 2L * primary_block
  stopifnot(bootstrap_stage_count_ok(sensitivity_block))
  sensitivity_block
}

bootstrap_stage_expected <- function() {
  list(
    fields = BOOTSTRAP_STAGE_FIELDS,
    roots = BOOTSTRAP_STAGE_ROOTS,
    family_fields = BOOTSTRAP_STAGE_FAMILY_FIELDS,
    family_names = paper_mbb_protocol()$family_names[
      c("primary", "sensitivity")
    ],
    failure_control_fields = BOOTSTRAP_STAGE_FAILURE_CONTROL_FIELDS,
    complete_case_policy = BOOTSTRAP_STAGE_COMPLETE_CASE_POLICY,
    rds_version = PAPER_SERIALIZATION_CONTROL$rds_version
  )
}

bootstrap_stage_frame <- function(mean_eq, lag_pc) {
  key_col <- PAPER_ANALYSIS_CONTRACT$model$key_col
  mean_data <- mean_eq$data
  bootstrap_stage_assert(list(
    "mean frame must be a data frame" = is.data.frame(mean_data),
    "PC frame must be a data frame" = is.data.frame(lag_pc),
    "mean key is missing" = key_col %in% names(mean_data),
    "PC key is missing" = key_col %in% names(lag_pc),
    "mean keys duplicate" = !anyDuplicated(mean_data[[key_col]]),
    "PC keys duplicate" = !anyDuplicated(lag_pc[[key_col]]),
    "key classes differ" = identical(class(mean_data[[key_col]]), class(lag_pc[[key_col]])),
    "mean sample count drifted" = identical(nrow(mean_data), mean_eq$sample$n),
    "bootstrap quarters are not gapless" = all(diff(as.numeric(mean_data[[key_col]])) == 1)
  ))
  bootstrap_stage_assert(list(
    "return-PC names are duplicated" = !anyDuplicated(names(lag_pc))
  ))
  pc_cols <- setdiff(names(lag_pc), key_col)
  bootstrap_stage_assert(list(
    "return-PC axis drifted" = identical(
      pc_cols, PAPER_ANALYSIS_CONTRACT$model$return_pc_cols
    ),
    "PC names collide with mean columns" = !length(intersect(pc_cols, names(mean_data)))
  ))
  matched <- match(mean_data[[key_col]], lag_pc[[key_col]])
  pc_data <- lag_pc[matched, pc_cols, drop = FALSE]
  names(pc_data) <- pc_cols
  data <- mean_data
  data[pc_cols] <- pc_data
  bootstrap_stage_assert(list(
    "augmented row count changed" = identical(nrow(data), nrow(mean_data)),
    "augmented key changed" = identical(data[[key_col]], mean_data[[key_col]]),
    "original mean data changed" = all(vapply(names(mean_data), function(name) {
      identical(data[[name]], mean_data[[name]])
    }, logical(1))),
    "augmented column order changed" = identical(names(data), c(names(mean_data), pc_cols))
  ))
  list(data = data, key_col = key_col, sample_size = nrow(data))
}

bootstrap_stage_dependency_graph <- function() {
  ids <- paper_logvar_estimator_ids(capability = "set_bootstrap", primary = TRUE)
  graph <- stats::setNames(lapply(ids, function(id) {
    paper_logvar_estimator_spec(id)$dependencies
  }), ids)
  bootstrap_stage_assert(list(
    "estimator dependency graph is not ordered" = bootstrap_stage_graph_ok(ids, graph)
  ))
  graph
}

bootstrap_stage_spec <- function(
  mean_eq, log_var_eq, lag_pc, estimator_results,
  n_draws, seed, z_col, impose_null, grid_cap, fit_budget
) {
  frame <- bootstrap_stage_frame(mean_eq, lag_pc)
  dependencies <- bootstrap_stage_dependency_graph()
  ids <- names(dependencies)
  pc_cols <- PAPER_ANALYSIS_CONTRACT$model$return_pc_cols
  family_names <- paper_mbb_protocol()$family_names[
    c("primary", "sensitivity")
  ]
  scale <- estimator_results[["ppml"]]$estimator$metadata$response_scale_value
  display <- mean_eq$tau_display
  block_length <- paper_mbb_block_len(frame$sample_size)
  spec <- list(
    frame = frame,
    system = list(
      gamma = mean_eq$gamma, y1_col = mean_eq$y1_col, x_cols = mean_eq$x_cols,
      y2_cols = mean_eq$y2_cols, z_col = z_col, impose_null = impose_null
    ),
    tau = list(baseline = mean_eq$tau_baseline, display = display, union = c(0, display)),
    design = list(
      seed = seed, failure_control = PAPER_INFERENCE_SEARCH_CONTROL$bootstrap,
      primary = list(
        family = family_names[["primary"]], n_draws = n_draws,
        block_length = block_length
      ),
      sensitivity = list(
        family = family_names[["sensitivity"]], n_draws = n_draws,
        block_length = bootstrap_stage_sensitivity_block_length(block_length)
      )
    ),
    mean = list(
      coefs = c(mean_eq$beta1_table$coef, mean_eq$theta_table$coef),
      tau_star_grid = unique(c(
        seq(0, mean_eq$tau_cap, by = PAPER_ANALYSIS_CONTRACT$tau$bootstrap_step),
        mean_eq$tau_cap
      )),
      tau_star_iterations = PAPER_INFERENCE_SEARCH_CONTROL$
        tau_star$bootstrap_bisection_iterations,
      result_inputs = bootstrap_stage_mean_result_inputs(mean_eq)
    ),
    log_variance = list(
      coefs = log_var_eq$table$coef, pc_cols = pc_cols,
      complete_case_policy = BOOTSTRAP_STAGE_COMPLETE_CASE_POLICY,
      grid_cap = grid_cap, fit_budget = fit_budget, estimator_ids = ids,
      estimator_dependencies = dependencies, response_scale = scale,
      logols_coef = stats::setNames(log_var_eq$table$ols, log_var_eq$table$coef),
      pc_preprocessing = PAPER_ANALYSIS_CONTRACT$model$preprocessing$return_pc,
      search_control = LOGVAR_SEARCH_CONTROL, ppml_control = LOGVAR_PPML_CONTROL,
      harvey_control = LOGVAR_HARVEY_CONTROL,
      normal_log_square_gap = LOGVAR_NORMAL_LOG_SQUARE_GAP,
      se_types = stats::setNames(vapply(
        ids,
        function(id) PAPER_REPORTING_CONTROL[[id]]$se_type,
        character(1)
      ), ids),
      result_inputs = NULL
    )
  )
  spec$log_variance$result_inputs <- bootstrap_stage_result_inputs(
    estimator_results, ids, display, spec$log_variance$coefs,
    spec$log_variance$se_types
  )
  bootstrap_stage_estimator_result_axes_assert(
    estimator_results, ids, spec$tau$baseline, spec$log_variance$coefs
  )
  bootstrap_stage_spec_validate(spec, bootstrap_stage_expected())
  spec
}

bootstrap_stage_spec_projection <- function(spec) {
  projection <- spec
  projection$frame$data <- NULL
  projection
}

bootstrap_stage_spec_hashes <- function(spec) {
  list(
    input_sha = paper_sha256_object(spec$frame$data),
    draw_spec_sha = paper_sha256_object(bootstrap_stage_spec_projection(spec))
  )
}

bootstrap_stage_failure_limit <- function(n_draws, failure_control) {
  paper_bootstrap_failure_limit(
    n_draws,
    control = list(bootstrap = failure_control)
  )
}
