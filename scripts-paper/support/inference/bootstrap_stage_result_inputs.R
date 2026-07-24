bootstrap_stage_result_inputs <- function(
  estimator_results, estimator_ids, taus, coefs, se_types
) {
  keys <- vapply(taus, paper_tau_key, character(1))
  stats::setNames(lapply(estimator_ids, function(estimator_id) {
    result <- estimator_results[[estimator_id]]
    se_type <- se_types[[estimator_id]]
    list(
      sets = lapply(result$sets[keys], function(set) {
        set[c("coef", "set_lower", "set_upper", "status")]
      }),
      se = list(point = result$se$point[c("coef", se_type)])
    )
  }), estimator_ids)
}

bootstrap_stage_result_inputs_current <- function(estimator_results, stage_spec) {
  spec <- stage_spec$log_variance
  actual <- bootstrap_stage_result_inputs(
    estimator_results, spec$estimator_ids, stage_spec$tau$display,
    spec$coefs, spec$se_types
  )
  if (!identical(actual, spec$result_inputs)) {
    bootstrap_stage_stop(
      "bootstrap stage result inputs drifted",
      "bootstrap_stage_result_input_error"
    )
  }
  spec$result_inputs
}
