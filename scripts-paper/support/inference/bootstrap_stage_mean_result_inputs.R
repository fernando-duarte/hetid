bootstrap_stage_mean_result_inputs <- function(mean_eq) {
  point_fields <- c("coef", "point")
  set_fields <- c("coef", "set_lower", "set_upper", "status")
  list(
    beta1_table = mean_eq$beta1_table[point_fields],
    theta_table = mean_eq$theta_table[point_fields],
    set_tables = lapply(mean_eq$set_tables, function(set_table) {
      list(
        beta1 = set_table$beta1[set_fields],
        theta = set_table$theta[set_fields]
      )
    })
  )
}

bootstrap_stage_mean_result_inputs_current <- function(mean_eq, stage_spec) {
  actual <- tryCatch(
    bootstrap_stage_mean_result_inputs(mean_eq),
    error = identity
  )
  expected <- stage_spec$mean$result_inputs
  if (inherits(actual, "error") || !identical(actual, expected)) {
    bootstrap_stage_stop(
      "bootstrap stage mean result inputs drifted",
      "bootstrap_stage_mean_result_input_error"
    )
  }
  c(
    expected,
    list(
      tau_baseline = stage_spec$tau$baseline,
      tau_display = stage_spec$tau$display
    )
  )
}
