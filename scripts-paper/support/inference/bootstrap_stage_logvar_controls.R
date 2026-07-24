# Validation mechanics for volatility controls stored by the stage owner.

bootstrap_stage_controls_ok <- function(logvar) {
  policy <- logvar$pc_preprocessing
  controls <- logvar[c("search_control", "ppml_control", "harvey_control")]
  se_types <- logvar$se_types
  bootstrap_stage_count_ok(logvar$grid_cap) &&
    bootstrap_stage_count_ok(logvar$fit_budget) &&
    bootstrap_stage_record_ok(policy, c("center", "scale")) &&
    all(vapply(policy, function(value) {
      is.logical(value) && length(value) == 1L && !is.na(value) &&
        is.null(attributes(value))
    }, logical(1))) &&
    all(vapply(controls, function(value) {
      is.list(value) && length(value) > 0L &&
        bootstrap_stage_record_ok(value, names(value))
    }, logical(1))) &&
    identical(names(logvar$search_control), names(LOGVAR_SEARCH_CONTROL)) &&
    identical(names(logvar$ppml_control), names(LOGVAR_PPML_CONTROL)) &&
    identical(names(logvar$harvey_control), names(LOGVAR_HARVEY_CONTROL)) &&
    bootstrap_stage_scalar_ok(logvar$normal_log_square_gap) &&
    is.character(se_types) && !anyNA(se_types) &&
    identical(attributes(se_types), list(names = logvar$estimator_ids))
}
