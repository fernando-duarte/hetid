# Estimator builders used by each moving-block bootstrap draw. Draw-path code:
# every function here runs inside the resample loop, so this file stays inside
# the manifest that invalidates the draw cache. The two tau = 0 summaries that
# used to live here are pure functions of the collected draws and moved to
# support/inference_post/logvar_point_summaries.R.

logvar_set_boot_builders <- function(
  scale_value,
  logols_coef,
  estimator_ids = paper_logvar_estimator_ids(
    capability = "set_bootstrap",
    primary = TRUE
  ),
  ppml_control = LOGVAR_PPML_CONTROL,
  harvey_control = LOGVAR_HARVEY_CONTROL,
  normal_log_square_gap = LOGVAR_NORMAL_LOG_SQUARE_GAP
) {
  force(scale_value)
  force(logols_coef)
  force(estimator_ids)
  force(ppml_control)
  force(harvey_control)
  force(normal_log_square_gap)
  build_ppml <- function(w1, w2, pcr, qtr, b_point, built) {
    anchor <- if (is.null(b_point)) rep(0, ncol(w2)) else b_point
    logvar_ppml_estimator(
      w1, w2, pcr, qtr,
      b_point = b_point,
      scale_anchor_b = anchor,
      scale_anchor_source = "boot",
      response_scale = scale_value,
      control = ppml_control
    )
  }
  build_harvey <- function(w1, w2, pcr, qtr, b_point, built) {
    ppml_obj <- built[["ppml"]]
    ppml_source_id <- if (!is.null(ppml_obj)) {
      ppml_obj$metadata$spec_id
    } else {
      NULL
    }
    logvar_harvey_estimator(
      w1, w2, pcr, qtr,
      b_point = b_point,
      ppml_bundle = if (!is.null(ppml_obj)) ppml_obj$start_bundle else NULL,
      ppml_start_at_b = if (!is.null(ppml_obj)) ppml_obj$fit_at_b else NULL,
      ppml_bundle_source_id = ppml_source_id,
      ppml_start_at_b_source_id = ppml_source_id,
      logols_coef = logols_coef,
      normal_log_square_gap = normal_log_square_gap,
      control = harvey_control
    )
  }
  builders <- list(ppml = build_ppml, harvey = build_harvey)
  stopifnot(all(estimator_ids %in% names(builders)))
  builders[estimator_ids]
}
