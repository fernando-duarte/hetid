# Estimator builders used by each moving-block bootstrap draw. Draw-path code:
# every function here runs inside the resample loop, so this file stays inside
# the manifest that invalidates the draw cache. The two tau = 0 summaries that
# used to live here are pure functions of the collected draws and moved to
# support/inference_post/logvar_point_summaries.R.

logvar_set_boot_builders <- function(
  scale_value,
  logols_coef,
  estimator_ids = paper_logvar_estimator_ids(capability = "set_bootstrap"),
  ppml_control = LOGVAR_PPML_CONTROL,
  harvey_control = LOGVAR_HARVEY_CONTROL,
  normal_log_square_gap = LOGVAR_NORMAL_LOG_SQUARE_GAP,
  log_projection_control = LOGVAR_LOG_PROJECTION_CONTROL
) {
  force(scale_value)
  force(logols_coef)
  force(estimator_ids)
  force(ppml_control)
  force(harvey_control)
  force(normal_log_square_gap)
  force(log_projection_control)
  build_ppml <- function(w1, w2, pcr, qtr, b_point, built, mean_sample) {
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
  build_harvey <- function(w1, w2, pcr, qtr, b_point, built, mean_sample) {
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
  build_log_projection <- function(id) {
    force(id)
    function(w1, w2, pcr, qtr, b_point, built, mean_sample) {
      # raw PCs and positional identifiers: the package centers once, as for
      # the published preparation, and a resample repeats quarters
      prep <- hetid::prepare_log_projection(
        mean_sample$w1, mean_sample$w2, mean_sample$pc_raw,
        seq_along(mean_sample$w1), mean_sample$volatility_rows
      )
      stopifnot(identical(unname(prep$w1), unname(w1)))
      logvar_log_projection_estimator(
        prep, id, id, hetid::LOG_PROJECTION_CONTROL$MULTIPLIER,
        logvar_sample_id(qtr, w1, w2, pcr), log_projection_control
      )
    }
  }
  builders <- list(
    ppml = build_ppml, harvey = build_harvey,
    log_plus = build_log_projection("log_plus"),
    log_fuller = build_log_projection("log_fuller")
  )
  stopifnot(all(estimator_ids %in% names(builders)))
  builders[estimator_ids]
}
