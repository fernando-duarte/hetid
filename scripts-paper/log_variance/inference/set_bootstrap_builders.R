# Estimator builders used by each moving-block bootstrap draw.

logvar_set_boot_builders <- function(scale_value, logols_coef) {
  build_ppml <- function(w1, w2, pcr, qtr, b_point) {
    anchor <- if (!is.null(b_point)) b_point else rep(0, ncol(w2))
    logvar_ppml_estimator(
      w1, w2, pcr, qtr,
      b_point = b_point,
      scale_anchor_b = anchor,
      scale_anchor_source = "boot",
      response_scale = scale_value,
      control = LOGVAR_PPML_CONTROL
    )
  }
  build_harvey <- function(w1, w2, pcr, qtr, b_point, ppml_obj) {
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
      control = LOGVAR_HARVEY_CONTROL
    )
  }
  list(ppml = build_ppml, harvey = build_harvey)
}
