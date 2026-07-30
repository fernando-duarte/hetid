# Estimator builders used by each moving-block bootstrap draw, plus the two
# tau = 0 summaries run once per estimator: the published t statistic and the
# bootstrap-versus-analytic-SE diagnostic ratio.

paper_source_once(paper_path(
  "support", "identification", "endpoint_point_statistic.R"
))

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

# The published tau = 0 cell for each estimator, through the same builder the mean
# panel uses. The numerator is the ANCHOR's point, not a projection of the
# estimator result: the anchor is the full-sample run of the very draw function
# that produced the denominator's draws, so both sides of the ratio come from one
# code path by construction. That holds only because the point record reads each
# estimator's own published recipe (set_bootstrap_core.R), which is what makes the
# anchor equal the estimator's published point column. The renderer re-checks that
# equality against the estimate it prints, since a divergence there would put a
# ratio under a number it does not belong to.
logvar_boot_point_t <- function(ests, collected, anchor, spec, tau0_slot = 1L) {
  out <- lapply(ests, function(est) {
    cell <- collected[[est]][[tau0_slot]]
    full <- anchor[[est]][[tau0_slot]]
    stopifnot(
      identical(colnames(cell$point), spec$coefs),
      length(full$point) == length(spec$coefs)
    )
    point_t_statistic(full$point, cell$point, cell$point_status)
  })
  names(out) <- ests
  out
}

# tau = 0 point diagnostic: bootstrap SD of the point draws against each
# estimator's analytic SE, printed as a sanity ratio and returned for the
# diagnostics CSV. The scale is taken from the authoritative point field over
# the draws its own status calls bounded, never from a compatibility mirror.
logvar_boot_tau0_diagnostics <- function(
  ests, collected, se_obj, se_type, spec,
  digits = PAPER_REPORTING_CONTROL$precision$console_significant,
  tau0_slot = 1L
) {
  tau0 <- lapply(ests, function(est) {
    cell <- collected[[est]][[tau0_slot]]
    bounded <- cell$point_status == PAPER_ENDPOINT_STATUS[["bounded"]]
    sd_boot <- stats::setNames(vapply(seq_along(spec$coefs), function(j) {
      values <- cell$point[bounded[, j], j]
      if (length(values) >= 2L) robust_scale(values) else NA_real_
    }, numeric(1)), spec$coefs)
    se_df <- se_obj[[est]]$se$point
    se_an <- stats::setNames(se_df[[se_type[[est]]]], se_df$coef)[spec$coefs]
    message(sprintf(
      "  %s tau=0 bootstrap SD / analytic %s SE: %s", est, se_type[[est]],
      paste(
        paper_format_general(
          sd_boot / se_an,
          digits
        ),
        collapse = " "
      )
    ))
    data.frame(coef = spec$coefs, sd_boot = sd_boot, se_analytic = se_an, ratio = sd_boot / se_an)
  })
  names(tau0) <- ests
  tau0
}
