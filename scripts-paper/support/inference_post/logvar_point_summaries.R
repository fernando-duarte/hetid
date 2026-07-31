# The two tau = 0 summaries run once per estimator after every draw exists: the
# published t statistic and the bootstrap-versus-analytic-SE diagnostic ratio.
# Post-draw code -- both are deterministic functions of the collected draws and
# the full-sample anchor, so editing them must not invalidate the draw cache.
# Split out of log_variance/inference/set_bootstrap_builders.R, which keeps the
# draw-time builders. Consumed by inference/bootstrap_stage_results.R.

paper_source_once(paper_path(
  "support", "inference_post", "endpoint_point_statistic.R"
))

# The published tau = 0 cell for each estimator, through the same builder the mean
# panel uses. The numerator is the ANCHOR's point, not a projection of the
# estimator result: the anchor is the full-sample run of the very draw function
# that produced the denominator's draws, so both sides of the ratio come from one
# code path by construction. That holds only because the point record reads each
# estimator's own published recipe (set_bootstrap_core.R), which is what makes the
# anchor equal the estimator's published point column. The renderer re-checks that
# equality against the estimate it prints, since a divergence there would put a
# ratio under a number it does not belong to.
logvar_boot_point_t <- function(ests, collected, anchor, spec, tau0_slot) {
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
  ests, collected, se_obj, se_type, spec, point_t,
  digits = PAPER_REPORTING_CONTROL$precision$console_significant
) {
  tau0 <- lapply(ests, function(est) {
    # the published cell's own denominator, not a second derivation of it: an
    # independent recomputation here masked the bounded status differently and
    # could print a ratio whose numerator was not the scale the cell divides by
    sd_boot <- stats::setNames(point_t[[est]]$se, spec$coefs)
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
