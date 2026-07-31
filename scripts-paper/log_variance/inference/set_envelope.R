# The one simultaneous (all-coefficient) containment critical value for the
# volatility panel, kept as a diagnostic so the report can state coefficientwise
# against simultaneous coverage. It is written to
# log_var_eq_set_inference_diagnostics.csv as c_sim and reaches no table cell.
#
# The per-cell machinery that used to live here is gone. Both panels' published
# cells now come from the shared builder in
# support/inference_post/endpoint_target_cells.R, which computes containment and
# pointwise coverage from one bootstrap reference distribution; keeping a second
# per-cell path here would have been a second live route into a published number.
# This function reuses that module's primitives rather than its own copies.

paper_source_once(paper_path(
  "support", "inference_post", "endpoint_targets.R"
))

logvar_simultaneous_critical <- function(draws, full,
                                         alpha =
                                           PAPER_ANALYSIS_CONTRACT$inference$nominal_alpha,
                                         min_reps = boot_min_reps(nrow(draws$lower)),
                                         stability =
                                           PAPER_ANALYSIS_CONTRACT$inference$stability_share) {
  b <- nrow(draws$lower)
  root <- rep(0, b)
  # the all-bounded pool: a simultaneous root needs every contributing side
  # defined on a draw, so the pool narrows with each live side it takes in
  common <- rep(TRUE, b)
  any_live <- FALSE
  for (k in seq_len(nrow(full))) {
    f <- full[k, ]
    lc <- endpoint_side_stat(
      draws$lower[, k], draws$lower_status[, k],
      f$set_lower, 1, min_reps, stability
    )
    uc <- endpoint_side_stat(
      draws$upper[, k], draws$upper_status[, k],
      f$set_upper, -1, min_reps, stability
    )
    if (identical(f$lower_status, PAPER_ENDPOINT_STATUS[["bounded"]]) && lc$gate) {
      root <- pmax(root, lc$z)
      common <- common & lc$ok
      any_live <- TRUE
    }
    if (identical(f$upper_status, PAPER_ENDPOINT_STATUS[["bounded"]]) && uc$gate) {
      root <- pmax(root, uc$z)
      common <- common & uc$ok
      any_live <- TRUE
    }
  }
  if (!any_live) {
    return(NA_real_)
  }
  root_critical(pmax(0, root)[common], alpha)
}
