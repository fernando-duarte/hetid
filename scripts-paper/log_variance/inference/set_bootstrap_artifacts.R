# Persist set-bootstrap diagnostics. The exact draw objects are cached by the
# unified-stage cache dispatcher, so
# this writer produces only the per-cell diagnostics table.

write_logvar_set_boot_artifacts <- function(
  ests,
  display_taus,
  disp_key,
  disp_idx,
  prim_env,
  c_sim,
  full,
  se_obj,
  sens_env,
  tau0,
  point_t,
  spec,
  prim_cells,
  sens_cells
) {
  diag_rows <- list()
  for (est in ests) {
    for (d in seq_along(display_taus)) {
      key <- disp_key[d]
      j <- disp_idx[d]
      pub <- se_obj[[est]]$sets[[key]]
      stopifnot(identical(pub$coef, spec$coefs))
      sci <- if (is.null(sens_env)) {
        data.frame(ci_lower = NA_real_, ci_upper = NA_real_)
      } else {
        sens_env[[est]][[key]][c("ci_lower", "ci_upper")]
      }
      diag_rows[[length(diag_rows) + 1L]] <- data.frame(
        estimator = est,
        tau = display_taus[d],
        prim_env[[est]][[key]],
        c_sim = c_sim[[est]][[key]],
        anchor_lower = full[[est]][[disp_idx[d]]]$set_lower,
        anchor_upper = full[[est]][[disp_idx[d]]]$set_upper,
        published_lower = pub$set_lower,
        published_upper = pub$set_upper,
        published_status = pub$status,
        sens_ci_lower = sci$ci_lower,
        sens_ci_upper = sci$ci_upper,
        # the tau = 0 cell, recycled across the display rows as the tau0 ratio
        # columns already are. All four status counts and both gate denominators
        # are recorded so the point cell's arithmetic reconciles from the artifact
        # alone; point_n_unbounded must always be zero, since a point evaluation
        # cannot diverge, and is written rather than assumed so that a nonzero
        # value is visible instead of silent.
        point_estimate = point_t[[est]]$point,
        point_se = point_t[[est]]$se,
        point_statistic = point_t[[est]]$statistic,
        point_p_value = point_t[[est]]$p_value,
        point_n_bounded = point_t[[est]]$n_bounded,
        point_n_unbounded = point_t[[est]]$n_unbounded,
        point_n_unreliable = point_t[[est]]$n_unreliable,
        point_n_failed = point_t[[est]]$n_failed,
        point_n_valid = point_t[[est]]$n_valid_point,
        point_n_non_failed = point_t[[est]]$n_non_failed,
        point_reason = point_t[[est]]$reason,
        tau0_sd_boot = tau0[[est]]$sd_boot,
        tau0_se_analytic = tau0[[est]]$se_analytic,
        tau0_ratio = tau0[[est]]$ratio,
        failed_count = prim_cells[[est]][j, "count"],
        sens_failed_count = sens_cells[[est]][j, "count"],
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    }
  }
  paper_write_typed_csv(
    cbind(
      paper_inference_metadata_frame(length(diag_rows)),
      do.call(rbind, diag_rows)
    ),
    artifact_path("log_variance_inference_diagnostics"),
    "log_variance_inference_diagnostics"
  )
}
