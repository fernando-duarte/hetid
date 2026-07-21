# Persist set-bootstrap diagnostics and exact draw objects.

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
  spec,
  collected,
  prim_cells,
  sens_cells,
  boot_reps,
  block,
  boot_seed,
  sens_block,
  sens_reps,
  provenance
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
  paper_write_exact_rds(
    list(
      b_reps = boot_reps,
      block = block,
      seed = boot_seed,
      sens_block = sens_block,
      sens_reps = sens_reps,
      inference_contract = PAPER_ANALYSIS_CONTRACT$inference,
      taus = spec$taus,
      coefs = spec$coefs,
      collected = collected,
      full = full,
      provenance = provenance
    ),
    artifact_path("log_variance_bootstrap_draws"),
    "log_variance_bootstrap_draws"
  )
}
