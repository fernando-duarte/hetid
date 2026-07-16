# Thin driver for the base-R log-variance dynamics gate (dynamics-gate protocol). It
# pulls the frozen benchmark objects, evaluates the predeclared lag-4 Ljung-Box
# screen on the tau = 0 residual xi_hat, prints the verdict with lags 1/4/8, the
# ACF through lag 8, the min |eps*| fragility, and the largest |xi_hat|, then
# writes the section-2.4 gate record and the always-present status manifest. The
# gate ships regardless of any approval and uses base R only. Sourcing this file
# offline (without log_var_eq / set_id_mean_eq) is a no-op: the exists() guard
# mirrors the guarded Harvey driver idiom, so offline sourcing defines no products.
#
# Wired into run_pipeline.R after set_id_mean_eq and log_var_eq are built (so the
# objects exist) and after diagnostics/joint_gmm/run.R. No decision-file reference
# here -- the router and approvals belong to the routing layer.

if (exists("log_var_eq") && exists("set_id_mean_eq")) {
  dyn_inputs <- log_var_eq$inputs
  dyn_b_point <- set_id_mean_eq$theta_table$point
  # deterministically recomputed fixed projection (not a re-estimation)
  dyn_proj <- logvar_projection(dyn_inputs$pcr)
  # read-only provenance stamp, fully captured so it never hits the console
  # regression; NA when the git call is unavailable
  dyn_commit <- tryCatch(
    suppressWarnings(
      system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)
    ),
    error = function(e) NA_character_
  )
  if (length(dyn_commit) != 1L || !nzchar(dyn_commit)) dyn_commit <- NA_character_

  log_var_eq_dynamics_gate <- logvar_gate_evaluate(
    inputs = dyn_inputs, b_point = dyn_b_point, proj = dyn_proj,
    table_point = log_var_eq$table$point, schema = log_var_eq$schema,
    b_seed = log_var_eq$b_seed, sample_id = log_var_eq$sample_id,
    benchmark_commit = dyn_commit
  )
  log_var_eq_egarch_status <- logvar_gate_status_manifest(log_var_eq_dynamics_gate)

  dyn_g <- log_var_eq_dynamics_gate
  cat("[BEGIN LOGVAR DYNAMICS GATE]\n")
  cat(sprintf(
    "  verdict: %s (gate lag %d, alpha %.2f)\n",
    dyn_g$verdict, dyn_g$gate_lag, dyn_g$gate_alpha
  ))
  for (nm in names(dyn_g$q_stats)) {
    cat(sprintf(
      "    %-5s Q = %8.3f  p = %.4g%s\n", nm, dyn_g$q_stats[[nm]],
      dyn_g$p_values[[nm]], if (nm == sprintf("lag%d", dyn_g$gate_lag)) "  <- gate" else ""
    ))
  }
  cat(sprintf(
    "  residual ACF (lag 1..8): %s\n",
    paste(sprintf("%.3f", dyn_g$acf), collapse = " ")
  ))
  cat(sprintf(
    "  min |eps*| = %.4g (fragility beside the verdict); crossing status: %s\n",
    dyn_g$min_abs_eps, dyn_g$crossing_status
  ))
  cat(sprintf(
    "  largest |xi_hat| = %.4g at %s\n",
    dyn_g$max_abs_xi, format(dyn_g$max_abs_xi_qtr)
  ))
  if (!is.null(dyn_g$crossing_qtr)) {
    cat(sprintf(
      "  crossing quarters: %s\n", paste(format(dyn_g$crossing_qtr), collapse = ", ")
    ))
  }
  for (note in dyn_g$notes) cat(sprintf("  note: %s\n", note))
  cat("[END LOGVAR DYNAMICS GATE]\n")

  dyn_out <- out_dir
  dyn_gate_rds <- artifact_path("dynamics_gate")
  dyn_status_rds <- artifact_path("egarch_status")
  unlink(c(dyn_gate_rds, dyn_status_rds))
  saveRDS(log_var_eq_dynamics_gate, dyn_gate_rds, version = 3)
  saveRDS(log_var_eq_egarch_status, dyn_status_rds, version = 3)

  rm(
    dyn_inputs, dyn_b_point, dyn_proj, dyn_commit, dyn_g, dyn_out,
    dyn_gate_rds, dyn_status_rds, nm, note
  )
}
