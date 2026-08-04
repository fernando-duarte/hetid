# Driver for the two residual-diagnostic figures. Assembles the standardized log
# residual at the Lewbel tau = 0 point for PPML, Harvey and log-OLS, renders the
# quantile-quantile and density panels, and prints the moments each figure is
# read against. Wired into run_pipeline.R after the PPML and Harvey set drivers,
# so both point fits are already in logvar_bounds_tau_registry and nothing here
# refits. No decision-file reference and no approval gate: the figures ship with
# every run.

paper_source_once(paper_path(
  "log_variance", "figures", "residual_diagnostics", "data.R"
))
paper_source_once(paper_path(
  "log_variance", "figures", "residual_diagnostics", "plot.R"
))

resid_diag_b_point <- set_id_mean_eq$theta_table$point
stopifnot(!anyNA(resid_diag_b_point))
resid_diag_thetas <- logvar_resid_diag_thetas(
  logvar_bounds_tau_registry, log_var_eq$inputs, resid_diag_b_point
)
log_var_eq_residual_diagnostics <- logvar_resid_diag_series(
  log_var_eq$inputs, resid_diag_b_point, resid_diag_thetas
)
stopifnot(identical(
  nrow(log_var_eq_residual_diagnostics),
  length(log_var_eq$inputs$qtr) * length(LOGVAR_RESID_DIAG_ESTIMATORS)
))

resid_diag_qq_path <- artifact_path("residual_qq_figure")
resid_diag_density_path <- artifact_path("residual_density_figure")
logvar_resid_diag_qq_render(log_var_eq_residual_diagnostics, resid_diag_qq_path)
logvar_resid_diag_density_render(
  log_var_eq_residual_diagnostics, resid_diag_density_path
)

cat("[BEGIN LOGVAR RESIDUAL DIAGNOSTICS]\n")
cat(sprintf(
  "  standardized log residual at the tau = 0 point, %d quarters\n",
  length(log_var_eq$inputs$qtr)
))
resid_diag_moments <- logvar_resid_diag_moments(log_var_eq_residual_diagnostics)
cat(sprintf(
  "  %-8s mean %7.3f  var %6.3f  skew %7.3f  excess kurtosis %7.3f\n",
  resid_diag_moments$estimator, resid_diag_moments$mean,
  resid_diag_moments$variance, resid_diag_moments$skewness,
  resid_diag_moments$excess_kurtosis
))
cat(sprintf(
  "  %-8s mean %7.3f  var %6.3f  skew %7.3f  excess kurtosis %7.3f\n",
  "log chi2", attr(resid_diag_moments, "reference_mean"), pi^2 / 2,
  -16.8288 / (pi^2 / 2)^1.5, 4
))
cat(sprintf("  wrote %s\n", resid_diag_qq_path))
cat(sprintf("  wrote %s\n", resid_diag_density_path))
cat("[END LOGVAR RESIDUAL DIAGNOSTICS]\n")

rm(
  resid_diag_b_point, resid_diag_thetas, resid_diag_qq_path,
  resid_diag_density_path, resid_diag_moments
)
