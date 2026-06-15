# Optimize Gamma to Minimize Total Identified-Set Width
# Holds tau fixed at BASELINE_TAU, optimizes PC loadings (gamma)

source(here::here("scripts/utils/common_settings.R"))

cli_h1("Optimizing Gamma for Minimum Set Width")

# Load baseline results from stage 04
baseline <- readRDS(file.path(
  OUTPUT_TEMP_DIR,
  "identification_baseline",
  "baseline_identification_results.rds"
))

# Fail closed if the loaded baseline was produced under different mode settings
# (Y1 lags, news-projection mode, Z source) than the current run.
assert_baseline_spec_current(baseline$spec)

moments <- baseline$moments
gamma_baseline <- baseline$gamma_baseline
lookup <- baseline$lookup

cli_alert_info(
  "Loaded baseline: {.val {ncol(gamma_baseline)}} components"
)

# The aligned instrument matrix the stage-04 moments were computed from. Var(Z)
# is not recoverable from the moments container, so the variance normalization
# needs Z itself. Stage 04 now persists the exact (Y1-lag-trimmed) Z, so reuse
# it; fall back to rebuilding from the front for older lag-free baseline RDS
# (only correct when no Y1 lags were applied).
z_aligned <- baseline$residuals$pcs_aligned
if (is.null(z_aligned)) {
  inputs <- load_identification_inputs(n_pcs = baseline$spec$n_pcs)
  pcs_mat <- as.matrix(inputs$data[, inputs$pc_vars])
  z_aligned <- get_identification_z(inputs$data, pcs_mat)[
    seq_len(baseline$residuals$n_obs), ,
    drop = FALSE
  ]
}
# Fail closed BEFORE any optimization: the rebuilt Z must reproduce
# the stage-04 moments exactly (tolerance 0), else the whitening
# covariance would not be the Var(Z) the constraints were built from.
assert_z_matches_moments(
  z_aligned, baseline$residuals$w1, baseline$residuals$w2, moments
)

# Fixed tau for set identification
n_comp <- ncol(gamma_baseline)
tau <- rep(BASELINE_TAU, n_comp)
cli_alert_info("Fixed tau = {.val {tau[1]}} for all components")

# Multistart count, shared by the optimizer call and its diagnostics record
n_starts_gamma <- 30L

# Run multi-start gamma optimization
cli_h2("Running Multi-Start Optimization")
cli_alert(
  "Primary start: baseline gamma (VFCI loadings)"
)
cli_alert("Additional starts: random perturbations")

opt_result <- run_lambda_optimization(
  lambda_start = gamma_baseline,
  moments = moments,
  tau = tau,
  whiten = list(z = z_aligned),
  n_starts = n_starts_gamma,
  seed = SEED
)
# Reported columns satisfy the repo default lambda' Vhat lambda = 1.
gamma_optimized <- do.call(cbind, opt_result$lambda_optimized)

obj_start <- round(opt_result$objective_start, 4)
obj_final <- round(opt_result$objective_final, 4)
best_idx <- opt_result$best_index

cli_alert_success(
  "Best from start index {.val {best_idx}}"
)
cli_alert_info(
  "Objective: {.val {obj_start}} -> {.val {obj_final}}"
)
lv <- opt_result$lambda_variance
cli_alert_info(paste0(
  "Unit-direction instrument variance (baseline -> optimized): ",
  paste(sprintf(
    "%s: %.4g -> %.4g", lookup$component_label,
    vapply(lv$start, `[[`, numeric(1), 1L),
    vapply(lv$optimized, `[[`, numeric(1), 1L)
  ), collapse = "; ")
))

# Rebuild bounds with optimized gamma
cli_h2("Rebuilding Bounds with Optimized Gamma")

quad_sys_opt <- build_pipeline_quadratic_system(
  gamma_optimized, tau, moments
)
optimized_bounds <- solve_all_profile_bounds(
  quad_sys_opt$quadratic
)

# Recompute baseline bounds for comparison
baseline_bounds <- baseline$bounds_tau_set

# Add maturity labels
optimized_bounds$component_label <- lookup$component_label
baseline_bounds$component_label <- lookup$component_label

# Constraint-checker closure membership probe (additive diagnostic) on the
# optimized-gamma system, over its profile-bound box.
optimized_membership <- probe_set_membership(
  quad_sys_opt$quadratic, optimized_bounds
)
cli_h2("Optimized-gamma membership probe (closure)")
print(optimized_membership$summary)

# Display comparison
cli_h2("Baseline Bounds (tau = {BASELINE_TAU}, VFCI gamma)")
print(
  baseline_bounds[, c(
    "component_label", "lower", "upper", "width"
  )]
)

cli_h2("Optimized Bounds (tau = {BASELINE_TAU}, optimized gamma)")
print(
  optimized_bounds[, c(
    "component_label", "lower", "upper", "width"
  )]
)

base_total <- sum(baseline_bounds$width)
opt_total <- sum(optimized_bounds$width)
if (!is.finite(base_total)) {
  cli_alert_success(
    "Baseline set unbounded; optimized total width = {.val {round(opt_total, 4)}}"
  )
} else {
  cli_alert_success(
    "Total width reduction: {.val {round(base_total - opt_total, 4)}}"
  )
}

# Build optimization trace from all starts
trace <- data.frame(
  start = seq_along(opt_result$all_results),
  objective = vapply(
    opt_result$all_results, function(r) r$value, numeric(1)
  ),
  convergence = vapply(
    opt_result$all_results, function(r) r$convergence, numeric(1)
  )
)

# Assemble results
results <- list(
  spec = baseline$spec,
  lookup = baseline$lookup,
  tau_fixed = tau,
  gamma_start = gamma_baseline,
  gamma_optimized = gamma_optimized,
  baseline_bounds = baseline_bounds,
  optimized_bounds = optimized_bounds,
  optimized_membership = optimized_membership,
  objective_start = opt_result$objective_start,
  objective_final = opt_result$objective_final,
  optimization_trace = trace,
  solver_diagnostics = list(
    n_starts = n_starts_gamma,
    best_index = opt_result$best_index,
    converged = opt_result$all_results[[
      opt_result$best_index
    ]]$convergence >= 0,
    whitening_source = opt_result$whitening$source,
    lambda_variance = opt_result$lambda_variance
  )
)

# Save outputs
output_dir <- file.path(
  OUTPUT_TEMP_DIR, "identification_optimized"
)
dir.create(
  output_dir,
  recursive = TRUE, showWarnings = FALSE
)

saveRDS(
  results,
  file.path(
    output_dir,
    "optimized_identification_results.rds"
  )
)

write.csv(
  trace,
  file.path(output_dir, "optimization_trace.csv"),
  row.names = FALSE
)

write.csv(
  optimized_membership$summary,
  file.path(output_dir, "optimized_membership.csv"),
  row.names = FALSE
)

cli_alert_success(
  "Results saved to: {.path {output_dir}}"
)
cli_alert_success(
  "Gamma optimization completed!"
)
