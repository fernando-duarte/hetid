# Optimize Gamma to Minimize Total Identified-Set Width
# Holds tau fixed at 0.2, optimizes PC loadings (gamma)

source(here::here("scripts/utils/common_settings.R"))

cli_h1("Optimizing Gamma for Minimum Set Width")

# Load baseline results from stage 04
baseline <- readRDS(file.path(
  OUTPUT_TEMP_DIR,
  "identification_baseline",
  "baseline_identification_results.rds"
))

moments <- baseline$moments
gamma_baseline <- baseline$gamma_baseline
lookup <- baseline$lookup

cli_alert_info(
  "Loaded baseline: {.val {ncol(gamma_baseline)}} components"
)

# Fixed tau for set identification
tau <- rep(0.2, 8)
cli_alert_info("Fixed tau = {.val {tau[1]}} for all components")

# Run multi-start gamma optimization
cli_h2("Running Multi-Start Optimization")
cli_alert(
  "Primary start: baseline gamma (VFCI loadings)"
)
cli_alert("Additional starts: random perturbations")

opt_result <- run_gamma_optimization(
  gamma_start = gamma_baseline,
  moments = moments,
  tau = tau,
  n_starts = 10,
  seed = SEED
)

obj_start <- round(opt_result$objective_start, 4)
obj_final <- round(opt_result$objective_final, 4)
best_idx <- opt_result$best_index

cli_alert_success(
  "Best from start index {.val {best_idx}}"
)
cli_alert_info(
  "Objective: {.val {obj_start}} -> {.val {obj_final}}"
)

# Rebuild bounds with optimized gamma
cli_h2("Rebuilding Bounds with Optimized Gamma")

quad_sys_opt <- build_quadratic_system(
  opt_result$gamma_optimized, tau, moments
)
quad_sys_opt <- symmetrize_quadratic_system(quad_sys_opt)
optimized_bounds <- solve_all_profile_bounds(
  quad_sys_opt$quadratic
)

# Recompute baseline bounds for comparison
baseline_bounds <- baseline$bounds_tau_set

# Add maturity labels
optimized_bounds$bond_maturity <- lookup$bond_maturity
baseline_bounds$bond_maturity <- lookup$bond_maturity

# Display comparison
cli_h2("Baseline Bounds (tau = 0.2, VFCI gamma)")
print(
  baseline_bounds[, c(
    "bond_maturity", "lower", "upper", "width"
  )]
)

cli_h2("Optimized Bounds (tau = 0.2, optimized gamma)")
print(
  optimized_bounds[, c(
    "bond_maturity", "lower", "upper", "width"
  )]
)

width_reduction <- sum(baseline_bounds$width) -
  sum(optimized_bounds$width)
cli_alert_success(
  "Total width reduction: {.val {round(width_reduction, 4)}}"
)

# Build optimization trace from all starts
trace <- data.frame(
  start = seq_along(opt_result$all_results),
  objective = sapply(
    opt_result$all_results, function(r) r$value
  ),
  convergence = sapply(
    opt_result$all_results, function(r) r$convergence
  )
)

# Assemble results
results <- list(
  spec = baseline$spec,
  lookup = baseline$lookup,
  tau_fixed = tau,
  gamma_start = gamma_baseline,
  gamma_optimized = opt_result$gamma_optimized,
  baseline_bounds = baseline_bounds,
  optimized_bounds = optimized_bounds,
  objective_start = opt_result$objective_start,
  objective_final = opt_result$objective_final,
  optimization_trace = trace,
  solver_diagnostics = list(
    n_starts = 10,
    best_index = opt_result$best_index,
    converged = opt_result$all_results[[
      opt_result$best_index
    ]]$convergence >= 0
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

cli_alert_success(
  "Results saved to: {.path {output_dir}}"
)
cli_alert_success(
  "Gamma optimization completed!"
)
