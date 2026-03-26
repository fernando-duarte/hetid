# Compute Baseline Identified Set
# Fixed PC weights (VFCI) and fixed tau values

# Load required packages and settings
source(here::here("scripts/utils/common_settings.R"))
# Core packages and utility functions loaded via common_settings.R

BASELINE_GAMMA_METHOD <- "vfci"
ID_MODE <- "factors" # "maturities" or "factors"

cli_h1("Computing Baseline Identified Set")

# Load data and configuration
inputs <- load_identification_inputs(mode = ID_MODE)
lookup <- inputs$lookup

cli_ul(c(
  paste("Mode:", ID_MODE),
  paste("PCs:", paste(inputs$pc_vars, collapse = ", ")),
  paste(
    "Components:",
    paste(lookup$component_label, collapse = ", ")
  ),
  paste("Gamma method:", BASELINE_GAMMA_METHOD)
))

# Compute residuals
cli_h2("Computing Residuals")
resid <- compute_identification_residuals(
  inputs$data,
  mode = ID_MODE
)

cli_alert_success(
  "Residuals computed: {.val {resid$n_obs}} observations"
)

# Use lagged PCs aligned with residuals
pcs <- resid$pcs_aligned

# Compute moments
cli_h2("Computing Moments")
moments <- compute_identification_moments(
  resid$w1, resid$w2, pcs
)
cli_alert_success("Moments computed successfully")

# Get baseline gamma and tau specifications
n_comp <- nrow(lookup)
gamma <- get_baseline_gamma(
  method = BASELINE_GAMMA_METHOD, n_components = n_comp
)
tau_specs <- get_tau_spec(
  tau_point = 0, tau_set = 0.2, n_components = n_comp
)

cli_alert_info(
  "Gamma: {.val {BASELINE_GAMMA_METHOD}} unit-norm loadings"
)
cli_alert_info(
  "Tau point: {.val {tau_specs$tau_point[1]}}, "
)
cli_alert_info(
  "Tau set: {.val {tau_specs$tau_set[1]}}"
)

# Build quadratic systems
cli_h2("Building Quadratic Systems")

cli_alert("Building system for tau = 0 (point ID)...")
quad_sys_tau0 <- build_quadratic_system(
  gamma, tau_specs$tau_point, moments
)
quad_sys_tau0 <- symmetrize_quadratic_system(quad_sys_tau0)

cli_alert("Building system for tau = 0.2 (set ID)...")
quad_sys_tau_set <- build_quadratic_system(
  gamma, tau_specs$tau_set, moments
)
quad_sys_tau_set <- symmetrize_quadratic_system(
  quad_sys_tau_set
)

cli_alert_success("Quadratic systems built and symmetrized")

# Solve profile bounds
cli_h2("Solving Profile Bounds")

cli_alert("Solving bounds for tau = 0...")
bounds_tau0 <- solve_all_profile_bounds(
  quad_sys_tau0$quadratic
)

cli_alert("Solving bounds for tau = 0.2...")
bounds_tau_set <- solve_all_profile_bounds(
  quad_sys_tau_set$quadratic
)

# Check convergence
all_conv_tau0 <- all(bounds_tau0$converged_lower) &&
  all(bounds_tau0$converged_upper)
all_conv_tau_set <- all(bounds_tau_set$converged_lower) &&
  all(bounds_tau_set$converged_upper)

if (all_conv_tau0) {
  cli_alert_success("All bounds converged for tau = 0")
} else {
  cli_alert_warning("Some bounds did NOT converge for tau = 0")
}

if (all_conv_tau_set) {
  cli_alert_success("All bounds converged for tau = 0.2")
} else {
  cli_alert_warning(
    "Some bounds did NOT converge for tau = 0.2"
  )
}

# Add maturity labels to bounds tables
bounds_tau0$component_label <- lookup$component_label
bounds_tau_set$component_label <- lookup$component_label

# Display results
cli_h2("Baseline Bounds (tau = 0)")
print(
  bounds_tau0[, c(
    "component_label", "lower", "upper", "width"
  )]
)

cli_h2("Baseline Bounds (tau = 0.2)")
print(
  bounds_tau_set[, c(
    "component_label", "lower", "upper", "width"
  )]
)

# Assemble results
results <- list(
  spec = list(
    n_pcs = length(inputs$pc_vars),
    mode = ID_MODE,
    components = lookup$component_id
  ),
  lookup = lookup,
  residuals = list(
    w1 = resid$w1,
    w2 = resid$w2,
    n_obs = resid$n_obs
  ),
  moments = moments,
  gamma_baseline = gamma,
  tau_specs = tau_specs,
  quadratic_tau0 = quad_sys_tau0,
  quadratic_tau_set = quad_sys_tau_set,
  bounds_tau0 = bounds_tau0,
  bounds_tau_set = bounds_tau_set,
  solver_diagnostics = list(
    all_converged_tau0 = all_conv_tau0,
    all_converged_tau_set = all_conv_tau_set
  )
)

# Create output directory and save results
output_dir <- file.path(
  OUTPUT_TEMP_DIR, "identification_baseline"
)
dir.create(
  output_dir,
  recursive = TRUE, showWarnings = FALSE
)

saveRDS(
  results,
  file.path(output_dir, "baseline_identification_results.rds")
)

write.csv(
  bounds_tau0,
  file.path(output_dir, "baseline_bounds_tau0.csv"),
  row.names = FALSE
)

write.csv(
  bounds_tau_set,
  file.path(output_dir, "baseline_bounds_tau_set.csv"),
  row.names = FALSE
)

cli_alert_success(
  "Results saved to: {.path {output_dir}}"
)
cli_alert_success(
  "Baseline identification computation completed!"
)
