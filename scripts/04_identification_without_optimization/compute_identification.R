# Compute Baseline Identified Set
# Fixed PC weights (VFCI) and fixed tau values

# Load required packages and settings
source(here::here("scripts/utils/common_settings.R"))
# Core packages and utility functions loaded via common_settings.R

# Selectable without editing this file: HETID_BASELINE_GAMMA=reduced_form Rscript ...
BASELINE_GAMMA_METHOD <- Sys.getenv("HETID_BASELINE_GAMMA", "vfci")
ID_MODE <- "maturities" # "maturities" or "factors"

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

# Get baseline gamma and tau specifications. resolve_baseline_gamma honors
# HETID_BASELINE_GAMMA (vfci | reduced_form | path defining
# build_gamma(moments)) and sizes the gamma from the moments container, so
# a custom-width HETID_Z_SOURCE needs only a matching gamma hook.
n_comp <- nrow(lookup)
gamma <- resolve_baseline_gamma(BASELINE_GAMMA_METHOD, moments, resid$gamma_rf)
tau_specs <- get_tau_spec(
  tau_point = 0, tau_set = BASELINE_TAU, n_components = n_comp
)

cli_alert_info(
  "Gamma: {.val {BASELINE_GAMMA_METHOD}} (rank {.val {qr(gamma)$rank}} of {.val {ncol(gamma)}})"
)
cli_alert_info("Tau point: {.val {tau_specs$tau_point[1]}}")
cli_alert_info("Tau set: {.val {tau_specs$tau_set[1]}}")

# Build quadratic systems
cli_h2("Building Quadratic Systems")

cli_alert("Building system for tau = 0 (point ID)...")
quad_sys_tau0 <- build_quadratic_system(
  gamma, tau_specs$tau_point, moments
)

cli_alert("Building system for tau = {BASELINE_TAU} (set ID)...")
quad_sys_tau_set <- build_quadratic_system(
  gamma, tau_specs$tau_set, moments
)

cli_alert_success("Quadratic systems built")

# Solve profile bounds
cli_h2("Solving Profile Bounds")

# At tau = 0 each constraint is the perfect square (Q_i' theta - L_i)^2, so the
# set is the single point solve(Q, L). Use the closed-form point when consistent,
# else fall back to the inequality solver.
cli_alert("Solving bounds for tau = 0...")
pt_tau0 <- solve_point_identification(quad_sys_tau0$components)
if (!is.null(pt_tau0)) {
  bounds_tau0 <- data.frame(
    component = seq_along(pt_tau0$theta),
    lower = pt_tau0$theta, upper = pt_tau0$theta, width = 0,
    bounded_lower = TRUE, bounded_upper = TRUE,
    valid_lower = TRUE, valid_upper = TRUE
  )
  cli_alert_info(
    "tau = 0 point ID: cond(Q) = {.val {round(pt_tau0$cond, 1)}}"
  )
} else {
  cli_alert_warning("tau = 0 linear system inconsistent; using set solver")
  bounds_tau0 <- solve_all_profile_bounds(quad_sys_tau0$quadratic)
}

cli_alert("Solving bounds for tau = 0.2...")
bounds_tau_set <- solve_all_profile_bounds(
  quad_sys_tau_set$quadratic
)

# Check validity (tau = 0 point ID) and boundedness (tau = 0.2 set)
all_valid_tau0 <- all(bounds_tau0$valid_lower) &&
  all(bounds_tau0$valid_upper)
all_bounded_tau_set <- all(bounds_tau_set$bounded_lower) &&
  all(bounds_tau_set$bounded_upper)

if (all_valid_tau0) {
  cli_alert_success("tau = 0 point-ID bounds valid")
} else {
  cli_alert_warning("Some tau = 0 bounds failed the validity check")
}

if (all_bounded_tau_set) {
  cli_alert_success("tau = 0.2 set is bounded")
} else {
  cli_alert_warning(
    "tau = 0.2 set is UNBOUNDED (slack exceeds this gamma's tau*)"
  )
}

# Add maturity labels to bounds tables
bounds_tau0$component_label <- lookup$component_label
bounds_tau_set$component_label <- lookup$component_label

# Constraint-checker closure membership probe (additive diagnostic) over the
# tau = 0.2 set's profile-bound box; complements the profile-bound widths.
membership_tau_set <- probe_set_membership(
  quad_sys_tau_set$quadratic, bounds_tau_set
)
cli_h2("Membership Probe (tau = 0.2, closure)")
print(membership_tau_set$summary)

# Display results
display_cols <- c("component_label", "lower", "upper", "width")
cli_h2("Baseline Bounds (tau = 0)")
print(bounds_tau0[, display_cols])

cli_h2("Baseline Bounds (tau = 0.2)")
print(bounds_tau_set[, display_cols])

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
  membership_tau_set = membership_tau_set,
  solver_diagnostics = list(
    all_converged_tau0 = all_valid_tau0,
    all_converged_tau_set = all_bounded_tau_set
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

write.csv(
  membership_tau_set$summary,
  file.path(output_dir, "baseline_membership_tau_set.csv"),
  row.names = FALSE
)

cli_alert_success(
  "Results saved to: {.path {output_dir}}"
)
cli_alert_success(
  "Baseline identification computation completed!"
)
