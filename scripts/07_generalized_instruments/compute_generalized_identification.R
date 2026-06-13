# Generalized-instrument identified set on squared principal components (Z = PC^2).
# Exercises the exported generalized-instrument front-end (build_instrument_matrix
# -> build_general_quadratic_system) and the constraint-checker closure
# (make_system_checker / make_constraint_checker, via probe_set_membership).
# Only the INSTRUMENT role is swapped to PC^2; the structural first stage is
# unchanged. Not a paper deliverable -- a generalized-API demonstration pass.

source(here::here("scripts/utils/common_settings.R"))

ID_MODE <- "maturities" # match the baseline stage (compute_identification.R)

cli_h1("Generalized-Instrument Identified Set (Z = PC^2)")

# Reduced-form residuals + level PCs (HETID_Z_SOURCE unset -> default level-PC
# instruments, already aligned to w1/w2 by compute_identification_residuals).
inputs <- load_identification_inputs(mode = ID_MODE)
lookup <- inputs$lookup
resid <- compute_identification_residuals(inputs$data, mode = ID_MODE)

# Z = squared PCs, built through the exported generalized front-end.
z_sq <- build_instrument_matrix(
  resid$pcs_aligned,
  transforms = list(sq = function(z) z^2),
  include_original = FALSE
)
cli_alert_info("Instruments: {.val {paste(colnames(z_sq), collapse = ', ')}}")

moments <- compute_identification_moments(resid$w1, resid$w2, z_sq)
n_comp <- attr(moments, "n_components")

# Package-default identity weights: one constraint per (component, instrument).
# PC^2 has no canonical loading, so identity weights are the natural default.
lambda <- separate_instruments_lambda(moments)

cli_h2("Building Generalized Quadratic Systems")
qs_set <- build_general_quadratic_system(
  lambda, rep(BASELINE_TAU, n_comp), moments
)
qs0 <- build_general_quadratic_system(
  lambda, rep(0, n_comp), moments
)
cli_alert_success("Constraints: {.val {nrow(qs_set$labels)}}")

cli_h2("Solving Profile Bounds")
bounds_set <- solve_all_profile_bounds(qs_set$quadratic)
bounds_set$component_label <- lookup$component_label
bounds0 <- solve_all_profile_bounds(qs0$quadratic)
bounds0$component_label <- lookup$component_label

cli_h2(sprintf("Generalized-Z bounds (tau = %.2f)", BASELINE_TAU))
print(bounds_set[, c("component_label", "lower", "upper", "width")])

# Constraint-checker closure membership probe over the profile-bound box.
cli_h2("Identified-Set Membership Probe (closure)")
membership <- probe_set_membership(qs_set$quadratic, bounds_set)
print(membership$summary)

results <- list(
  spec = list(
    mode = ID_MODE,
    n_pcs = ncol(resid$pcs_aligned),
    n_components = n_comp,
    instruments = colnames(z_sq),
    tau_set = BASELINE_TAU,
    constraints = nrow(qs_set$labels)
  ),
  lookup = lookup,
  moments = moments,
  lambda = lambda,
  quadratic_tau0 = qs0,
  quadratic_tau_set = qs_set,
  bounds_tau0 = bounds0,
  bounds_tau_set = bounds_set,
  membership = membership
)

output_dir <- file.path(OUTPUT_TEMP_DIR, "identification_generalized")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(
  results,
  file.path(output_dir, "generalized_identification_results.rds")
)
write.csv(
  bounds_set[, c(
    "component_label", "lower", "upper", "width",
    "bounded_lower", "bounded_upper", "valid_lower", "valid_upper"
  )],
  file.path(output_dir, "generalized_bounds.csv"),
  row.names = FALSE
)
write.csv(
  membership$summary,
  file.path(output_dir, "generalized_membership.csv"),
  row.names = FALSE
)

cli_alert_success("Generalized-instrument identification computed")
cli_alert_success("Results saved to: {.path {output_dir}}")
