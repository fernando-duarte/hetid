# Compute the I x J (separate-instrument) identified set.
#
# Each principal component PC_j is its own instrument for each component i, giving
# one quadratic constraint per (i, j) pair; the identified set is their
# intersection. theta stays in R^I and the deliverable is I profile intervals.
# Covariance / cross-moment form (see scripts/utils/ixj_identification.R). Tau is
# held fixed on a grid (no tau optimisation -- minimising width over tau >= 0 is
# degenerate). Boundedness is reported honestly (bounded-valid / unbounded /
# no-certified-bound); an empty intersection surfaces as no-certified-bound, NOT
# claimed as "empty" without a separate feasibility certificate.

source(here::here("scripts/utils/common_settings.R"))
source(here::here("scripts/utils/ixj_identification.R"))

# tau must lie in [0, 1): the package validator rejects 1.0 outright, so the
# near-uninformative probe uses 0.99, mirroring OPT_TAU_CAP in
# tau_star_comparison.R
TAU_GRID <- c(0.05, 0.1, 0.5, 0.99)

cli_h1("Computing I x J Identified Set (separate instruments)")

# Load data and reduced-form residuals, then the heteroskedasticity moments.
inputs <- load_identification_inputs()
lookup <- inputs$lookup
resid <- compute_identification_residuals(inputs$data)
moments <- compute_identification_moments(
  resid$w1, resid$w2, resid$pcs_aligned
)

n_pcs <- nrow(moments$r_i_0)
n_comp <- ncol(moments$r_i_0)
cli_ul(c(
  paste("Components (I):", n_comp),
  paste("Instruments (J):", n_pcs),
  paste("Constraints (I x J):", n_comp * n_pcs),
  paste("Tau grid:", paste(TAU_GRID, collapse = ", "))
))

# Honest per-side state from the solver's (bounded, valid) flags.
classify_side <- function(bounded, valid) {
  if (isTRUE(bounded) && isTRUE(valid)) {
    "bounded-valid"
  } else if (!isTRUE(bounded) && isTRUE(valid)) {
    "unbounded"
  } else {
    "no-certified-bound"
  }
}

# Solve the I x J set across the tau grid.
bounds_by_tau <- list()
for (tau in TAU_GRID) {
  qs <- build_ixj_quadratic_system(
    moments, matrix(tau, nrow = n_pcs, ncol = n_comp)
  )
  bnds <- solve_all_profile_bounds(qs$quadratic)
  bnds$component_label <- lookup$component_label
  bnds$tau <- tau
  bnds$state_lower <- mapply(classify_side, bnds$bounded_lower, bnds$valid_lower)
  bnds$state_upper <- mapply(classify_side, bnds$bounded_upper, bnds$valid_upper)
  bounds_by_tau[[as.character(tau)]] <- bnds

  cli_h2(sprintf("I x J bounds (tau = %.2f)", tau))
  print(data.frame(
    component = bnds$component_label,
    lower = format_bound(bnds$lower, bnds$valid_lower),
    upper = format_bound(bnds$upper, bnds$valid_upper),
    state_lower = bnds$state_lower,
    state_upper = bnds$state_upper,
    stringsAsFactors = FALSE
  ))
}

grid <- do.call(rbind, bounds_by_tau)
rownames(grid) <- NULL

# Side-by-side with the gamma-aggregated baseline (tau = 0.05 set), if available.
baseline_path <- file.path(
  OUTPUT_TEMP_DIR, "identification_baseline",
  "baseline_identification_results.rds"
)
if (file.exists(baseline_path)) {
  base_set <- readRDS(baseline_path)$bounds_tau_set
  ixj_02 <- bounds_by_tau[["0.05"]]
  cli_h2("Baseline (gamma-aggregated) vs I x J  --  tau = 0.05")
  print(data.frame(
    component = lookup$component_label,
    baseline_width = format_bound(
      base_set$width, base_set$valid_lower & base_set$valid_upper
    ),
    ixj_width = format_bound(
      ixj_02$width, ixj_02$valid_lower & ixj_02$valid_upper
    ),
    stringsAsFactors = FALSE
  ))
  cli_alert_info(
    "Different instrument constructions: a width nesting relation is NOT implied."
  )
} else {
  cli_alert_warning(
    "Baseline results not found; run compute_identification.R first for comparison."
  )
}

# Constraint-checker closure membership probe (additive diagnostic) on the
# tau = 0.05 I x J system, over its profile-bound box.
qs_02 <- build_ixj_quadratic_system(
  moments, matrix(0.05, nrow = n_pcs, ncol = n_comp)
)
ixj_membership <- probe_set_membership(
  qs_02$quadratic, bounds_by_tau[["0.05"]]
)
cli_h2("I x J membership probe (tau = 0.05, closure)")
print(ixj_membership$summary)

# Persist results.
output_dir <- file.path(OUTPUT_TEMP_DIR, "identification_ixj")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
paper_dir <- file.path(OUTPUT_PAPER_DIR, "identification")
dir.create(paper_dir, recursive = TRUE, showWarnings = FALSE)

results <- list(
  spec = list(
    mode = "maturities", n_pcs = n_pcs, n_components = n_comp,
    tau_grid = TAU_GRID, form = "covariance", constraints = n_comp * n_pcs
  ),
  lookup = lookup,
  bounds_by_tau = bounds_by_tau,
  grid = grid,
  ixj_membership = ixj_membership
)
saveRDS(results, file.path(output_dir, "ixj_identification_results.rds"))

# Human-readable export: keep numeric values only where the side is certified
# (bounded-valid keeps its value; unbounded keeps +/-Inf; no-certified-bound -> NA
# so a stray solver point is never mistaken for a real bound). The RDS layer
# retains the raw numerics + flags.
grid$lower <- ifelse(grid$valid_lower, grid$lower, NA_real_)
grid$upper <- ifelse(grid$valid_upper, grid$upper, NA_real_)
grid$width <- grid$upper - grid$lower
grid_csv <- grid[, c(
  "tau", "component_label", "lower", "upper", "width",
  "state_lower", "state_upper"
)]
write.csv(grid_csv, file.path(output_dir, "ixj_bounds_grid.csv"), row.names = FALSE)
write.csv(
  ixj_membership$summary,
  file.path(output_dir, "ixj_membership.csv"),
  row.names = FALSE
)
write.csv(grid_csv, file.path(paper_dir, "ixj_bounds_grid.csv"), row.names = FALSE)

cli_alert_success("I x J identified set computed across {length(TAU_GRID)} tau values")
cli_alert_success("Results saved to: {.path {output_dir}}")
