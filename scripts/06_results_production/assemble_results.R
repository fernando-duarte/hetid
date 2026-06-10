# Assemble Final Identification Results
# Merge baseline, optimized, and supporting artifacts
# into one clean comparison object

source(here::here("scripts/utils/common_settings.R"))

cli_h1("Assembling Final Identification Results")

# Load baseline artifacts
cli_h2("Loading Baseline Results")
baseline_results <- readRDS(file.path(
  OUTPUT_TEMP_DIR,
  "identification_baseline",
  "baseline_identification_results.rds"
))
baseline_analysis <- readRDS(file.path(
  OUTPUT_TEMP_DIR,
  "identification_baseline",
  "baseline_identification_analysis.rds"
))
cli_alert_success("Baseline results loaded")

# Load optimized artifacts
cli_h2("Loading Optimized Results")
optimized_results <- readRDS(file.path(
  OUTPUT_TEMP_DIR,
  "identification_optimized",
  "optimized_identification_results.rds"
))
optimized_analysis <- readRDS(file.path(
  OUTPUT_TEMP_DIR,
  "identification_optimized",
  "optimized_identification_analysis.rds"
))
cli_alert_success("Optimized results loaded")

# Optionally load variance bounds
cli_h2("Loading Variance Bounds (if available)")
vb_path <- file.path(
  OUTPUT_TEMP_DIR,
  "variance_bounds",
  "variance_bounds_results.rds"
)
variance_bounds <- if (file.exists(vb_path)) {
  cli_alert_success("Variance bounds found")
  readRDS(vb_path)
} else {
  cli_alert_info("Variance bounds not found; skipping")
  NULL
}

# Build unified comparison table
cli_h2("Building Comparison Table")
lookup <- baseline_results$lookup
join_by <- c("component" = "component_id")

baseline_bounds <- baseline_results$bounds_tau_set |>
  select(-any_of(c("bond_maturity", "component_label"))) |>
  left_join(lookup, by = join_by)
optimized_bounds <- optimized_results$optimized_bounds |>
  select(-any_of(c("bond_maturity", "component_label"))) |>
  left_join(lookup, by = join_by)

comparison_table <- baseline_bounds |>
  select(
    component_label, component,
    baseline_lower = lower,
    baseline_upper = upper,
    baseline_valid_lower = valid_lower,
    baseline_valid_upper = valid_upper
  ) |>
  left_join(
    optimized_bounds |>
      select(
        component,
        optimized_lower = lower,
        optimized_upper = upper,
        optimized_valid_lower = valid_lower,
        optimized_valid_upper = valid_upper
      ),
    by = "component"
  ) |>
  mutate(
    baseline_width = baseline_upper - baseline_lower,
    optimized_width = optimized_upper - optimized_lower,
    baseline_unbounded = !is.finite(baseline_width),
    # reduction metrics are meaningless unless BOTH sides are bounded AND valid.
    # A genuinely unbounded optimized side is valid=TRUE (see solve_profile_bound),
    # so check optimized finiteness explicitly -- not just the validity flags.
    metric_invalid = baseline_unbounded |
      !is.finite(optimized_width) |
      !(baseline_valid_lower & baseline_valid_upper) |
      !(optimized_valid_lower & optimized_valid_upper),
    abs_width_reduction = ifelse(metric_invalid, NA_real_,
      baseline_width - optimized_width
    ),
    pct_width_reduction = ifelse(metric_invalid, NA_real_,
      abs_width_reduction / baseline_width * 100
    ),
    reduction_label = format_reduction(
      baseline_width, optimized_width,
      baseline_valid_lower & baseline_valid_upper,
      optimized_valid_lower & optimized_valid_upper
    )
  )

# Merge variance bounds if available
if (
  !is.null(variance_bounds) &&
    "bond_maturity" %in% names(lookup)
) {
  vb_df <- variance_bounds$variance_bounds_df |>
    rename(bond_maturity = Maturity)
  comparison_table <- comparison_table |>
    left_join(
      lookup |> select(component_id, bond_maturity),
      by = c("component" = "component_id")
    ) |>
    left_join(vb_df, by = "bond_maturity")
  cli_alert_success(
    "Variance bounds merged into table"
  )
}

# Display summary
cli_h2("Comparison Summary")
print(comparison_table |>
  select(
    component_label, baseline_width,
    optimized_width, pct_width_reduction
  ))

mean_reduction <- round(
  mean_pct_reduction(
    comparison_table$baseline_width,
    comparison_table$optimized_width,
    comparison_table$baseline_valid_lower &
      comparison_table$baseline_valid_upper,
    comparison_table$optimized_valid_lower &
      comparison_table$optimized_valid_upper
  ), 2
)
cli_alert_info(
  paste(
    "Mean width reduction (bounded components only):",
    "{.val {mean_reduction}}%"
  )
)

# Determine gamma method from baseline
gamma_method <- attr(
  baseline_results$gamma_baseline, "method"
)
if (is.null(gamma_method)) gamma_method <- "unknown"

# Assemble final results object
cli_h2("Assembling Final Object")
final_results <- list(
  comparison_table = comparison_table,
  baseline_results = baseline_results,
  optimized_results = optimized_results,
  variance_bounds = variance_bounds,
  metadata = list(
    created = Sys.time(),
    gamma_method = gamma_method,
    tau_baseline = BASELINE_TAU,
    n_components = nrow(comparison_table),
    mean_pct_reduction = mean_reduction
  )
)

# Save outputs
output_dir <- file.path(
  OUTPUT_TEMP_DIR, "identification_results"
)
dir.create(
  output_dir,
  recursive = TRUE, showWarnings = FALSE
)

saveRDS(
  final_results,
  file.path(
    output_dir,
    "final_identification_results.rds"
  )
)

write.csv(
  comparison_table,
  file.path(
    output_dir,
    "final_identification_comparison.csv"
  ),
  row.names = FALSE
)

cli_alert_success(
  "Results saved to: {.path {output_dir}}"
)
cli_alert_success(
  "Final identification assembly completed!"
)
