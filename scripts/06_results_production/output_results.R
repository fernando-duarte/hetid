# Output Final Identification Results
# Write human-readable summaries and stable machine-readable
# exports for downstream use

source(here::here("scripts/utils/common_settings.R"))

cli_h1("Outputting Final Identification Results")

# Load assembled final results
final <- readRDS(file.path(
  OUTPUT_TEMP_DIR,
  "identification_results",
  "final_identification_results.rds"
))

# Create output directory for paper
paper_dir <- file.path(
  OUTPUT_PAPER_DIR, "identification"
)
dir.create(
  paper_dir,
  recursive = TRUE, showWarnings = FALSE
)

# Extract components for convenience
comp_table <- final$comparison_table
baseline <- final$baseline_results
optimized <- final$optimized_results
meta <- final$metadata

# Save machine-readable exports
cli_h2("Saving Machine-Readable Exports")

rds_path <- file.path(
  paper_dir, "final_identification_results.rds"
)
saveRDS(final, rds_path)
cli_alert_success("RDS saved: {.path {rds_path}}")

csv_path <- file.path(
  paper_dir, "final_identification_results.csv"
)
write.csv(comp_table, csv_path, row.names = FALSE)
cli_alert_success("CSV saved: {.path {csv_path}}")

# Build the plain text summary
cli_h2("Building Plain Text Summary")

gamma_method <- attr(
  baseline$gamma_baseline, "method"
)
gamma_str <- paste(
  sprintf("%.6f", as.vector(baseline$gamma_baseline)),
  collapse = ", "
)
tau_point <- baseline$tau_specs$tau_point[1]
tau_set <- baseline$tau_specs$tau_set[1]

obj_start <- optimized$objective_start
obj_final <- optimized$objective_final
obj_improvement <- obj_start - obj_final
obj_pct <- obj_improvement / obj_start * 100

comp_labels <- paste(
  comp_table$component_label,
  collapse = ", "
)
baseline_diag <- baseline$solver_diagnostics
optimized_diag <- optimized$solver_diagnostics

# Per-component width reduction detail lines
comp_detail <- vapply(
  seq_len(nrow(comp_table)), function(i) {
    r <- comp_table[i, ]
    paste0(
      "  ", r$component_label,
      " (component ", r$component, "): ",
      sprintf("%.4f", r$baseline_width), " -> ",
      sprintf("%.4f", r$optimized_width),
      " (reduction: ",
      sprintf("%.2f", r$pct_width_reduction), "%)"
    )
  }, character(1)
)

# Convergence status
baseline_conv <- if (
  baseline_diag$all_converged_tau0 &&
    baseline_diag$all_converged_tau_set
) {
  "All baseline bounds converged."
} else {
  paste0(
    "Baseline convergence: tau=0 ",
    ifelse(baseline_diag$all_converged_tau0,
      "OK", "INCOMPLETE"
    ),
    ", tau=", tau_set, " ",
    ifelse(baseline_diag$all_converged_tau_set,
      "OK", "INCOMPLETE"
    )
  )
}
optimized_conv <- if (optimized_diag$converged) {
  "Optimization solver converged."
} else {
  "WARNING: Optimization solver did not converge."
}

# Assemble summary text
vfci_note <- paste(
  "  The baseline uses the bundled VFCI",
  "construction by explicit",
  "implementation choice."
)
summary_lines <- c(
  "FINAL IDENTIFICATION RESULTS SUMMARY",
  "====================================",
  "", paste("Generated:", Sys.time()),
  "",
  "BASELINE GAMMA:",
  paste("  Method:", gamma_method),
  paste("  Values:", gamma_str),
  vfci_note,
  "",
  "TAU VALUES:",
  paste("  Point identification (tau):", tau_point),
  paste("  Set identification (tau):", tau_set),
  "",
  "OPTIMIZATION RESULTS:",
  paste(
    "  Total width (start):",
    sprintf("%.6f", obj_start)
  ),
  paste(
    "  Total width (final):",
    sprintf("%.6f", obj_final)
  ),
  paste(
    "  Absolute improvement:",
    sprintf("%.6f", obj_improvement)
  ),
  paste(
    "  Percent improvement:",
    sprintf("%.2f%%", obj_pct)
  ),
  "",
  paste("COMPONENTS:", comp_labels),
  "",
  "COMPONENT SUMMARY:",
  comp_detail,
  "",
  paste(
    "  Mean width reduction:",
    sprintf("%.2f%%", meta$mean_pct_reduction)
  ),
  "",
  "NUMERICAL NOTES:",
  paste(" ", baseline_conv),
  paste(" ", optimized_conv),
  paste(
    "  Optimization used",
    optimized_diag$n_starts,
    "random starts; best index:",
    optimized_diag$best_index
  )
)

txt_path <- file.path(
  paper_dir, "final_identification_summary.txt"
)
writeLines(
  paste(summary_lines, collapse = "\n"), txt_path
)
cli_alert_success(
  "Summary saved: {.path {txt_path}}"
)

# Final export listing
cli_h2("Export Summary")
cli_ul(c(
  "RDS: final_identification_results.rds",
  "CSV: final_identification_results.csv",
  "Summary: final_identification_summary.txt"
))
cli_alert_success(
  "Final identification results exported!"
)
cli_alert_info(
  "Publication files: {.path {paper_dir}}"
)
