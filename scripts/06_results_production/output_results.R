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
# Sanitize numeric bound/width/reduction columns so the published CSV carries no
# Inf/NaN: format Inf -> "unbounded" and NA -> "n/a" via the shared formatters.
publish_table <- comp_table |>
  mutate(
    baseline_lower = format_bound(baseline_lower, baseline_valid_lower),
    baseline_upper = format_bound(baseline_upper, baseline_valid_upper),
    optimized_lower = format_bound(optimized_lower, optimized_valid_lower),
    optimized_upper = format_bound(optimized_upper, optimized_valid_upper),
    baseline_width = format_width(
      baseline_width, baseline_valid_lower & baseline_valid_upper
    ),
    optimized_width = format_width(
      optimized_width, optimized_valid_lower & optimized_valid_upper
    ),
    abs_width_reduction = ifelse(is.na(abs_width_reduction), "n/a",
      formatC(abs_width_reduction, format = "f", digits = 4)
    ),
    pct_width_reduction = ifelse(is.na(pct_width_reduction), "n/a",
      formatC(pct_width_reduction, format = "f", digits = 2)
    )
  )
write.csv(publish_table, csv_path, row.names = FALSE)
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
# Improvement is defined only when BOTH objectives are finite. objective_start is
# Inf when the baseline set is unbounded; objective_final is Inf when the optimizer
# found no bounded gamma (every multi-start lands on the penalty plateau). Either
# makes the start width / percent improvement undefined, so report qualitatively
# instead of printing -Inf.
baseline_unbounded <- !is.finite(obj_start)
final_unbounded <- !is.finite(obj_final)
comparable <- !baseline_unbounded && !final_unbounded
obj_improvement <- if (comparable) obj_start - obj_final else NA_real_
obj_pct <- if (comparable) obj_improvement / obj_start * 100 else NA_real_
obj_start_str <- if (baseline_unbounded) "unbounded" else sprintf("%.6f", obj_start)
obj_final_str <- if (final_unbounded) "unbounded" else sprintf("%.6f", obj_final)
reduction_note <- if (baseline_unbounded) {
  "n/a (baseline unbounded)"
} else {
  "n/a (optimizer found no bounded gamma)"
}
obj_improvement_str <- if (comparable) {
  sprintf("%.6f", obj_improvement)
} else {
  reduction_note
}
obj_pct_str <- if (comparable) sprintf("%.2f%%", obj_pct) else reduction_note
# NA when no component has a finite, bounded baseline+optimized pair. That covers
# baseline-unbounded, optimizer-regressed, and both-unbounded alike, so stay
# neutral rather than asserting a single (possibly wrong) direction.
mean_reduction_str <- if (is.na(meta$mean_pct_reduction)) {
  "n/a (no comparable bounded rows)"
} else {
  sprintf("%.2f%%", meta$mean_pct_reduction)
}

comp_labels <- paste(
  comp_table$component_label,
  collapse = ", "
)
baseline_diag <- baseline$solver_diagnostics
optimized_diag <- optimized$solver_diagnostics

# Per-component width reduction detail lines. Widths render via format_width
# (Inf -> "unbounded") and the reduction uses the qualitative reduction_label.
comp_detail <- vapply(
  seq_len(nrow(comp_table)), function(i) {
    r <- comp_table[i, ]
    paste0(
      "  ", r$component_label,
      " (component ", r$component, "): ",
      format_width(
        r$baseline_width, r$baseline_valid_lower & r$baseline_valid_upper
      ), " -> ",
      format_width(
        r$optimized_width, r$optimized_valid_lower & r$optimized_valid_upper
      ),
      " (reduction: ", r$reduction_label, ")"
    )
  }, character(1)
)

# Baseline status: tau=0 reports KKT validity, tau=tau_set reports boundedness
# (the diagnostic field now carries boundedness, so FALSE means an unbounded set).
baseline_conv <- if (
  baseline_diag$all_converged_tau0 &&
    baseline_diag$all_converged_tau_set
) {
  paste0("Baseline tau=0 bounds valid; tau=", tau_set, " set bounded.")
} else {
  paste0(
    "Baseline status: tau=0 ",
    ifelse(baseline_diag$all_converged_tau0,
      "valid", "INVALID"
    ),
    ", tau=", tau_set, " ",
    ifelse(baseline_diag$all_converged_tau_set,
      "bounded", "unbounded"
    )
  )
}
optimized_conv <- if (optimized_diag$converged) {
  "Optimization solver converged."
} else {
  "WARNING: Optimization solver did not converge."
}

# Assemble summary text (method-aware baseline note)
baseline_note <- if (identical(gamma_method, "reduced_form")) {
  paste(
    "  The baseline uses reduced-form per-factor loadings",
    "(rank-3 benchmark; unbounded above its tau*)."
  )
} else if (identical(gamma_method, "vfci")) {
  paste(
    "  The baseline uses the bundled VFCI",
    "construction by explicit implementation choice."
  )
} else {
  paste("  Baseline gamma method:", gamma_method)
}
summary_lines <- c(
  "FINAL IDENTIFICATION RESULTS SUMMARY",
  "====================================",
  "", paste("Generated:", Sys.time()),
  "",
  "BASELINE GAMMA:",
  paste("  Method:", gamma_method),
  paste("  Values:", gamma_str),
  baseline_note,
  "",
  "TAU VALUES:",
  paste("  Point identification (tau):", tau_point),
  paste("  Set identification (tau):", tau_set),
  "",
  "OPTIMIZATION RESULTS:",
  paste(
    "  Total width (start):",
    obj_start_str
  ),
  paste(
    "  Total width (final):",
    obj_final_str
  ),
  paste(
    "  Absolute improvement:",
    obj_improvement_str
  ),
  paste(
    "  Percent improvement:",
    obj_pct_str
  ),
  "",
  paste("COMPONENTS:", comp_labels),
  "",
  "COMPONENT SUMMARY:",
  comp_detail,
  "",
  paste(
    "  Mean width reduction:",
    mean_reduction_str
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
