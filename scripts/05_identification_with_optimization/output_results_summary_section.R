# Analysis-summary section of the stage-05 output_results.R export: gathers
# the optimization summary statistics and writes the plain-text summary.
# Sourced IN PLACE by output_results.R -- not standalone; expects results,
# analysis, the width/validity vectors, and temp_dir in the calling
# environment.

cli_h2("Creating Analysis Summary")

# Gather summary statistics
obj_start <- results$objective_start
obj_final <- results$objective_final
mean_cosine <- mean(
  analysis$gamma_similarity$cosine_sim,
  na.rm = TRUE
)
# Mean reduction over bounded AND valid components only; NA when none qualify.
total_reduction <- mean_pct_reduction(
  baseline_width, optimized_width, base_w_valid, opt_w_valid
)
# Honest objective-start string: "unbounded" rather than the 1e6 penalty/Inf.
obj_start_str <- if (is.finite(obj_start)) {
  sprintf("%.6f", obj_start)
} else {
  "unbounded"
}
# Neutral when NA: the unusable rows may be unbounded OR finite-but-invalid.
total_reduction_str <- if (is.finite(total_reduction)) {
  sprintf("%.2f", total_reduction)
} else {
  "n/a (no comparable bounded rows)"
}
tau_fixed <- results$tau_fixed

# Build plain text summary
summary_lines <- c(
  "OPTIMIZED IDENTIFICATION RESULTS SUMMARY",
  "========================================",
  "",
  paste("Analysis Date:", Sys.Date()),
  paste("Fixed tau:", tau_fixed),
  "",
  "OPTIMIZATION:",
  paste(
    "  Objective (start):",
    obj_start_str
  ),
  paste(
    "  Objective (final):",
    sprintf("%.6f", obj_final)
  ),
  "",
  "WIDTH REDUCTION:",
  paste(
    "  Mean width reduction (%):",
    total_reduction_str
  ),
  "",
  "GAMMA COMPARISON:",
  paste(
    "  Mean cosine similarity:",
    sprintf("%.6f", mean_cosine)
  )
)

summary_text <- paste(summary_lines, collapse = "\n")
txt_path <- file.path(
  temp_dir, "optimized_identification_summary.txt"
)
writeLines(summary_text, txt_path)
