# Export generalized-instrument (Z = PC^2) identification results.
# Reads the stage-07 compute RDS; writes a labeled CSV + a plain-text summary.

source(here::here("scripts/utils/common_settings.R"))

base_dir <- file.path(OUTPUT_TEMP_DIR, "identification_generalized")
results <- readRDS(
  file.path(base_dir, "generalized_identification_results.rds")
)

cli_h1("Exporting Generalized-Instrument Identification Results")

paper_dir <- file.path(OUTPUT_PAPER_DIR, "identification")
dir.create(paper_dir, recursive = TRUE, showWarnings = FALSE)

lookup <- results$lookup
bounds <- results$bounds_tau_set
membership <- results$membership

# Labeled bounds table (validity-aware bound formatting, matching other stages).
table_df <- data.frame(
  Component = lookup$component_label,
  Component_ID = lookup$component_id,
  Lower = format_bound(bounds$lower, bounds$valid_lower),
  Upper = format_bound(bounds$upper, bounds$valid_upper),
  Width = format_bound(bounds$width, bounds$valid_lower & bounds$valid_upper)
)
csv_path <- file.path(base_dir, "generalized_identification_table.csv")
write.csv(table_df, csv_path, row.names = FALSE)
write.csv(table_df, file.path(paper_dir, "generalized_identification_table.csv"),
  row.names = FALSE
)

# Plain-text summary including the closure membership diagnostic.
s <- membership$summary
summary_lines <- c(
  "GENERALIZED-INSTRUMENT IDENTIFICATION (Z = PC^2)",
  "================================================",
  "",
  paste("Analysis Date:", Sys.Date()),
  paste("Instruments:", paste(results$spec$instruments, collapse = ", ")),
  paste("Components:", paste(lookup$component_label, collapse = ", ")),
  paste("Constraints (separate instruments):", results$spec$constraints),
  paste("Tau (set):", results$spec$tau_set),
  "",
  "CLOSURE MEMBERSHIP PROBE (theta in set <=> max constraint <= 0):",
  paste(
    "  Grid points:", s$n_grid, "| inside:", s$n_inside,
    sprintf("(%.1f%%)", 100 * s$frac_inside)
  ),
  paste("  Box center inside:", s$center_inside),
  paste("  Box corners inside:", s$n_corners_inside, "of", s$n_corners),
  paste("  Tightest constraint at center:", s$tightest_constraint),
  paste("  Fallback axes used (unbounded sides):", s$any_fallback)
)
txt_path <- file.path(base_dir, "generalized_identification_summary.txt")
writeLines(paste(summary_lines, collapse = "\n"), txt_path)

cli_ul(c(
  "Results table (CSV): generalized_identification_table.csv",
  "Text summary: generalized_identification_summary.txt"
))
cli_alert_success("Generalized-instrument results exported!")
cli_alert_info("Publication files saved to: {.path {paper_dir}}")
cli_alert_info("Working files saved to: {.path {base_dir}}")
