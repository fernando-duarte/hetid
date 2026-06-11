# Output Optimized Identification Results
# Export optimized results for the final production stage

# Load required packages and settings
source(here::here("scripts/utils/common_settings.R"))
load_web_packages()

# Load optimized identification results and analysis
base_dir <- file.path(
  OUTPUT_TEMP_DIR, "identification_optimized"
)
results <- readRDS(
  file.path(base_dir, "optimized_identification_results.rds")
)
analysis <- readRDS(
  file.path(base_dir, "optimized_identification_analysis.rds")
)

cli_h1("Exporting Optimized Identification Results")

# Create output directories
paper_dir <- file.path(OUTPUT_PAPER_DIR, "identification")
temp_dir <- base_dir
dir.create(
  paper_dir,
  recursive = TRUE, showWarnings = FALSE
)

# Extract results components
lookup <- results$lookup
baseline_bounds <- results$baseline_bounds
optimized_bounds <- results$optimized_bounds

# Compute widths and reduction (numeric -- kept for the summary mean below)
baseline_width <- baseline_bounds$upper - baseline_bounds$lower
optimized_width <- optimized_bounds$upper -
  optimized_bounds$lower
# A width is reliable only when BOTH bounds are valid. Pass an explicit
# length-matched validity vector (default scalar TRUE collapses the ifelse).
base_w_valid <- baseline_bounds$valid_lower & baseline_bounds$valid_upper
opt_w_valid <- optimized_bounds$valid_lower & optimized_bounds$valid_upper
# The reduction label is also validity-aware: an unreliable bound must not yield
# a confident percentage.
width_reduction_label <- format_reduction(
  baseline_width, optimized_width, base_w_valid, opt_w_valid
)

# Finite/Inf-aware formatters turn unbounded bounds/widths into "unbounded".
table_df <- data.frame(
  Component = lookup$component_label,
  Baseline_Lower = format_bound(baseline_bounds$lower, baseline_bounds$valid_lower),
  Baseline_Upper = format_bound(baseline_bounds$upper, baseline_bounds$valid_upper),
  Optimized_Lower = format_bound(optimized_bounds$lower, optimized_bounds$valid_lower),
  Optimized_Upper = format_bound(optimized_bounds$upper, optimized_bounds$valid_upper),
  Baseline_Width = format_width(baseline_width, base_w_valid),
  Optimized_Width = format_width(optimized_width, opt_w_valid),
  Width_Reduction = width_reduction_label
)

cli_h2("Creating Publication-Ready Tables")

# Build gt table
tbl <- gt(table_df) |>
  tab_header(
    title = "Optimized Identified Set"
  ) |>
  cols_label(
    Component = "Component",
    Baseline_Lower = "Baseline Lower",
    Baseline_Upper = "Baseline Upper",
    Optimized_Lower = "Optimized Lower",
    Optimized_Upper = "Optimized Upper",
    Baseline_Width = "Baseline Width",
    Optimized_Width = "Optimized Width",
    Width_Reduction = "Width Reduction"
  )
# Bound/width columns are pre-formatted character; no fmt_number (errors in gt).

# Save HTML
html_path <- file.path(
  paper_dir,
  "optimized_identification_table.html"
)
gtsave(tbl, html_path)

# Save LaTeX
latex_out <- as_latex(tbl)
tex_path <- file.path(
  paper_dir,
  "optimized_identification_table.tex"
)
writeLines(as.character(latex_out), tex_path)

cli_h2("Creating Data Exports")

# Save CSV to temp directory
csv_path <- file.path(
  temp_dir, "optimized_identification_table.csv"
)
write.csv(table_df, csv_path, row.names = FALSE)

# Save gamma matrix as CSV
gamma_df <- as.data.frame(results$gamma_optimized)
colnames(gamma_df) <- paste0(
  "component_", seq_len(ncol(gamma_df))
)
# Instrument names travel on the baseline (gamma_start) rownames under the
# custom hooks; default gammas carry none, keeping the historical pc labels.
gamma_row_names <- rownames(results$gamma_start)
rownames(gamma_df) <- if (is.null(gamma_row_names)) {
  paste0("pc", seq_len(nrow(gamma_df)))
} else {
  gamma_row_names
}
gamma_path <- file.path(
  temp_dir, "optimized_gamma_matrix.csv"
)
write.csv(gamma_df, gamma_path)

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

cli_h2("Export Summary")

cli_ul(c(
  paste(
    "Results table (HTML):",
    "optimized_identification_table.html"
  ),
  paste(
    "Results table (LaTeX):",
    "optimized_identification_table.tex"
  ),
  paste(
    "Results table (CSV):",
    "optimized_identification_table.csv"
  ),
  "Gamma matrix (CSV): optimized_gamma_matrix.csv",
  "Text summary: optimized_identification_summary.txt"
))

cli_alert_success(
  "Optimized identification results exported!"
)
cli_alert_info(
  "Publication files saved to: {.path {paper_dir}}"
)
cli_alert_info(
  "Working files saved to: {.path {temp_dir}}"
)
