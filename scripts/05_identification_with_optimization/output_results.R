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

# Compute widths and reduction
baseline_width <- baseline_bounds$upper - baseline_bounds$lower
optimized_width <- optimized_bounds$upper -
  optimized_bounds$lower
width_reduction_pct <- analysis$width_comparison$percent_reduction

# Build the main results table
table_df <- data.frame(
  Component = lookup$component_label,
  Baseline_Lower = baseline_bounds$lower,
  Baseline_Upper = baseline_bounds$upper,
  Optimized_Lower = optimized_bounds$lower,
  Optimized_Upper = optimized_bounds$upper,
  Baseline_Width = baseline_width,
  Optimized_Width = optimized_width,
  Width_Reduction_Pct = width_reduction_pct
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
    Width_Reduction_Pct = "Width Reduction (%)"
  ) |>
  fmt_number(
    columns = c(
      Baseline_Lower, Baseline_Upper,
      Optimized_Lower, Optimized_Upper,
      Baseline_Width, Optimized_Width
    ),
    decimals = 4
  ) |>
  fmt_number(
    columns = Width_Reduction_Pct, decimals = 2
  )

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
rownames(gamma_df) <- paste0(
  "pc", seq_len(nrow(gamma_df))
)
gamma_path <- file.path(
  temp_dir, "optimized_gamma_matrix.csv"
)
write.csv(gamma_df, gamma_path)

cli_h2("Creating Analysis Summary")

# Gather summary statistics
obj_start <- results$objective_start
obj_final <- results$objective_final
mean_cosine <- mean(
  analysis$gamma_similarity,
  na.rm = TRUE
)
total_reduction <- mean(
  width_reduction_pct,
  na.rm = TRUE
)
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
    sprintf("%.6f", obj_start)
  ),
  paste(
    "  Objective (final):",
    sprintf("%.6f", obj_final)
  ),
  "",
  "WIDTH REDUCTION:",
  paste(
    "  Mean width reduction (%):",
    sprintf("%.2f", total_reduction)
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
