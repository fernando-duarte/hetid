# Output Baseline Identification Results
# Export identified sets for downstream analysis and publication

# Load required packages and settings
source(here::here("scripts/utils/common_settings.R"))
load_web_packages()

# Load identification results and analysis
base_dir <- file.path(
  OUTPUT_TEMP_DIR, "identification_baseline"
)
results <- readRDS(
  file.path(base_dir, "baseline_identification_results.rds")
)
analysis <- readRDS(
  file.path(base_dir, "baseline_identification_analysis.rds")
)

cli_h1("Exporting Baseline Identification Results")

# Create output directories
paper_dir <- file.path(OUTPUT_PAPER_DIR, "identification")
temp_dir <- base_dir
dir.create(paper_dir, recursive = TRUE, showWarnings = FALSE)

# Extract results components
lookup <- results$lookup
bounds_tau0 <- results$bounds_tau0
bounds_tau_set <- results$bounds_tau_set

# Compute width reduction percentage
width_tau0 <- bounds_tau0$upper - bounds_tau0$lower
width_tau_set <- bounds_tau_set$upper - bounds_tau_set$lower
width_reduction_pct <- (
  (width_tau0 - width_tau_set) / width_tau0 * 100
)

# Build the main results table
table_df <- data.frame(
  Bond_Maturity = lookup$bond_maturity,
  Component_ID = lookup$component_id,
  Lower_tau0 = bounds_tau0$lower,
  Upper_tau0 = bounds_tau0$upper,
  Lower_tau02 = bounds_tau_set$lower,
  Upper_tau02 = bounds_tau_set$upper,
  Width_Reduction_Pct = width_reduction_pct
)

cli_h2("Creating Publication-Ready Tables")

# Build gt table
tbl <- gt(table_df) |>
  tab_header(title = "Baseline Identified Set") |>
  cols_label(
    Bond_Maturity = "Bond Maturity",
    Component_ID = "Component ID",
    Lower_tau0 = html("Lower (&tau;=0)"),
    Upper_tau0 = html("Upper (&tau;=0)"),
    Lower_tau02 = html("Lower (&tau;=0.2)"),
    Upper_tau02 = html("Upper (&tau;=0.2)"),
    Width_Reduction_Pct = "Width Reduction (%)"
  ) |>
  fmt_number(
    columns = c(
      Lower_tau0, Upper_tau0,
      Lower_tau02, Upper_tau02
    ),
    decimals = 4
  ) |>
  fmt_number(columns = Width_Reduction_Pct, decimals = 2)

# Save HTML
html_path <- file.path(
  paper_dir, "baseline_identification_table.html"
)
gtsave(tbl, html_path)

# Save LaTeX
latex_out <- as_latex(tbl)
tex_path <- file.path(
  paper_dir, "baseline_identification_table.tex"
)
writeLines(as.character(latex_out), tex_path)

cli_h2("Creating Data Exports")

# Save CSV to temp directory
csv_path <- file.path(
  temp_dir, "baseline_identification_table.csv"
)
write.csv(table_df, csv_path, row.names = FALSE)

cli_h2("Creating Analysis Summary")

# Extract summary statistics from analysis object
bounds_combined <- analysis$bounds_combined
width_summary <- analysis$width_summary
diagnostics <- analysis$diagnostics

# Gather metadata for summary
gamma_method <- attr(results$gamma_baseline, "method")
n_components <- nrow(lookup)
mat_range <- range(lookup$bond_maturity)
mean_w_tau0 <- mean(width_tau0, na.rm = TRUE)
mean_w_tau_set <- mean(width_tau_set, na.rm = TRUE)

# Count converged bounds
n_conv_tau0 <- sum(
  is.finite(bounds_tau0$lower) &
    is.finite(bounds_tau0$upper)
)
n_conv_tau_set <- sum(
  is.finite(bounds_tau_set$lower) &
    is.finite(bounds_tau_set$upper)
)

# Build plain text summary
summary_lines <- c(
  "BASELINE IDENTIFICATION RESULTS SUMMARY",
  "=======================================",
  "",
  paste("Analysis Date:", Sys.Date()),
  paste("Gamma method:", gamma_method),
  "",
  "CONFIGURATION:",
  "  Tau values: 0 (point), 0.2 (set)",
  paste("  Number of components:", n_components),
  paste(
    "  Maturity range:",
    mat_range[1], "to", mat_range[2]
  ),
  "",
  "BOUNDS WIDTH:",
  paste(
    "  Mean width at tau=0:",
    sprintf("%.6f", mean_w_tau0)
  ),
  paste(
    "  Mean width at tau=0.2:",
    sprintf("%.6f", mean_w_tau_set)
  ),
  "",
  "CONVERGENCE:",
  paste(
    "  Converged bounds (tau=0):",
    n_conv_tau0, "of", n_components
  ),
  paste(
    "  Converged bounds (tau=0.2):",
    n_conv_tau_set, "of", n_components
  )
)

summary_text <- paste(summary_lines, collapse = "\n")
txt_path <- file.path(
  temp_dir, "baseline_identification_summary.txt"
)
writeLines(summary_text, txt_path)

cli_h2("Export Summary")

cli_ul(c(
  "Results table (HTML): baseline_identification_table.html",
  "Results table (LaTeX): baseline_identification_table.tex",
  "Results table (CSV): baseline_identification_table.csv",
  "Text summary: baseline_identification_summary.txt"
))

cli_alert_success(
  "Baseline identification results exported!"
)
cli_alert_info(
  "Publication files saved to: {.path {paper_dir}}"
)
cli_alert_info(
  "Working files saved to: {.path {temp_dir}}"
)
