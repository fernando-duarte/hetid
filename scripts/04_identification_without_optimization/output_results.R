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
paper_dir <- file.path(OUTPUT_TEMP_DIR, "identification")
temp_dir <- base_dir
dir.create(paper_dir, recursive = TRUE, showWarnings = FALSE)

# Extract results components
lookup <- results$lookup
bounds_tau0 <- results$bounds_tau0
bounds_tau_set <- results$bounds_tau_set

# Compute width reduction label. tau = 0.05 is the wider/unbounded baseline spec,
# tau = 0 the point-ID target, so the reduction is measured baseline -> point.
width_tau0 <- bounds_tau0$upper - bounds_tau0$lower
width_tau_set <- bounds_tau_set$upper - bounds_tau_set$lower
# Validity-aware: render "unreliable" rather than a confident percentage when
# either side's width came from a bound that failed its feasibility check.
valid_tau_set <- bounds_tau_set$valid_lower & bounds_tau_set$valid_upper
valid_tau0 <- bounds_tau0$valid_lower & bounds_tau0$valid_upper
width_reduction_label <- format_reduction(
  width_tau_set, width_tau0, valid_tau_set, valid_tau0
)

# Build the main results table. Bounds are formatted to character with the
# finite/Inf/validity-aware helper so Inf renders "unbounded" and failures
# "unreliable" (never spurious large numbers).
table_df <- data.frame(
  Component = lookup$component_label,
  Component_ID = lookup$component_id,
  Lower_tau0 = format_bound(bounds_tau0$lower, bounds_tau0$valid_lower),
  Upper_tau0 = format_bound(bounds_tau0$upper, bounds_tau0$valid_upper),
  Lower_tau02 = format_bound(bounds_tau_set$lower, bounds_tau_set$valid_lower),
  Upper_tau02 = format_bound(bounds_tau_set$upper, bounds_tau_set$valid_upper),
  Width_Reduction = width_reduction_label
)

cli_h2("Creating Publication-Ready Tables")

# Build gt table
# All bound columns and Width_Reduction are pre-formatted character (so Inf
# renders "unbounded"); gt::fmt_number would error on character, so omit it.
tbl <- gt(table_df) |>
  tab_header(title = "Baseline Identified Set") |>
  cols_label(
    Component = "Component",
    Component_ID = "Component ID",
    Lower_tau0 = html("Lower (&tau;=0)"),
    Upper_tau0 = html("Upper (&tau;=0)"),
    Lower_tau02 = html("Lower (&tau;=0.05)"),
    Upper_tau02 = html("Upper (&tau;=0.05)"),
    Width_Reduction = "Width Reduction"
  )

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
comp_labels <- paste(
  lookup$component_label,
  collapse = ", "
)
# Mean widths over finite components only; "unbounded" when all are infinite.
fmt_mean_width <- function(w) {
  if (all(!is.finite(w))) {
    "unbounded"
  } else {
    paste0(
      sprintf("%.6f", mean(w[is.finite(w)])),
      if (any(!is.finite(w))) " (some unbounded)" else ""
    )
  }
}
mean_w_tau0 <- fmt_mean_width(width_tau0)
mean_w_tau_set <- fmt_mean_width(width_tau_set)

# Count bounded (finite both-sided) components
n_bnd_tau0 <- sum(
  is.finite(bounds_tau0$lower) &
    is.finite(bounds_tau0$upper)
)
n_bnd_tau_set <- sum(
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
  "  Tau values: 0 (point), 0.05 (set)",
  paste("  Number of components:", n_components),
  paste("  Components:", comp_labels),
  "",
  "BOUNDS WIDTH:",
  paste("  Mean width at tau=0:", mean_w_tau0),
  paste("  Mean width at tau=0.05:", mean_w_tau_set),
  "",
  "BOUNDEDNESS:",
  paste(
    "  Bounded components (tau=0):",
    n_bnd_tau0, "of", n_components
  ),
  paste(
    "  Bounded components (tau=0.05):",
    n_bnd_tau_set, "of", n_components
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
