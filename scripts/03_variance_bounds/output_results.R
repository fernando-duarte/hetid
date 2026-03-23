# Output Variance Bound Results
# Export variance bound results for downstream analysis and publication

# Load required packages and settings
source(here::here("scripts/utils/common_settings.R"))
load_web_packages()

# Load variance bounds results and analysis
vb_results_path <- file.path(
  OUTPUT_DIR, "temp/variance_bounds/variance_bounds_results.rds"
)
vb_analysis_path <- file.path(
  OUTPUT_DIR, "temp/variance_bounds/variance_bounds_analysis.rds"
)
variance_bounds_results <- readRDS(vb_results_path)
analysis_results <- readRDS(vb_analysis_path)

cli_h1("Exporting Variance Bounds Results")

# Create output directories
output_paper_dir <- file.path(OUTPUT_DIR, "for_paper/variance_bounds")
output_temp_dir <- file.path(OUTPUT_DIR, "temp/variance_bounds")
dir.create(output_paper_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_temp_dir, recursive = TRUE, showWarnings = FALSE)

# Extract main results
variance_bounds_df <- variance_bounds_results$variance_bounds_df
summary_stats <- variance_bounds_results$summary_statistics
correlations <- variance_bounds_results$correlations

cli_h2("Creating Publication-Ready Tables")

# Table 1: Main variance bounds results
main_table <- variance_bounds_df |>
  gt() |>
  tab_header(
    title = "Theoretical Variance Bounds for Forecast Errors",
    subtitle = "Components and bounds across bond maturities"
  ) |>
  fmt_number(
    columns = c(c_hat, k_hat),
    decimals = 4
  ) |>
  fmt_scientific(
    columns = Variance_Bound,
    decimals = 2
  ) |>
  cols_label(
    Maturity = "Maturity (years)",
    c_hat = html("ĉ<sub>i</sub>"),
    k_hat = html("k̂<sub>i</sub>"),
    Variance_Bound = html("Variance Bound<br/>(×10<sup>-3</sup>)")
  ) |>
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(columns = Maturity)
  ) |>
  tab_style(
    style = cell_fill(color = "lightyellow"),
    locations = cells_body(columns = Variance_Bound)
  ) |>
  tab_footnote(
    footnote = "Variance bound = (1/4) × ĉᵢ × k̂ᵢ",
    locations = cells_column_labels(columns = Variance_Bound)
  ) |>
  tab_footnote(
    footnote = "ĉᵢ = max_t exp(2 × n̂(i,t))",
    locations = cells_column_labels(columns = c_hat)
  ) |>
  tab_footnote(
    footnote = "k̂ᵢ = E[(p_(t+i)^(1) - E_(t+1)[p_(t+i)^(1)])^4]",
    locations = cells_column_labels(columns = k_hat)
  )

# Save main table
gtsave(main_table, file.path(output_paper_dir, "variance_bounds_table.html"))

# Save as PNG if webshot2 is available
if (requireNamespace("webshot2", quietly = TRUE)) {
  gtsave(main_table, file.path(output_paper_dir, "variance_bounds_table.png"))
} else {
  cli_alert_warning("webshot2 package not available. Skipping PNG export.")
}

# Table 2: Summary statistics
summary_table <- data.frame(
  Statistic = c("Mean", "Median", "Minimum", "Maximum", "Standard Deviation"),
  Value = c(
    summary_stats$mean,
    summary_stats$median,
    summary_stats$min,
    summary_stats$max,
    summary_stats$sd
  )
) |>
  gt() |>
  tab_header(
    title = "Variance Bounds Summary Statistics",
    subtitle = "Distributional properties across maturities"
  ) |>
  fmt_scientific(
    columns = Value,
    decimals = 2
  ) |>
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(columns = Statistic)
  )

gtsave(summary_table, file.path(output_paper_dir, "variance_bounds_summary.html"))

# Table 3: Component correlations
correlation_table <- data.frame(
  Component_Pair = c("ĉ and k̂", "ĉ and Variance Bound", "k̂ and Variance Bound"),
  Correlation = c(
    correlations$c_hat_k_hat,
    correlations$c_hat_variance_bound,
    correlations$k_hat_variance_bound
  )
) |>
  gt() |>
  tab_header(
    title = "Component Correlations",
    subtitle = "Relationships between variance bound components"
  ) |>
  fmt_number(
    columns = Correlation,
    decimals = 3
  ) |>
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = Correlation,
      rows = abs(Correlation) > 0.7
    )
  )

gtsave(correlation_table, file.path(output_paper_dir, "component_correlations.html"))

cli_h2("Creating Data Exports")

# Export main data in multiple formats
write.csv(
  variance_bounds_df,
  file.path(output_paper_dir, "variance_bounds_data.csv"),
  row.names = FALSE
)
write.csv(
  variance_bounds_df,
  file.path(output_temp_dir, "variance_bounds_data.csv"),
  row.names = FALSE
)

# Export for LaTeX
latex_table <- variance_bounds_df |>
  mutate(
    c_hat = sprintf("%.4f", c_hat),
    k_hat = sprintf("%.4f", k_hat),
    Variance_Bound = sprintf("%.2e", Variance_Bound)
  )

# Create LaTeX table code
latex_code <- paste0(
  "\\begin{table}[htbp]\n",
  "\\centering\n",
  "\\caption{Theoretical Variance Bounds for Forecast Errors}\n",
  "\\label{tab:variance_bounds}\n",
  "\\begin{tabular}{cccc}\n",
  "\\toprule\n",
  "Maturity & $\\hat{c}_i$ & $\\hat{k}_i$ & Variance Bound \\\\\n",
  "\\midrule\n"
)

for (i in seq_len(nrow(latex_table))) {
  latex_code <- paste0(
    latex_code,
    latex_table$Maturity[i], " & ",
    latex_table$c_hat[i], " & ",
    latex_table$k_hat[i], " & ",
    latex_table$Variance_Bound[i], " \\\\\n"
  )
}

latex_code <- paste0(
  latex_code,
  "\\bottomrule\n",
  "\\end{tabular}\n",
  "\\begin{tablenotes}\n",
  "\\small\n",
  "\\item Note: Variance bound = $(1/4) \\times \\hat{c}_i \\times \\hat{k}_i$ where ",
  "$\\hat{c}_i = \\max_t \\exp(2 \\times \\hat{n}(i,t))$ and ",
  "$\\hat{k}_i = E[(p_{t+i}^{(1)} - E_{t+1}[p_{t+i}^{(1)}])^4]$.\n",
  "\\end{tablenotes}\n",
  "\\end{table}\n"
)

writeLines(latex_code, file.path(output_paper_dir, "variance_bounds_table.tex"))

cli_h2("Creating Analysis Summary")

# Create comprehensive analysis summary
analysis_summary <- list(
  title = "Variance Bounds Analysis Summary",
  date_created = Sys.Date(),
  data_summary = list(
    n_maturities = nrow(variance_bounds_df),
    maturity_range = paste(range(variance_bounds_df$Maturity), collapse = " to "),
    variance_bound_range = paste(
      sprintf("%.2e", range(variance_bounds_df$Variance_Bound)),
      collapse = " to "
    )
  ),
  key_findings = list(
    mean_variance_bound = summary_stats$mean,
    median_variance_bound = summary_stats$median,
    highest_bound_maturity = variance_bounds_df$Maturity[
      which.max(variance_bounds_df$Variance_Bound)
    ],
    lowest_positive_bound_maturity = variance_bounds_df$Maturity[
      which.min(
        variance_bounds_df$Variance_Bound[
          variance_bounds_df$Variance_Bound > 0
        ]
      )
    ],
    c_hat_k_hat_correlation = correlations$c_hat_k_hat,
    monotonicity_ratio =
      variance_bounds_results$monotonicity$monotonicity_ratio
  ),
  interpretation = list(
    general_trend = if (
      variance_bounds_results$monotonicity$monotonicity_ratio > 0.6
    ) {
      "Generally increasing with maturity"
    } else {
      "Mixed pattern across maturities"
    },
    component_dominance = if (
      abs(correlations$c_hat_variance_bound) >
        abs(correlations$k_hat_variance_bound)
    ) {
      "c_hat component more influential"
    } else {
      "k_hat component more influential"
    },
    identification_potential = if (
      any(variance_bounds_df$Variance_Bound < 0.01)
    ) {
      "Some bounds tight enough for identification"
    } else {
      "Bounds may be loose for identification"
    }
  )
)

# Save analysis summary
saveRDS(analysis_summary, file.path(output_paper_dir, "analysis_summary.rds"))
saveRDS(analysis_summary, file.path(output_temp_dir, "analysis_summary.rds"))

# Create human-readable summary
kf <- analysis_summary$key_findings
interp <- analysis_summary$interpretation
ds <- analysis_summary$data_summary
mean_vb <- sprintf("%.2e", kf$mean_variance_bound)
median_vb <- sprintf("%.2e", kf$median_variance_bound)
ck_corr <- round(kf$c_hat_k_hat_correlation, 3)
mono_ratio <- round(kf$monotonicity_ratio, 3)
summary_text <- paste0(
  "VARIANCE BOUNDS ANALYSIS SUMMARY\n",
  "================================\n\n",
  "Analysis Date: ", Sys.Date(), "\n",
  "Number of Maturities: ", ds$n_maturities, "\n",
  "Maturity Range: ", ds$maturity_range, " years\n",
  "Variance Bound Range: ", ds$variance_bound_range,
  "\n\n",
  "KEY FINDINGS:\n",
  "- Mean Variance Bound: ", mean_vb, "\n",
  "- Median Variance Bound: ", median_vb, "\n",
  "- Highest Bound at Maturity: ",
  kf$highest_bound_maturity, " years\n",
  "- Lowest Positive Bound at Maturity: ",
  kf$lowest_positive_bound_maturity, " years\n",
  "- Component Correlation (c-hat, k-hat): ", ck_corr, "\n",
  "- Monotonicity Ratio: ", mono_ratio, "\n\n",
  "INTERPRETATION:\n",
  "- Trend: ", interp$general_trend, "\n",
  "- Component Dominance: ", interp$component_dominance, "\n",
  "- Identification Potential: ",
  interp$identification_potential, "\n"
)

writeLines(summary_text, file.path(output_paper_dir, "variance_bounds_summary.txt"))
writeLines(summary_text, file.path(output_temp_dir, "variance_bounds_summary.txt"))

cli_h2("Copying Visualization Files")

# Copy plots from temp to paper directory
temp_plots_dir <- file.path(OUTPUT_DIR, "temp/variance_bounds")
paper_plots_dir <- file.path(output_paper_dir, "figures")
dir.create(paper_plots_dir, recursive = TRUE, showWarnings = FALSE)

plot_files <- c(
  "variance_bounds_by_maturity.png",
  "variance_bound_components.png",
  "log_variance_bounds.png"
)

for (plot_file in plot_files) {
  if (file.exists(file.path(temp_plots_dir, plot_file))) {
    file.copy(
      from = file.path(temp_plots_dir, plot_file),
      to = file.path(paper_plots_dir, plot_file),
      overwrite = TRUE
    )
  }
}

cli_h2("Export Summary")

cli_ul(c(
  paste("Main results table:", "variance_bounds_table.html"),
  paste("Summary statistics:", "variance_bounds_summary.html"),
  paste("Component correlations:", "component_correlations.html"),
  paste("Data export:", "variance_bounds_data.csv"),
  paste("LaTeX table:", "variance_bounds_table.tex"),
  paste("Analysis summary:", "analysis_summary.rds"),
  paste("Plots copied:", length(plot_files), "files")
))

cli_alert_success("Variance bounds results exported successfully!")
cli_alert_info("Publication-ready files saved to: {.path {output_paper_dir}}")
cli_alert_info("Working files saved to: {.path {output_temp_dir}}")
