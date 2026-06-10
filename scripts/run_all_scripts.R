# Main Script to Run All Analysis Scripts
# This script executes all scripts in the hetid package in the proper order
# Run this script directly in RStudio by clicking Source or using Ctrl/Cmd+Shift+S

# Load required packages
library(here)
library(cli)

# Set up timing
start_time <- Sys.time()

cli_h1("Running All hetid Analysis Scripts")
cli_text("Started at: {.timestamp {start_time}}")
cli_rule()

# Helper function to run a script with error handling
# `env` (optional named character vector) sets environment variables for the
# duration of this script only, then restores them, so per-script switches like
# HETID_SPEC_QUICK do not leak into the rest of the pipeline.
run_script <- function(script_path, description, env = NULL) {
  script_name <- basename(script_path)
  cli_h2(description)
  cli_text("Running: {.file {script_name}}")

  if (length(env)) {
    do.call(Sys.setenv, as.list(env))
    on.exit(Sys.unsetenv(names(env)), add = TRUE)
  }

  script_start <- Sys.time()

  tryCatch(
    {
      source(script_path, echo = FALSE)
      script_end <- Sys.time()
      elapsed <- round(as.numeric(difftime(script_end, script_start, units = "secs")), 2)
      cli_alert_success("Completed in {.val {elapsed}} seconds")
    },
    error = function(e) {
      cli_alert_danger("Error in {.file {script_name}}: {.val {e$message}}")
      stop(e)
    }
  )

  cli_rule()
}

# Define the order of script execution
scripts_to_run <- list(
  # Data preparation and analysis scripts
  list(
    path = here::here("scripts/01_data_analysis/create_data.R"),
    desc = "Creating and Loading Data"
  ),
  list(
    path = here::here("scripts/01_data_analysis/summary_statistics.R"),
    desc = "Computing Summary Statistics"
  ),
  list(
    path = here::here("scripts/01_data_analysis/visualize_raw_data.R"),
    desc = "Creating Data Visualizations"
  ),
  list(
    path = here::here("scripts/01_data_analysis/time_series_properties.R"),
    desc = "Analyzing Time Series Properties"
  ),

  # Identification diagnostics (heteroskedasticity tests and n-hat context)
  list(
    path = here::here(
      "scripts/02_identification_diagnostics",
      "heteroskedasticity_tests.R"
    ),
    desc = "Testing W2 Residuals for Heteroskedasticity"
  ),
  list(
    path = here::here(
      "scripts/02_identification_diagnostics",
      "n_hat_episodes.R"
    ),
    desc = "Analyzing n-hat Episodes and Economic Context"
  ),
  list(
    path = here::here(
      "scripts/02_identification_diagnostics",
      "output_results.R"
    ),
    desc = "Exporting Identification Diagnostics"
  ),

  # Variance bounds analysis
  list(
    path = here::here("scripts/03_variance_bounds/compute_variance_bounds.R"),
    desc = "Computing Theoretical Variance Bounds"
  ),
  list(
    path = here::here("scripts/03_variance_bounds/analyze_bounds.R"),
    desc = "Analyzing Variance Bounds"
  ),
  list(
    path = here::here("scripts/03_variance_bounds/output_results.R"),
    desc = "Exporting Variance Bounds Results"
  ),

  # Baseline identification (fixed PC weights)
  list(
    path = here::here(
      "scripts/04_identification_without_optimization",
      "compute_identification.R"
    ),
    desc = "Computing Baseline Identification"
  ),
  list(
    path = here::here(
      "scripts/04_identification_without_optimization",
      "analyze_identification.R"
    ),
    desc = "Analyzing Baseline Identification"
  ),
  list(
    path = here::here(
      "scripts/04_identification_without_optimization",
      "output_results.R"
    ),
    desc = "Exporting Baseline Identification Results"
  ),
  list(
    path = here::here(
      "scripts/04_identification_without_optimization",
      "compute_identification_ixj.R"
    ),
    desc = "Computing I x J Per-Instrument Identified Set"
  ),

  # Optimized identification (PC weight optimization)
  list(
    path = here::here(
      "scripts/05_identification_with_optimization",
      "optimize_identification.R"
    ),
    desc = "Optimizing PC Weights (Gamma)"
  ),
  list(
    path = here::here(
      "scripts/05_identification_with_optimization",
      "analyze_optimization.R"
    ),
    desc = "Analyzing Optimization Results"
  ),
  list(
    path = here::here(
      "scripts/05_identification_with_optimization",
      "output_results.R"
    ),
    desc = "Exporting Optimization Results"
  ),
  list(
    path = here::here(
      "scripts/05_identification_with_optimization",
      "tau_star_comparison.R"
    ),
    desc = "Comparing Identification Strength (tau*)"
  ),
  list(
    path = here::here(
      "scripts/05_identification_with_optimization",
      "tau_sensitivity.R"
    ),
    desc = "Locating tau* (Bounded-to-Unbounded Sensitivity)"
  ),
  list(
    path = here::here(
      "scripts/05_identification_with_optimization",
      "spec_comparison.R"
    ),
    desc = "Comparing Specifications, Instruments, and tau (quick)",
    env = c(HETID_SPEC_QUICK = "1")
  ),

  # Final results production
  list(
    path = here::here(
      "scripts/06_results_production",
      "assemble_results.R"
    ),
    desc = "Assembling Final Results"
  ),
  list(
    path = here::here(
      "scripts/06_results_production",
      "create_tables_and_figures.R"
    ),
    desc = "Creating Publication Tables and Figures"
  ),
  list(
    path = here::here(
      "scripts/06_results_production",
      "create_theta_panel_table.R"
    ),
    desc = "Creating Theta Panel LaTeX Table"
  ),
  list(
    path = here::here(
      "scripts/06_results_production",
      "output_results.R"
    ),
    desc = "Exporting Final Results"
  )
)

# Run all scripts
cli_h1("Executing Scripts")

for (script_info in scripts_to_run) {
  run_script(script_info$path, script_info$desc, script_info$env)
}

# Summary
end_time <- Sys.time()
total_elapsed <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 2)

cli_h1("Analysis Complete")
cli_alert_success("All scripts executed successfully!")
cli_text("Total time: {.val {total_elapsed}} minutes")
cli_text("Completed at: {.timestamp {end_time}}")

# List output directories
cli_h2("Output Files Created")
output_dir <- here::here("scripts/output")
if (dir.exists(output_dir)) {
  temp_dir <- file.path(output_dir, "temp")

  # Count files by type
  if (dir.exists(temp_dir)) {
    all_files <- list.files(temp_dir, recursive = TRUE, full.names = TRUE)

    n_csv <- sum(grepl("\\.csv$", all_files))
    n_rds <- sum(grepl("\\.rds$", all_files))
    n_svg <- sum(grepl("\\.svg$", all_files))
    n_png <- sum(grepl("\\.png$", all_files))
    n_html <- sum(grepl("\\.html$", all_files))
    n_txt <- sum(grepl("\\.txt$", all_files))
    n_tex <- sum(grepl("\\.tex$", all_files))

    cli_ul(c(
      paste0("CSV files: ", n_csv),
      paste0("RDS files: ", n_rds),
      paste0("SVG plots: ", n_svg),
      paste0("PNG plots: ", n_png),
      paste0("HTML tables: ", n_html),
      paste0("Text summaries: ", n_txt),
      paste0("LaTeX files: ", n_tex),
      paste0("Total files: ", length(all_files))
    ))

    cli_text("")
    cli_text("Output directory: {.path {output_dir}}")
  }
}
