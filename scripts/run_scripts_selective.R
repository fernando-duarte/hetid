#!/usr/bin/env Rscript

# Selective Script Runner for hetid Analysis
# This script allows running specific groups of scripts

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Load required packages
library(here)
library(cli)

# Helper function to display usage
show_usage <- function() {
  cli_h1("hetid Script Runner - Usage")
  cli_text("Run this script with one or more of the following options:")
  cli_ul(c(
    "{.code all} - Run all scripts (default)",
    "{.code data} - Run data preparation scripts only",
    "{.code analysis} - Run data analysis scripts only",
    "{.code news} - Run SDF news analysis scripts only",
    "{.code viz} - Run visualization scripts only"
  ))
  cli_text("")
  cli_text("Example: {.code Rscript run_scripts_selective.R data analysis}")
}

# If no arguments or help requested, show usage
if (length(args) == 0 || any(args %in% c("-h", "--help", "help"))) {
  show_usage()
  if (length(args) == 0) {
    cli_text("")
    cli_alert_info("No arguments provided. Running all scripts...")
    args <- "all"
  } else {
    quit(status = 0)
  }
}

# Start timing
start_time <- Sys.time()

cli_h1("Running Selected hetid Analysis Scripts")
cli_text("Started at: {.timestamp {start_time}}")
cli_text("Selected groups: {.val {paste(args, collapse = ', ')}}")
cli_rule()

# Helper function to run a script
run_script <- function(script_path, description) {
  script_name <- basename(script_path)
  cli_h2(description)
  cli_text("Running: {.file {script_name}}")

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

# Define script groups
script_groups <- list(
  data = list(
    list(
      path = here::here("scripts/00_data_prep/load_data.R"),
      desc = "Loading and Preparing Data"
    )
  ),
  analysis = list(
    list(
      path = here::here("scripts/01_data_analysis/summary_statistics.R"),
      desc = "Computing Summary Statistics"
    ),
    list(
      path = here::here("scripts/01_data_analysis/time_series_properties.R"),
      desc = "Analyzing Time Series Properties"
    )
  ),
  viz = list(
    list(
      path = here::here("scripts/01_data_analysis/visualize_raw_data.R"),
      desc = "Creating Data Visualizations"
    )
  ),
  news = list(
    list(
      path = here::here("scripts/02_sdf_news/compute_news.R"),
      desc = "Computing Price News"
    ),
    list(
      path = here::here("scripts/02_sdf_news/analyze_news.R"),
      desc = "Analyzing Price News and Heteroskedasticity"
    )
  )
)

# Determine which scripts to run
scripts_to_run <- list()

for (arg in args) {
  if (arg == "all") {
    # Add all scripts in order
    scripts_to_run <- c(
      script_groups$data,
      script_groups$analysis,
      script_groups$viz,
      script_groups$news
    )
    break
  } else if (arg %in% names(script_groups)) {
    scripts_to_run <- c(scripts_to_run, script_groups[[arg]])
  } else {
    cli_alert_warning("Unknown group: {.val {arg}} (skipping)")
  }
}

# Remove duplicates while preserving order
unique_scripts <- list()
seen_paths <- character()

for (script in scripts_to_run) {
  if (!script$path %in% seen_paths) {
    unique_scripts <- c(unique_scripts, list(script))
    seen_paths <- c(seen_paths, script$path)
  }
}

# Run selected scripts
if (length(unique_scripts) > 0) {
  cli_h1("Executing Scripts")

  for (script_info in unique_scripts) {
    run_script(script_info$path, script_info$desc)
  }

  # Summary
  end_time <- Sys.time()
  total_elapsed <- round(as.numeric(difftime(end_time, start_time, units = "mins")), 2)

  cli_h1("Execution Complete")
  cli_alert_success("Ran {.val {length(unique_scripts)}} scripts successfully!")
  cli_text("Total time: {.val {total_elapsed}} minutes")
  cli_text("Completed at: {.timestamp {end_time}}")
} else {
  cli_alert_warning("No scripts to run!")
}
