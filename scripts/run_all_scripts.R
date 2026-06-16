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
# duration of this script only, then restores each variable to its prior value
# (unsetting ones that were previously unset), so per-script switches like
# HETID_SPEC_QUICK neither leak into the rest of the pipeline nor wipe values
# the user exported before launching it.
run_script <- function(script_path, description, env = NULL) {
  script_name <- basename(script_path)
  cli_h2(description)
  cli_text("Running: {.file {script_name}}")

  if (length(env)) {
    prior <- Sys.getenv(names(env), unset = NA_character_)
    names(prior) <- names(env) # Sys.getenv drops names for a single variable
    do.call(Sys.setenv, as.list(env))
    on.exit(
      for (var in names(env)) {
        if (is.na(prior[[var]])) {
          Sys.unsetenv(var)
        } else {
          do.call(Sys.setenv, as.list(prior[var]))
        }
      },
      add = TRUE
    )
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

source(here::here("scripts/run_all_stage_list.R"))

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

# Fail-closed: scripts/output/for_paper must hold EXACTLY the three paper-spec
# tables (stage 08 builds them into a staging dir and atomically replaces
# for_paper). If anything else leaked in, a legacy writer was not redirected to
# temp -- abort so the regression is caught immediately.
cli_h2("Verifying the for_paper invariant")
if (!exists("assert_for_paper_allowlist")) {
  source(here::here("scripts/utils/common_settings.R"))
}
assert_for_paper_allowlist()
cli_alert_success("for_paper holds exactly the three paper-spec tables.")

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
