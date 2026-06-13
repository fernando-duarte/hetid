# Stage list for run_all_scripts.R: the ordered scripts_to_run definition
# (path, description, optional per-script env vector). Moved verbatim from
# run_all_scripts.R, its only consumer. Editing the pipeline order means
# editing this list.

# Pipeline run profile. The default pipeline is the QUICK run (spec_comparison
# is pinned to its quick subgrid below). The tau* identification-strength stages
# (tau_star_comparison.R + its report tau_star_report.R) are the most expensive
# part of the pipeline -- a multi-start optimizer oracle bisected over a tau grid
# for BOTH modes -- and are EXCLUDED from the quick run. Set HETID_FULL_RUN=1 to
# include them. Stages tagged `full_only = TRUE` are dropped when quick_run.
quick_run <- !nzchar(Sys.getenv("HETID_FULL_RUN"))

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
    desc = "Comparing Identification Strength (tau*)",
    full_only = TRUE
  ),
  list(
    path = here::here(
      "scripts/05_identification_with_optimization",
      "tau_star_report.R"
    ),
    desc = "Reporting Identification Strength (tau*)",
    full_only = TRUE
  ),
  list(
    path = here::here(
      "scripts/05_identification_with_optimization",
      "spec_comparison.R"
    ),
    desc = "Comparing Specifications, Instruments, and tau (quick)",
    env = c(HETID_SPEC_QUICK = "1")
  ),
  # Point the report at the quick grid the previous stage just computed; the
  # report's default otherwise prefers spec_comparison_full.rds when a full-run
  # grid exists, which would skip refreshing the _quick artifacts.
  list(
    path = here::here(
      "scripts/05_identification_with_optimization",
      "spec_comparison_report.R"
    ),
    desc = "Reporting Specification Comparison Results (quick grid)",
    env = c(HETID_SPEC_SOURCE = here::here(
      "scripts/output/temp/identification_optimized",
      "spec_comparison_quick.rds"
    ))
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
    path = here::here("scripts/06_results_production/create_consumption_equation_table.R"),
    desc = "Creating Consumption-Equation Structural-Coefficient Table"
  ),
  list(
    path = here::here(
      "scripts/06_results_production",
      "output_results.R"
    ),
    desc = "Exporting Final Results"
  ),

  # Generalized-instrument identification on Z = PC^2 (exported generalized API
  # + constraint-checker closure membership probe)
  list(
    path = here::here(
      "scripts/07_generalized_instruments",
      "compute_generalized_identification.R"
    ),
    desc = "Computing Generalized-Instrument Identified Set (Z = PC^2)"
  ),
  list(
    path = here::here(
      "scripts/07_generalized_instruments",
      "output_results.R"
    ),
    desc = "Exporting Generalized-Instrument Results"
  )
)

# Drop full-only stages on a quick run (the default). A re-run with
# HETID_FULL_RUN=1 keeps them.
if (quick_run) {
  scripts_to_run <- Filter(
    function(stage) !isTRUE(stage$full_only),
    scripts_to_run
  )
}
