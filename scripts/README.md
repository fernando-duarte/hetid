# Scripts Directory Structure

This directory contains all analysis scripts for the hetid
package, organized by workflow stage. All outputs are stored
within the scripts directory for self-contained
reproducibility.

## Directory Structure

### 01_data_analysis/
Initial data preparation and exploratory analysis
- `create_data.R` - Load ACM data, convert to quarterly,
  merge with variables.RData, consolidate into single dataset
  with dates
- `summary_statistics.R` - Generate descriptive statistics
  for all variables
- `time_series_properties.R` - Analyze autocorrelation,
  stationarity, and other time series characteristics; add
  heteroskedasticity tests from package skedastic
- `visualize_raw_data.R` - Create exploratory plots of raw
  data

### 02_sdf_news/
Computation and analysis of pricing kernel innovations
- `compute_news.R` - Calculate price news and SDF innovations
- `analyze_news.R` - Statistical analysis of computed
  news/innovations
- `visualize_news.R` - Create exploratory plots of bond price
  news and SDF news

### 03_variance_bounds/
Variance bound calculations for identification
- `compute_variance_bounds.R` - Calculate theoretical variance
  bounds
- `analyze_bounds.R` - Analyze computed bounds and their
  implications
- `output_results.R` - Export variance bound results

### 04_identification_without_optimization/
Identification analysis using fixed PC weights.
This directory is currently empty (placeholder for future
scripts).

### 05_identification_with_optimization/
Optimal identification through PC weight optimization.
This directory is currently empty (placeholder for future
scripts).

### 06_results_production/
Generate publication-ready outputs.
This directory is currently empty (placeholder for future
scripts).

### utils/
Shared utility functions
- `common_settings.R` - Shared configuration, directories,
  and parameters; sources all utility functions
- `plotting_utils.R` - Common plotting utilities (save plots,
  apply themes, correlation heatmaps, time series plots)
- `stats_utils.R` - Statistical analysis and data validation
  (summary stats, stationarity tests, formatted tables)
- `hetero_test_utils.R` - Heteroskedasticity testing and
  diagnostics
- `README.md` - Documentation for the utility functions

### output/
All script outputs organized by purpose

#### for_paper/
Publication-ready outputs
- `tables/` - Formatted tables for manuscript
- `figures/` - Publication-quality figures
- `other/` - Supplementary materials
- `variance_bounds/` - Variance bound results

#### temp/
Working outputs and intermediate results
- `consolidated_data.rds` - Consolidated analysis dataset
- `data.rds` - Base data
- `tables/` - Draft tables
- `figures/` - Exploratory plots
- `other/` - Temporary files
- `plots/` - Working plots
- `sdf_news/` - SDF news intermediate results
- `summary_stats/` - Summary statistics output
- `time_series_properties/` - Time series analysis output
- `variance_bounds/` - Variance bound working results

## Top-Level Scripts

- `run_all_scripts.R` - Runs the complete analysis pipeline
  in the correct order
- `quality-check.R` - Package quality checks (spelling,
  linting, test coverage, R CMD check)

## Workflow

- **Data Preparation**: Start with scripts in
  `01_data_analysis/` to prepare and explore the data
- **Core Computations**: Run `02_sdf_news/` and
  `03_variance_bounds/` for fundamental calculations
- **Identification Analysis**: Execute
  `04_identification_without_optimization/` for baseline
  results (not yet implemented)
- **Optimization**: Run
  `05_identification_with_optimization/` for optimal
  identification (not yet implemented)
- **Results Production**: Use `06_results_production/` to
  generate final outputs (not yet implemented)

## Running the Scripts

### Run All Scripts

To run the complete analysis pipeline:

```bash
# From the scripts directory
Rscript run_all_scripts.R
```

### Run Individual Scripts

Each script can also be run individually:

```bash
# Example: Run only summary statistics
Rscript 01_data_analysis/summary_statistics.R
```

## Notes

- All scripts should source necessary functions from `utils/`
- Outputs are organized by whether they are publication-ready
  (`output/for_paper/`) or temporary (`output/temp/`)
- Each analysis folder contains its own `output_results.R` to
  ensure modularity
- Scripts are numbered to indicate the recommended execution
  order
- The main runner script (`run_all_scripts.R`) handles
  dependencies and runs scripts in the correct order
