# Scripts Directory Structure

This directory contains all analysis scripts for the hetid package, organized by workflow stage. All outputs are stored within the scripts directory for self-contained reproducibility.

## Directory Structure

### 01_data_analysis/
Initial data preparation and exploratory analysis
- `create_data.R` - Load ACM data, convert to quarterly, merge with variables.RData, consolidate into single dataset with dates
- `summary_statistics.R` - Generate descriptive statistics for all variables
- `time_series_properties.R` - Analyze autocorrelation, stationarity, and other time series characteristics; add heteroskedasticity tests from package skedastic
- `visualize_raw_data.R` - Create exploratory plots of raw data

### 02_sdf_news/
Computation and analysis of pricing kernel innovations
- `compute_news.R` - Calculate price news and SDF innovations
- `analyze_news.R` - Statistical analysis of computed news/innovations
- `visualize_news.R` - Create exploratory plots of bond price news and sdf news
- `output_results.R` - Export results for downstream analysis

### 03_variance_bounds/
Variance bound calculations for identification
- `compute_variance_bounds.R` - Calculate theoretical variance bounds
- `analyze_bounds.R` - Analyze computed bounds and their implications
- `output_results.R` - Export variance bound results

### 04_identification_without_optimization/
Identification analysis using fixed PC weights
- `compute_reduced_form_residuals.R` - Calculate W1 and W2 residuals
- `analyze_reduced_form_residuals.R` - Statistical properties of residuals
- `identification_with_PC.R` - Special case with linear combinations of PCs given by unit vectors (c_i = e_i)
- `output_results.R` - Export identification results

### 05_identification_with_optimization/
Optimal identification through PC weight optimization
- `create_optimization_variables.R` - Prepare variables for optimization
- `setup_optimization.R` - Configure optimization problem, analyze initial conditions
- `run_optimization.R` - Execute optimization procedures
- `validate_results.R` - Check optimizer flags and constraint satisfaction
- `output_results.R` - Export optimized identification results

### 06_results_production/
Generate publication-ready outputs
- `create_tables.R` - Produce formatted tables for paper
- `create_figures.R` - Create publication-quality figures
- `create_other.R` - Generate additional outputs (e.g., supplementary materials)

### utils/
Shared utility functions
- `plotting_functions.R` - Common plotting utilities
- `table_formatting.R` - Table formatting helpers
- `latex_helpers.R` - LaTeX output utilities
- `common_settings.R` - Shared configuration and parameters

### output/
All script outputs organized by purpose

#### for_paper/
Publication-ready outputs
- `tables/` - Formatted tables for manuscript
- `figures/` - Publication-quality figures
- `other/` - Supplementary materials

#### temp/
Working outputs and intermediate results
- `tables/` - Draft tables
- `figures/` - Exploratory plots
- `other/` - Temporary files

## Workflow

1. **Data Preparation**: Start with scripts in `01_data_analysis/` to prepare and explore the data
2. **Core Computations**: Run `02_sdf_news/` and `03_variance_bounds/` for fundamental calculations
3. **Identification Analysis**: Execute `04_identification_without_optimization/` for baseline results
4. **Optimization**: Run `05_identification_with_optimization/` for optimal identification
5. **Results Production**: Use `06_results_production/` to generate final outputs

## Running the Scripts

### Run All Scripts

To run the complete analysis pipeline:

```bash
# From the scripts directory
Rscript run_all_scripts.R

# Or use the shell script
./run_all_scripts.sh
```

### Run Individual Scripts

Each script can also be run individually:

```bash
# Example: Run only summary statistics
Rscript 01_data_analysis/summary_statistics.R
```

## Notes

- All scripts should source necessary functions from `utils/`
- Outputs are organized by whether they are publication-ready (`output/for_paper/`) or temporary (`output/temp/`)
- Each analysis folder contains its own `output_results.R` to ensure modularity
- Scripts are numbered to indicate the recommended execution order
- The main runner script (`run_all_scripts.R`) handles dependencies and runs scripts in the correct order
