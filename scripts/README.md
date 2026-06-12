# Scripts Directory Structure

This directory contains all analysis scripts for the hetid
package, organized by workflow stage. All outputs are stored
within the scripts directory for self-contained
reproducibility.

## News Clock

The identification pipeline runs on a quarterly news clock
(`NEWS_STEP <- 3L` in `utils/common_settings.R`): one news period
equals one quarter, matching the quarterly rows of the stage-01
dataset, so the rows-equal-period contract of the SDF-news
construction holds exactly. Stage 01 therefore carries maturities at
3-month spacing (`PIPELINE_ACM_MATURITIES`, 3 to 120 months), giving
every horizon its step-adjacent neighbors; the data's 3-month floor
makes the quarterly clock feasible from the boundary horizon (i = 3)
upward, and the stage-03 variance bounds run on the same quarterly
clock (the realized leg of `compute_k_hat()` uses the 3-month yield
as its one-period rate). One diagnostic deliberately remains on the
annual clock: the stage-02 n-hat episode analysis, which validates an
inherently 1-year-ahead prediction.

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

### 02_identification_diagnostics/
Diagnostics of the identification-relevant objects (W2
residuals and n-hat)
- `heteroskedasticity_tests.R` - Test the Lewbel assumption on
  the W2 residuals: skedastic suite plus Glejser, BP LM on
  PCs, ARCH(1), and PC/squared-residual correlations
- `n_hat_episodes.R` - Detect positive n-hat episodes (monthly
  ACM data), map them to crisis/QE events, and validate the
  implied one-year-ahead rate prediction; quarterly cross-check
- `output_results.R` - Export the diagnostics panel table
  (LaTeX fragment + standalone), HTML mirrors, CSVs, and figures

### 03_variance_bounds/
Variance bound calculations for identification
- `compute_variance_bounds.R` - Calculate theoretical variance
  bounds
- `analyze_bounds.R` - Analyze computed bounds and their
  implications
- `output_results.R` - Export variance bound results

### 04_identification_without_optimization/
Baseline identified set using fixed PC weights (VFCI gamma)
and fixed tau values
- `compute_identification.R` - Compute the baseline identified
  set from fixed gamma and tau
- `analyze_identification.R` - Analyze the baseline identified
  set
- `output_results.R` - Export baseline identification results

### 05_identification_with_optimization/
Optimal identification by optimizing PC loadings (gamma) to
minimize total identified-set width (tau held fixed)
- `optimize_identification.R` - Optimize instrument weights via the
  whitened lambda optimizer (variance normalization
  `lambda' Var(Z) lambda = 1`) to minimize total set width
- `analyze_optimization.R` - Analyze optimization results
- `output_results.R` - Export optimization results

### 06_results_production/
Publication-ready outputs assembled from stages 04 and 05
- `assemble_results.R` - Merge baseline, optimized, and
  supporting artifacts into one comparison object
- `create_tables_and_figures.R` - Build publication tables and
  figures
- `create_theta_panel_table.R` - Build the theta identified-set
  and optimized-loadings panel table (LaTeX fragment +
  standalone)
- `output_results.R` - Export final results

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
- `factor_utils.R` - Yield curve PCA factors (level, slope,
  curvature) for identification
- `identification_utils.R` - Non-optimization identification
  plumbing (maturity lookups, set construction)
- `optimization_utils.R` - Width objective + Euclidean display
  normalizer (legacy gamma optimizer retired)
- `lambda_varnorm.R` - Variance normalization: zero check +
  identification diagnostic
- `latex_table_utils.R` - Booktabs/threeparttable/siunitx
  panel tables with standalone compilable variants
- `README.md` - Documentation for the utility functions

### output/
All script outputs organized by purpose

#### for_paper/
Publication-ready outputs
- `tables/` - Formatted tables for manuscript
- `figures/` - Publication-quality figures
- `other/` - Supplementary materials
- `variance_bounds/` - Variance bound results
- `identification_diagnostics/` - Identification diagnostics
  tables and figures

#### temp/
Working outputs and intermediate results
- `data.rds` - Consolidated analysis dataset (quarterly
  ACM data merged with variables.RData)
- `tables/` - Draft tables
- `figures/` - Exploratory plots
- `other/` - Temporary files
- `plots/` - Working plots
- `summary_stats/` - Summary statistics output
- `time_series_properties/` - Time series analysis output
- `variance_bounds/` - Variance bound working results
- `identification_diagnostics/` - Identification diagnostics
  working results

## Top-Level Scripts

- `run_all_scripts.R` - Runs the complete analysis pipeline
  in the correct order
- `quality-check.R` - Package quality checks (spelling,
  linting, test coverage, R CMD check)

## Workflow

- **Data Preparation**: Start with scripts in
  `01_data_analysis/` to prepare and explore the data
- **Identification Diagnostics**: Run
  `02_identification_diagnostics/` to test the identifying
  assumption and interpret n-hat
- **Core Computations**: Run `03_variance_bounds/` for
  fundamental calculations
- **Identification Analysis**: Execute
  `04_identification_without_optimization/` for baseline
  results
- **Optimization**: Run
  `05_identification_with_optimization/` for optimal
  identification
- **Results Production**: Use `06_results_production/` to
  generate final outputs

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

### Run the Identification Diagnostics (`02_identification_diagnostics/`)

These scripts read the consolidated dataset that stage 01
writes to `output/temp/data.rds`, so create it first by
running `01_data_analysis/create_data.R` (or the full
pipeline through stage 01). Then run the three scripts in
order: both compute scripts must finish before
`output_results.R`, which loads the `*_results.rds` files
they save.

```bash
# From the scripts directory

# Prerequisite: writes output/temp/data.rds
Rscript 01_data_analysis/create_data.R

# Stage 02, in order:
Rscript 02_identification_diagnostics/heteroskedasticity_tests.R
Rscript 02_identification_diagnostics/n_hat_episodes.R
Rscript 02_identification_diagnostics/output_results.R
```

Working artifacts (RDS objects, CSVs, and paired PNG/SVG
figures) are written to
`output/temp/identification_diagnostics/`; the publication
LaTeX panel (fragment + standalone), HTML mirrors, CSV
exports, and copied figures land in
`output/for_paper/identification_diagnostics/`.

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
