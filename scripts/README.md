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
- `create_data.R` - Load ACM data, convert to quarterly, build
  lagged/normalized PCs, merge with `variables.RData`, and write the
  consolidated dataset to `output/temp/data.rds`
- `summary_statistics.R` - Descriptive statistics (mean, SD,
  quantiles, skewness, kurtosis, autocorrelation) and correlation
  matrices for yields, term premia, PCs, and consumption growth
- `time_series_properties.R` - Unit-root (ADF, KPSS, PP), serial
  correlation (Ljung-Box, ARCH), normality (Jarque-Bera), and
  heteroskedasticity tests across variables, with diagnostic plots
  and HTML summaries
- `visualize_raw_data.R` - Exploratory SVG plots of raw data (yield
  series, term-structure snapshots, term premia, PCs, distributions,
  correlation heatmaps, rolling statistics)

### 02_identification_diagnostics/
Diagnostics of the identification-relevant objects (W2
residuals and n-hat)
- `heteroskedasticity_tests.R` - Test the Lewbel assumption on the
  W2 residuals: regime-aware skedastic suite plus Glejser, BP LM,
  ARCH(1), and squared-residual/instrument correlations; emits
  p-value and correlation heatmaps
- `n_hat_episodes.R` - Detect positive n-hat episodes (monthly ACM
  data), map them to crisis/QE event windows, and validate the
  implied one-year-ahead short-rate prediction; quarterly cross-check
- `output_results.R` - Export the diagnostics panel (LaTeX fragment +
  standalone), HTML mirrors, CSVs, and copied figures

### 03_variance_bounds/
Variance bound calculations for identification
- `compute_variance_bounds.R` - Compute the leading-term variance
  bounds across maturities from plug-in `c_hat`, `k_hat`, and
  `k2_hat` estimators; assess monotonicity and component correlations
- `analyze_bounds.R` - Analyze computed bounds (by-maturity,
  component, and log-scale plots; growth rates; Spearman trend tests;
  component dominance)
- `output_results.R` - Export variance-bound tables (HTML, optional
  PNG, LaTeX), CSVs, summaries, and copied figures

### 04_identification_without_optimization/
Baseline identified set using fixed PC weights (VFCI gamma)
and fixed tau values
- `compute_identification.R` - Compute the baseline identified set
  from fixed gamma and tau (tau=0 point ID; tau=`BASELINE_TAU` set ID)
- `compute_identification_ixj.R` - Compute the I×J (separate-instrument)
  identified set: one quadratic constraint per (component, instrument)
  pair across a tau grid
- `analyze_identification.R` - Compare tau=0 vs set-ID bounds, width
  summaries, and eigenvalue/quadratic diagnostics, with plots
- `output_results.R` - Export baseline identification results (HTML,
  LaTeX, CSV)

### 05_identification_with_optimization/
Optimal identification by optimizing instrument weights to
minimize total identified-set width (tau held fixed)
- `optimize_identification.R` - Optimize PC loadings (gamma) via
  multi-start SLSQP under variance normalization to minimize total
  set width
- `analyze_optimization.R` - Compare baseline vs optimized widths,
  objective values, and gamma-similarity metrics, with plots
- `output_results.R` - Export optimized identification results
  (HTML, LaTeX, CSV); `output_results_summary_section.R` is its
  in-place summary helper
- `spec_comparison.R` - Resumable, parallel specification comparison
  across (mode, n_pcs, components, gamma, tau) cells;
  `spec_comparison_design.R` defines the full/quick grid profiles
- `spec_comparison_report*.R` - Generate the spec-comparison report
  from the grid: `_report.R` orchestrates; `_report_utils.R`,
  `_report_stats.R`, `_report_artifacts.R`, `_report_figures.R`, and
  `_report_text.R` supply labels/classification, aggregation,
  table/CSV/LaTeX artifacts, plots, and narrative prose
- `tau_star_comparison.R` - Compute identification strength via tau*
  (the slack where the set transitions bounded→unbounded) across
  gamma choices using coarse grid, bisection, and fine grid
- `tau_star_report*.R` - Generate the tau* report:
  `tau_star_report.R` orchestrates tables/sweeps/summary;
  `_report_utils.R`, `_report_figures.R`, and `_report_text.R`
  supply formatting helpers, overlay/blow-up figures, and prose

### 06_results_production/
Publication-ready outputs assembled from stages 03-05
- `assemble_results.R` - Merge baseline, optimized, and
  variance-bounds artifacts into one final comparison object
- `create_tables_and_figures.R` - Build publication tables and
  figures (main table, interval plot, width-reduction chart, gamma
  heatmap); `create_figures_section.R` is its in-place figure helper
- `create_theta_panel_table.R` - Build the theta identified-set and
  optimized-loadings panel table (LaTeX fragment + standalone)
- `output_results.R` - Write final human-readable summaries and
  stable machine-readable exports (RDS, CSV, text)

### 07_generalized_instruments/
Generalized-instrument identified set on Z = PC^2 (squared
principal components)
- `compute_generalized_identification.R` - Build the squared-PC
  instrument matrix and quadratic system via the exported
  `build_instrument_matrix` / `build_general_quadratic_system` API,
  compute the identified set, and run a constraint-checker closure
  membership probe over a theta grid using `make_system_checker` /
  `make_constraint_checker`
- `output_results.R` - Export the labeled identification table and
  membership summary

### utils/
Shared utility functions, sourced by `common_settings.R`
- `common_settings.R` - Central configuration: shared paths, output
  directories, and constants (`NEWS_STEP = 3`, `PIPELINE_ACM_MATURITIES`
  = 3..120 by 3, `BASELINE_TAU = 0.2`, `SEED`, `N_CORES`, plot/table
  settings, `DATA_RDS_PATH`); sources every utility file below
- `stats_utils.R` - Summary statistics (mean/sd/quantiles/skewness/
  kurtosis, optional autocorrelation)
- `format_utils.R` - Finite/Inf-aware formatters for bounds and
  widths (distinguishes unreliable from unbounded)
- `plotting_utils.R` - Plot styling and SVG/PNG saving with
  consistent ggplot2 themes and DPI
- `hetero_test_utils.R` - Heteroskedasticity test suite (White, BP,
  Goldfeld-Quandt, Harvey, Anscombe, Cook-Weisberg)
- `hetero_lm_tests.R` - LM-style hetero tests (BP, ARCH(1)) and
  regime-aware suite metadata
- `hetero_panel_meta.R` - Regime-aware panel row labels and LaTeX
  notes for stage-02 exports
- `hetero_plot_utils.R` - Heteroskedasticity diagnostic plots
  (residuals vs fitted, QQ, time-series)
- `hetero_diag_figures.R` - Stage-02 figure builders (-log10 p-value
  profiles across maturities)
- `identification_utils.R` - Identification plumbing: maturity
  lookups, input loading, and W1/W2 reduced-form residuals
- `ixj_identification.R` - I×J separate-instrument set: one quadratic
  per (component, instrument) pair, intersected
- `z_source.R` - Z-source hook resolver (selects the instrument
  matrix; default or custom `build_z(data)`)
- `gamma_source.R` - Baseline-gamma hook resolver (VFCI or custom);
  also `build_reduced_form_gamma()` (maturities Y2-on-PC slopes)
- `optimization_utils.R` - Total-width objective, Euclidean display
  normalizer, and inner steering penalty
- `lambda_mask.R` - Weight-optimizer helpers: packing, legacy
  matrix-start coercion, per-component support masks, coordinate codecs
- `lambda_whitening.R` - Whitening reparameterization
  (`lambda' Var(Z) lambda = 1` as search geometry) with validation
- `lambda_varnorm.R` - Variance normalization of optimized weights
  (zero check + identification diagnostic)
- `lambda_optimization.R` - Outer optimizer over per-component weight
  matrices (variance-normalization default, multistart)
- `profile_bounds_core.R` - Non-dimensionalized profile-bounds solver
  with scale-aware unbounded detection
- `profile_bounds.R` - Profile-bounds API (coordinate-tracking solver;
  closed-form tau=0 point ID)
- `tau_star_utils.R` - tau* machinery: fixed-gamma sweep, bisection,
  re-optimizing oracle, curvature degeneracy diagnostics
- `spec_comparison_eval.R` - Spec-comparison evaluators (per-cell
  moments, fixed/optimized/I×J widths, per-group row builder)
- `latex_table_utils.R` - Booktabs/threeparttable/siunitx panel tables
  with standalone compilable variants
- `tests/` - Unit tests for the utility layer (lambda/whitening/
  optimization, profile bounds, I×J, hetero tests, gamma sources,
  stats) plus `fixtures/` capture scripts
- `README.md` - Documentation for the utility functions

### examples/
Standalone demonstrations (not part of the pipeline)
- `custom_z_demo.R` - End-to-end generalized-instrument workflow on a
  custom Z (nonlinear PC transform + volatility); runs offline

### z_sources/
Drop-in payloads for the `z_source.R` hook
- `pc_squared.R` - Squared principal components as instruments
  (Regime B, outside span(1, PC); enables Anscombe in the diagnostics)

### output/
All script outputs organized by purpose

#### for_paper/
Publication-ready outputs
- `identification/` - Baseline, optimized, I×J, spec-comparison, tau*,
  and final identification tables and figures
- `variance_bounds/` - Variance bound tables, CSVs, and figures
- `identification_diagnostics/` - Diagnostics tables and figures
- `tables/`, `figures/`, `other/` - Curated manuscript materials

#### temp/
Working outputs and intermediate results
- `data.rds` - Consolidated analysis dataset (quarterly ACM data
  merged with `variables.RData`), created by stage 01 and read by all
  later stages
- `identification_baseline/` - Stage 04 baseline results
- `identification_ixj/` - Stage 04 I×J results
- `identification_optimized/` - Stage 05 optimized weights,
  spec-comparison grids, tau* sweeps
- `identification_results/` - Stage 06 final comparison
- `variance_bounds/` - Stage 03 working results
- `identification_diagnostics/` - Stage 02 working results
- `summary_stats/`, `time_series_properties/`, `plots/`, `figures/`,
  `tables/`, `other/` - Stage-01 and assorted working artifacts

## Top-Level Scripts

- `run_all_scripts.R` - Runs the complete analysis pipeline. Sources
  `run_all_stage_list.R` for the ordered script list, runs each via a
  `run_script()` helper (per-script env vars, error handling, timing),
  and prints an execution summary of output file counts by type
- `run_all_stage_list.R` - Defines `scripts_to_run`: the ordered
  (path, description, optional env) triples that constitute the
  pipeline; edit this to add, remove, or reorder stages
- `quality-check.R` - Package quality suite (pkgcheck, rcmdcheck,
  codetools, cyclocomp, dupree, CodeDepends, lintr, checkglobals,
  spelling, urlchecker, covr); writes reports to `docs/quality-reports/`

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
  dependencies and runs scripts in the correct order via the
  list in `run_all_stage_list.R`
</content>
