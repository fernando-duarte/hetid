# Scripts Directory Structure

_Last modified: 2026-06-15 15:50 EDT_

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
  from fixed gamma and tau (tau=0 point ID; tau=`BASELINE_TAU` set ID);
  stamps the resolved baseline `spec` (Y1 lags, news-projection mode, Z
  source) into the saved RDS via `current_baseline_spec()`
- `analyze_identification.R` - Compare tau=0 vs set-ID bounds, width
  summaries, and eigenvalue/quadratic diagnostics, with plots
- `output_results.R` - Export baseline identification results (HTML,
  LaTeX, CSV)
- `compute_identification_ixj.R` - Compute the I×J (separate-instrument)
  identified set: one quadratic constraint per (component, instrument)
  pair across a tau grid (runs after `output_results.R` in the pipeline)

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
- `tau_star_comparison.R` - Compute identification strength via tau*
  (the slack where the set transitions bounded→unbounded) across
  gamma choices using coarse grid, bisection, and fine grid. Tagged
  `full_only` in the stage list: it runs only on the full run
  (`HETID_FULL_RUN=1`), not the default quick run
- `tau_star_report*.R` - Generate the tau* report (also `full_only`):
  `tau_star_report.R` orchestrates tables/sweeps/summary;
  `_report_utils.R`, `_report_figures.R`, and `_report_text.R`
  supply formatting helpers, overlay/blow-up figures, and prose
- `spec_comparison.R` - Resumable, parallel specification comparison
  across (n_pcs, components, gamma, tau) cells, with the gamma scheme
  drawn from `vfci` / `optimized` / `separate` (I×J); honors
  `HETID_SPEC_QUICK` to select the quick subgrid;
  `spec_comparison_design.R` is the single source of truth for the
  full/quick grid profiles (`spec_comparison_design()` selector,
  `spec_comparison_design_cells()` enumerator)
- `spec_comparison_report*.R` - Generate the spec-comparison report
  from the grid: `_report.R` orchestrates; `_report_utils.R`,
  `_report_stats.R`, `_report_artifacts.R`, `_report_figures.R`, and
  `_report_text.R` supply labels/classification, aggregation,
  table/CSV/LaTeX artifacts, plots, and narrative prose

### 06_results_production/
Publication-ready outputs assembled from stages 03-05
- `assemble_results.R` - Merge baseline, optimized, and
  variance-bounds artifacts into one final comparison object
- `create_tables_and_figures.R` - Build publication tables and
  figures (main table, interval plot, width-reduction chart, gamma
  heatmap); `create_figures_section.R` is its in-place figure helper
- `create_theta_panel_table.R` - Build the theta identified-set and
  optimized-loadings panel table (LaTeX fragment + standalone)
- `create_consumption_equation_table.R` - Build the consumption-growth
  equation structural-coefficient table: recovers `beta1(theta)` via
  the exported `recover_structural_coefficients()`
  (`beta1(theta) = beta1R - (beta2R)'theta`) and tabulates a point-ID
  column (theta from the reduced-form gamma at tau=0) beside a set-ID
  column (optimized gamma at tau=`BASELINE_TAU`), where each beta1
  coefficient is an exact interval over Theta from
  `solve_linear_functional_bound`; LaTeX fragment + standalone
- `output_results.R` - Write final human-readable summaries and
  stable machine-readable exports (RDS, CSV, text)

### 07_generalized_instruments/
Generalized-instrument identified set on Z = PC^2 (squared
principal components). A demonstration of the exported generalized API
on a non-default instrument set; not a paper deliverable. Relies on
`HETID_Z_SOURCE` being unset so the structural first stage keeps the
default level-PC residuals while only the instrument role is squared.
- `compute_generalized_identification.R` - Build Z = PC^2 via the
  exported `build_instrument_matrix(..., transforms = list(sq = ...),
  include_original = FALSE)`, assemble identity-weight (separate-
  instrument) quadratic systems with `build_general_quadratic_system`
  at tau=`BASELINE_TAU` and tau=0, solve the identified set, and run a
  closure membership probe over a theta grid via `probe_set_membership`
  (which exercises the exported `make_system_checker` /
  `make_constraint_checker` closures)
- `output_results.R` - Export the labeled identification table and
  membership summary

### utils/
Shared utility functions. `common_settings.R` sources the core layer
listed first below (an explicit `file.exists`-guarded block, not a
loop); the remaining `utils/` helpers (`hetero_lm_tests.R`,
`hetero_panel_meta.R`, `hetero_diag_figures.R`, `ixj_identification.R`,
`latex_simple_table.R`, `spec_comparison_eval.R`) are sourced directly
by the stage scripts that consume them. (The spec-comparison grid
design, `spec_comparison_design.R`, lives with stage 05, not in
`utils/`.)
- `common_settings.R` - Central configuration: shared paths, output
  directories, and constants (`NEWS_STEP = 3`, `PIPELINE_ACM_MATURITIES`
  = 3..120 by 3, `BASELINE_TAU = 0.05`, `SEED = 123`, `N_CORES`,
  `N_Y1_LAGS = 4`, `IMPOSE_NEWS_PROJECTION_ZERO = FALSE`,
  `TAU_STAR_N_STARTS = 15`, plot/table settings, `DATA_RDS_PATH`);
  sources the core utility files
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
  lookups, input loading, W1/W2 reduced-form residuals (retaining the
  Y2-on-PC `beta2R` for structural recovery), the VFCI baseline gamma
  and tau spec, and `build_pipeline_quadratic_system()` -- the single
  front door that routes every quadratic-system assembly through the
  exported generalized-instrument builder
- `ixj_identification.R` - I×J separate-instrument set: one quadratic
  per (component, instrument) pair, intersected
- `z_source.R` - Z-source hook resolver (selects the instrument
  matrix; default or custom `build_z(data)`)
- `gamma_source.R` - Baseline-gamma hook resolver (VFCI or custom);
  also `build_reduced_form_gamma(beta2r)` (maturities Y2-on-PC slopes)
- `news_projection.R` - Resolves whether the W2-residual construction
  imposes the exact-news projection `B = 0` or estimates `B` from the
  data: `impose_news_projection_zero()` reads the
  `HETID_IMPOSE_NEWS_PROJECTION_ZERO` env override, falling back to the
  `IMPOSE_NEWS_PROJECTION_ZERO` script constant (default `FALSE`)
- `baseline_spec.R` - Stage-04 baseline spec stamp and the fail-closed
  downstream consistency check (`current_baseline_spec()`,
  `baseline_spec_mismatches()`, `assert_baseline_spec_current()`): stages
  that load the baseline RDS abort if its stamped Y1-lags /
  news-projection mode / Z source differ from the current run
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
- `closure_membership.R` - Identified-set membership probes built on the
  package's `make_system_checker` / `make_constraint_checker` closures:
  `make_theta_grid()` and `probe_set_membership()` (complements the
  profile-bound interval widths)
- `tau_star_utils.R` - tau* machinery: fixed-gamma sweep, bisection,
  re-optimizing oracle, curvature degeneracy diagnostics
- `spec_comparison_eval.R` - Spec-comparison evaluators (per-cell
  moments, fixed/optimized/I×J widths, per-group row builder)
- `latex_table_utils.R` - Booktabs/threeparttable/siunitx panel tables
  with standalone compilable variants
- `latex_simple_table.R` - Plain-column (l c c ...) booktabs/
  threeparttable table for cells that are not pure numbers (e.g.
  identified-set interval strings); reuses the standalone/writer
  helpers from `latex_table_utils.R`
- `tests/` - Unit tests for the utility layer (lambda/whitening/
  optimization, profile bounds, I×J, closure membership, hetero tests,
  gamma sources, news-projection mode and Stage-04 baseline spec stamp,
  residual alignment, Z-width pipeline, generalized-vs-legacy pipeline
  equivalence, stats) plus `fixtures/` capture scripts and RDS
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
  final, consumption-equation, and generalized-instrument
  identification tables and figures
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
  spec-comparison grids (and per-group checkpoint dirs), tau* sweeps
- `identification_results/` - Stage 06 final comparison
- `identification_generalized/` - Stage 07 generalized-instrument
  (Z = PC^2) results
- `variance_bounds/` - Stage 03 working results
- `identification_diagnostics/` - Stage 02 working results
- `summary_stats/`, `time_series_properties/`, `plots/`, `figures/`,
  `tables/`, `other/` - Stage-01 and assorted working artifacts

## Top-Level Scripts

- `run_all_scripts.R` - Runs the complete analysis pipeline. Sources
  `run_all_stage_list.R` for the ordered script list, then runs each
  stage via the `run_script(path, desc, env)` helper. For a stage with
  an `env` vector, `run_script()` saves each variable's prior value,
  sets the stage's env, sources the script, and restores the prior
  state on exit (unsetting variables that were previously unset) -- so
  a per-stage switch such as `HETID_SPEC_QUICK` neither leaks into
  later stages nor wipes a value the caller exported before launching.
  Stages are wrapped in `tryCatch`, timed, and an execution summary of
  output file counts by type is printed at the end.
- `run_all_stage_list.R` - Defines `scripts_to_run`: the ordered
  `list(path, desc, optional env, optional full_only)` stages that
  constitute the pipeline; edit this to add, remove, or reorder stages.
  It also sets the run profile: `quick_run <-
  !nzchar(Sys.getenv("HETID_FULL_RUN"))` (the default is the quick
  run), and at the bottom it `Filter`s out every stage tagged
  `full_only = TRUE` when `quick_run` is in effect.
- `quality-check.R` - Package quality suite (pkgcheck, rcmdcheck,
  codetools, cyclocomp, dupree, CodeDepends, lintr, checkglobals,
  spelling, urlchecker, covr); writes reports to `docs/quality-reports/`

## Run Profiles and Environment Switches

The default `Rscript run_all_scripts.R` is the **quick run**. It runs
every stage except the two `full_only` tau* stages, and pins the
spec-comparison stage to its quick subgrid. The verified switches:

- `HETID_FULL_RUN` (read in `run_all_stage_list.R:12`) - any non-empty
  value selects the **full run**, which additionally keeps the
  `full_only = TRUE` stages `tau_star_comparison.R` and
  `tau_star_report.R` (the most expensive part of the pipeline: a
  multi-start optimizer oracle bisected over a tau grid). Unset (the
  default) drops them.
- `HETID_SPEC_QUICK` (read in `spec_comparison.R:30`) - any non-empty
  value selects the quick spec-comparison subgrid via
  `spec_comparison_design("quick")` and suffixes its artifacts
  `_quick`; otherwise the full grid (`_full`) is computed. The stage
  list pins it on for the in-pipeline `spec_comparison.R` stage
  (`env = c(HETID_SPEC_QUICK = "1")`).
- `HETID_SPEC_SOURCE` (read in `spec_comparison_report.R:35`) - path to
  the saved grid (`.rds`/`.csv`) the spec-comparison report should read
  instead of recomputing. The stage list points it at
  `spec_comparison_quick.rds` so the report refreshes the `_quick`
  artifacts; unset, the report prefers `spec_comparison_full.*` when a
  full grid exists, then falls back to the quick grid.
- `HETID_BASELINE_GAMMA` (read in `compute_identification.R:9` and
  `gamma_source.R:13`, default `"vfci"`) - selects the baseline gamma:
  `"vfci"` (the fixed unit-norm VFCI PC loading, which requires exactly
  4 instruments) or a path to an R file defining
  `build_gamma(moments)` returning a J×I matrix (the arbitrary-width
  escape hatch).
- `HETID_Z_SOURCE` (read in `z_source.R:25`/`:65`) - path to an R file
  defining `build_z(data)` returning a named numeric T×K instrument
  matrix; unset, the pipeline uses the default level-PC instruments.
  Both branches funnel through the exported `build_instrument_matrix`.
- `HETID_IMPOSE_NEWS_PROJECTION_ZERO` (read in `news_projection.R:9` via
  `impose_news_projection_zero()`) - `TRUE`/`1` imposes the exact-news
  projection `B = 0` (residual Y2 = the raw news); `FALSE`/`0` estimates
  `B` from the data (the default, "let the data speak"). Unset, it falls
  back to the `IMPOSE_NEWS_PROJECTION_ZERO` script constant in
  `common_settings.R` (default `FALSE`). The resolved value is part of
  the Stage-04 baseline spec stamp.
- `HETID_ASSERT_EQUIV` (read in `identification_utils.R:274`) -
  diagnostic shadow flag: when non-empty, every
  `build_pipeline_quadratic_system()` call additionally asserts
  numeric-leaf identity with the legacy `build_quadratic_system()`.
  Off by default (zero overhead).

Examples:

```bash
# From the scripts directory

# Quick run (default): skips the tau* stages, quick spec grid
Rscript run_all_scripts.R

# Full run: adds the tau* identification-strength stages
HETID_FULL_RUN=1 Rscript run_all_scripts.R
```

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
- **Generalized Instruments**: Run
  `07_generalized_instruments/` for the generalized-instrument
  (Z = PC^2) demonstration

## Running the Scripts

### Run All Scripts

To run the pipeline end to end (the default is the quick run, which
skips the `full_only` tau* stages -- see "Run Profiles and Environment
Switches" above):

```bash
# From the scripts directory

# Quick run (default)
Rscript run_all_scripts.R

# Full run (adds the tau* identification-strength stages)
HETID_FULL_RUN=1 Rscript run_all_scripts.R
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
- Each downstream stage folder (`02`-`07`) contains its own
  `output_results.R` to ensure modularity; stage `01` writes its
  artifacts directly from its analysis scripts
- Scripts are numbered to indicate the recommended execution
  order
- The main runner script (`run_all_scripts.R`) handles
  dependencies and runs scripts in the correct order via the
  list in `run_all_stage_list.R`
