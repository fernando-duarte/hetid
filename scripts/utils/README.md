# Utility Functions for hetid Scripts

_Last modified: 2026-06-21 08:44 EDT_

This directory contains reusable utility functions that consolidate common patterns across the analysis scripts, following the DRY (Don't Repeat Yourself) principle. `common_settings.R` sources the core layer and then sources the heteroskedasticity LM helpers (`hetero_lm_tests.R`), the plain-column LaTeX renderer (`latex_simple_table.R`), the stage-08 paper-spec layer (`paper_spec_residuals.R`, `paper_spec_estimator.R`, `paper_spec_bootstrap.R`), and the `for_paper` guard (`for_paper_guard.R`) -- the paper spec needs all of these. The remaining `utils/` helpers (`hetero_panel_meta.R`, `hetero_diag_figures.R`, `ixj_identification.R`, `spec_comparison_eval.R`) are NOT sourced by `common_settings.R`; the stage scripts that consume them source them directly. (The spec-comparison grid design `spec_comparison_design.R` is a stage-05-local file, not a `utils/` helper.)

## Configuration

### common_settings.R
- Defines shared config constants (`NEWS_STEP = 3`, `PIPELINE_ACM_MATURITIES` = 3..120 by 3, `SEED = 123`, `BASELINE_TAU = 0.05`, `OPT_TAU_CAP = 0.99` (the near-uninformative slack cap shared across the tau* oracle and the set probes; admissible slack is tau in [0, 1)), `N_Y1_LAGS = 4`, `IMPOSE_NEWS_PROJECTION_ZERO = FALSE`, `TAU_STAR_N_STARTS = 15`, `N_CORES` (= `parallel::detectCores() - 1`), `MAX_N_PCS`, `FOR_PAPER_TABLE_STEMS`, plot/table settings, `DATA_RDS_PATH`) and output directories (`OUTPUT_DIR`, `OUTPUT_PAPER_DIR`, `OUTPUT_TEMP_DIR`)
- Sources the core utility files (via an explicit `file.exists`-guarded block, not a loop): `plotting_utils.R`, `stats_utils.R`, `hetero_test_utils.R`, `hetero_plot_utils.R`, `z_source.R`, `gamma_source.R`, `news_projection.R`, `baseline_spec.R`, `identification_utils.R`, `optimization_utils.R`, `lambda_mask.R`, `lambda_whitening.R`, `lambda_varnorm.R`, `lambda_optimization.R`, `profile_bounds_core.R`, `profile_bounds.R`, `format_utils.R`, `latex_table_utils.R`, `tau_star_utils.R`, `closure_membership.R`; then sources the paper-spec layer `hetero_lm_tests.R`, `latex_simple_table.R`, `paper_spec_residuals.R`, `paper_spec_estimator.R`, `paper_spec_bootstrap.R`, and the `for_paper` guard `for_paper_guard.R`; uses package constants from hetid where available
- Helper functions: `get_timestamp()`, `set_analysis_seed()`, and the package loaders `load_visualization_packages()`, `load_timeseries_packages()`, `load_web_packages()`

## Plotting and tables

### plotting_utils.R
Consistent plot creation and saving:
- `save_plot_svg()` - Save plots in SVG format
- `apply_theme()` - Apply a consistent minimal theme to ggplot objects
- `save_correlation_heatmap()` - Create and save correlation heatmaps
- `create_time_series_plot()` - Create multi-series time series plots
- `display_and_save_plot()` - Display in the viewer and save to disk

### latex_table_utils.R
Publication LaTeX panel tables (booktabs/threeparttable/siunitx):
- `build_panel_latex_table()` - Build a multi-panel table fragment
- `make_standalone_latex()` - Wrap a fragment in a compilable document
- `write_latex_table()` - Write the fragment and its standalone variant
- `.brace_s_cell()` - Wrap non-numeric cells in braces for siunitx S columns

### latex_simple_table.R
Plain-column booktabs/threeparttable LaTeX table (`l c c ...`), the
non-numeric-cell counterpart to `build_panel_latex_table`'s siunitx
S-columns -- it left-aligns row labels and centers data columns so
interval strings such as `[lo, hi]` render cleanly:
- `build_simple_latex_table()` - Build the fragment (supports a
  `rule_after` argument to group blocks of rows with `\midrule`);
  reuses `make_standalone_latex()` / `write_latex_table()` from
  `latex_table_utils.R` for the standalone variant and file writing

### format_utils.R
Finite/Inf-aware formatting for identification results:
- `format_bound()` - Format bounds as finite/unbounded/"unreliable" strings
- `format_width()` - Format interval widths under the same convention
- `format_reduction()` - Format per-row percent reduction with validity awareness
- `mean_pct_reduction()` - Mean percent reduction over valid, finite, positive-baseline components

## Statistics and diagnostics

### stats_utils.R
Statistical analysis and data validation:
- `compute_summary_stats()` - Mean, SD, quantiles, skewness, kurtosis, autocorrelations
- `perform_stationarity_tests()` - Run ADF, KPSS, and Ljung-Box tests
- `kpss_pvalue()` - Interpolate a KPSS p-value from critical values
- `create_formatted_table()` - Create publication-ready gt tables
- `create_interactive_table()` - Create interactive DT tables
- `check_data_completeness()` - Check for missing values (optionally stop)

### hetero_test_utils.R
Core heteroskedasticity testing:
- `perform_all_hetero_tests()` - Run the skedastic suite (White, BP, Goldfeld-Quandt, Harvey, Anscombe, Cook-Weisberg) on a fitted lm
- `summarize_hetero_tests()` - Summarize rejection counts and rates

### hetero_lm_tests.R
LM-style tests and regime selection:
- `bp_lm_test()` - Breusch-Pagan LM test
- `arch1_test()` - ARCH(1) LM test
- `w2_refit_fitted_ratio()` - Fitted-SD ratio used to pick the diagnostics regime (A vs B)
- `select_diagnostics_suite()` - Select the regime-dependent test suite
- `suite_na_row()` - NA fallback row matching `perform_all_hetero_tests()` columns

### hetero_panel_meta.R
Regime-aware metadata for stage-02 exports:
- `hetero_suite_meta()` - Test names, labels, and symbols for the diagnostics table
- `hetero_table_notes()` - LaTeX table notes explaining the test panels

### hetero_plot_utils.R
- `create_hetero_diagnostic_plots()` - Residuals-vs-fitted, scale-location, squared-residual, and residual-vs-index diagnostic plots

### hetero_diag_figures.R
- `build_hetero_pvalue_figure()` - -log10 p-values across maturities
- `build_hetero_corr_heatmap()` - Absolute correlations between instruments and squared W2 residuals

## Identification inputs

### identification_utils.R
Identification setup plumbing:
- `get_identification_maturity_lookup()` - Map components to bond maturities
- `load_identification_inputs()` - Load data and construct yields/term premia/PCs inputs
- `assert_w2_alignment()` - Validate row alignment of W2 residuals across maturities
- `w2_response_dates()` - Recover the response-row dates kept by the W2 complete-case filter
- `assert_w1_leading_block()` - Validate the W1 (consumption) complete-case filter kept a contiguous trailing block (Y1 own-lags drop only a leading prefix)
- `compute_identification_residuals()` - Compute W1/W2 residuals and aligned instruments (honors `impose_news_projection_zero()` for the B=0 vs estimate-B mode; also retains the Y2-on-PC `beta2R` coefficients for structural recovery)
- `build_pipeline_quadratic_system()` - Single pipeline front door: assemble the identified-set quadratic system through the exported generalized-instrument builder `build_general_quadratic_system()`, re-attaching the `hetid_components` class/attributes; `HETID_ASSERT_EQUIV` makes it additionally assert numeric identity with the legacy `build_quadratic_system()`
- `assert_pipeline_quadratic_equiv()` - Numeric-leaf identity check between the generalized and legacy quadratic systems (used under `HETID_ASSERT_EQUIV`)
- `get_baseline_gamma()` - VFCI raw PC loading matrix (J x I; requires 4 PCs)
- `get_tau_spec()` - Tolerance specification (`tau_point` and `tau_set`)

### z_source.R
Instrument-matrix (Z) hook:
- `get_identification_z()` - Return the instrument matrix (default or `HETID_Z_SOURCE` script)
- `z_source_active()` - TRUE if a custom Z source is active
- `assert_z_matches_moments()` - Validate the rebuilt Z reproduces stored moments

### gamma_source.R
Baseline-gamma hook:
- `baseline_gamma_method()` - Current method from `HETID_BASELINE_GAMMA`
- `resolve_baseline_gamma()` - Resolve the gamma matrix (vfci / custom file)
- `build_reduced_form_gamma(beta2r)` - Maturities-mode Y2-on-PC slope gamma from the retained `beta2R` coefficients
- `reduced_form_gamma_or_skip()` - `build_reduced_form_gamma()` wrapper that skips gracefully when no PC slopes are available
- `classify_common_design_cols()` - Split common-conditioning column names into PC slopes vs other (lags/intercept) blocks
- `assert_beta_columns_matched()` - Validate that the `beta1R`/`beta2R` reduced-form coefficient blocks share aligned columns

### news_projection.R
News-projection mode hook (mirrors `baseline_gamma_method()`):
- `impose_news_projection_zero()` - Resolve whether the W2-residual construction imposes the exact-news projection `B = 0` (TRUE) or estimates `B` from the data (FALSE, the default); reads `HETID_IMPOSE_NEWS_PROJECTION_ZERO`, falling back to the `IMPOSE_NEWS_PROJECTION_ZERO` script constant

### baseline_spec.R
Stage-04 baseline spec stamp and fail-closed consistency guard:
- `current_baseline_spec()` - Resolve the spec the current process is configured for (Y1 lags, news-projection mode, Z source); single source of truth for both the Stage-04 stamp and the downstream check
- `baseline_spec_mismatches()` - Pure comparison core returning human-readable mismatch lines (an absent field in an old baseline is treated as a mismatch, failing toward safety)
- `assert_baseline_spec_current()` - Abort downstream stages (05 optimizer, 06 tables) if the loaded baseline RDS was produced under different settings

## Set construction and bounds

### profile_bounds.R
Profile-bound API:
- `solve_profile_bound()` - Solve one profile bound (min/max of theta_k) with coordinate tracking
- `solve_linear_functional_bound()` - Min/max a linear functional of theta (e.g. a recovered structural coefficient `beta1_k(theta) = beta1R_k - c_k'theta`) over the quadratically-constrained set, with scale-aware unbounded detection; the engine behind the stage-06 consumption-equation interval bounds
- `solve_all_profile_bounds()` - Solve all components in both directions; returns a bounds table
- `solve_point_identification()` - Closed-form tau=0 point identification

### profile_bounds_core.R
Non-dimensionalized solver internals:
- `.derive_theta_scale()`, `.derive_constraint_scales()` - Variable and per-constraint scaling
- `.solve_scaled()` - Solve the scaled quadratic program (SLSQP)
- `.feasibility_residual()`, `.solve_finite()`, `.finalize_bound()` - Feasibility/validity certification

### ixj_identification.R
I×J separate-instrument set:
- `make_basis_gamma()` - Canonical basis-vector gamma (e_j in column j)
- `build_ixj_quadratic_system()` - One quadratic constraint per (component, instrument) pair

### closure_membership.R
Identified-set membership probes (complements the profile-bound
interval widths) built on the package's `make_system_checker` /
`make_constraint_checker` closures:
- `make_theta_grid()` - Build a theta grid over the profile-bound box
  plus its center and corners
- `probe_set_membership()` - Probe set membership across the grid via
  the constraint-checker closure

## Weight optimization

### optimization_utils.R
- `compute_total_width()` - Sum profile-interval widths from a bounds table
- `normalize_gamma_columns()` - Euclidean unit-normalize gamma columns (display)
- `UNBOUNDED_PENALTY` - Constant (`1e12`) steering penalty for the inner SLSQP objective only; selection/reporting never read it (terminal points are re-evaluated with `honest_width_lambda`)

### lambda_mask.R
Optimizer packing and support masks:
- `lambda_dims()`, `pack_lambda()`, `unpack_lambda()` - Dimensions and vector<->list codecs
- `coerce_lambda_start()` - Coerce a legacy matrix start to list form
- `validate_support_mask()`, `support_free_mask()` - Support-mask construction/validation
- `pack_active()`, `unpack_active()` - Free-element codecs under a support mask
- `encode_lambda()`, `decode_lambda()` - Whitened mu-space codecs
- `honest_width_lambda()` - Total-width oracle at fixed tau

### lambda_whitening.R
Whitening reparameterization (`lambda' Var(Z) lambda = 1` as search geometry):
- `check_whiten_names()` - Validate instrument names against moments
- `parse_whiten_input()` - Parse/validate tagged whiten input into vcov and ridge
- `whiten_context()` - Build the whitening context (Cholesky factor, support rows)

### lambda_varnorm.R
Variance normalization of optimized weights:
- `lambda_vcov_quadform()` - `lambda' V lambda` on support rows
- `assert_lambda_variance_nonzero()` - Pre-optimization nonzero-variance check
- `normalize_lambda_columns_vcov()` - Normalize columns so `lambda' V lambda = 1`
- `lambda_variance_diagnostic()`, `lambda_variance_report()` - Per-column variance diagnostics/report

### lambda_optimization.R
Outer weight optimizer:
- `normalize_lambda_columns()` - Unit-normalize each component's columns
- `objective_lambda_only()` - Inner total-width objective for a parameter vector
- `run_lambda_optimization()` - Outer multistart SLSQP over lambda weights

## Specification and tau* comparison

### spec_comparison_eval.R
Per-cell evaluators for the (n_pcs, components, gamma, tau) grid, where the gamma scheme is `vfci` / `optimized` / `separate` (I×J):
- `spec_moments()` - Load and compute moments for one cell
- `eval_fixed()`, `eval_opt()`, `eval_ixj()` - Width for fixed gamma, optimized lambda, and I×J
- `compute_group_rows()` - All (gamma, tau) rows for a group

### tau_star_utils.R
tau* machinery (slack where the set transitions bounded->unbounded):
- `tau_quadratic_system()` - Quadratic system for fixed gamma at slack tau
- `eval_width_at_tau()`, `.sweep_row()`, `sweep_fixed_gamma()` - Width sweeps over a tau grid
- `fine_tau_grid()` - Fine grid inside the bounded region
- `tau_star_fixed()` - Bisect the transition for fixed gamma
- `tau_star_optimized()` - tau* by re-optimizing lambda at each candidate
- `recession_metric()` - Curvature-degeneracy diagnostic

## Paper specification (stage 08)

### paper_spec_residuals.R
Collapse the per-maturity SDF news into ONE principal component (Y2, I = 1)
and align the single de-meaned VFCI instrument (Z, J = 1):
- `compute_paper_spec_residuals()` - Reuses `compute_identification_residuals()`
  for all date alignment; the news factor is the RAW (uncentered) PC1
  projection (`news_mat %*% combo`), NOT prcomp scores, so the reduced-form
  intercept and `b2R` stay correct with no intercept correction (an internal
  `.reconcile_maturities()` matches the news / residual / `beta2R` objects by
  maturity name)

### paper_spec_estimator.R
From the I = 1 / J = 1 residuals, assemble everything the three `for_paper`
tables need:
- `compute_paper_spec_estimator()` - The theta identified set, the `beta1(theta)`
  structural intervals, tau*, the heteroskedasticity-relevance tests of W2 on the
  VFCI instrument, the relevance diagnostics that replace the vacuous kappa(Q) at
  I = J = 1, the endogeneity correlation, and the OLS benchmark
- `paper_spec_set_columns()` - Per-coefficient identified-set intervals at a given
  slack (theta from profile bounds; every other coefficient via the linear
  functional `beta1_p(theta) = beta1r_p - beta2r_p . theta`)

### paper_spec_bootstrap.R
- `compute_paper_spec_bootstrap()` - Moving-block bootstrap band (block = 15,
  200 reps, deterministic given `SEED`): resamples quarters, recomputes the
  moments under the fixed unit gamma, and re-derives tau*, the set endpoints at
  `tau_set`, and the relevance moment Cov(Z, W2^2)

### for_paper_guard.R
Fail-closed invariant for `output/for_paper/` (exactly the three paper-spec
tables, no aux, no subdirs):
- `for_paper_allowlist()` - The 12 exact basenames permitted
  (`FOR_PAPER_TABLE_STEMS` x {`.tex`, `_standalone.tex`, `_standalone.pdf`,
  `.csv`})
- `assert_for_paper_allowlist()` - Abort if `for_paper` has any unexpected or
  missing file, or any subdir; run both to validate the stage-08 staging dir
  before publishing and to re-check `for_paper` at the end of `run_all_scripts.R`

## tests/

Unit tests for the utility layer: lambda packing/whitening/varnorm/optimization, profile bounds, I×J identification, closure membership, hetero tests, gamma sources, the news-projection mode (`test_news_projection.R`, `test_impose_b_zero.R`), the Stage-04 baseline spec stamp (`test_baseline_spec.R`), residual alignment and saved-residual dates (`test_identification_alignment.R`, `test_saved_residuals_dates.R`), the Z-width pipeline, the generalized-vs-legacy pipeline equivalence (`test_pipeline_equivalence.R`, which toggles `HETID_ASSERT_EQUIV`), the tau* utils (`test_tau_star_utils.R`), the stage-08 paper-spec residuals/estimator (`test_paper_spec_residuals.R`, `test_paper_spec_estimator.R`), and stats -- plus the `pipeline_value_diff.R` before/after value-comparison harness and a `fixtures/` directory of capture scripts and saved fixture RDS.

## Usage

Sourcing `common_settings.R` loads the core utility layer; scripts that need a specialized helper (e.g. `ixj_identification.R`, `spec_comparison_eval.R`) source it directly in addition. To pull in the core layer from a new script:

```r
source(here::here("scripts/utils/common_settings.R"))
```

## Benefits

1. **Consistency**: All plots and tables have consistent formatting
2. **Maintainability**: Changes to formatting only need to be made in one place
3. **Reliability**: Common functionality is tested and debugged once
4. **Efficiency**: Reduces code duplication across scripts
5. **Clarity**: Scripts focus on analysis logic rather than formatting details
