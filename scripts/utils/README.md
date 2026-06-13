# Utility Functions for hetid Scripts

This directory contains reusable utility functions that consolidate common patterns across the analysis scripts, following the DRY (Don't Repeat Yourself) principle. All files are sourced through `common_settings.R`.

## Configuration

### common_settings.R
- Defines shared config constants (`NEWS_STEP`, `PIPELINE_ACM_MATURITIES`, `SEED`, `BASELINE_TAU`, `N_CORES`, plot/table settings, `DATA_RDS_PATH`) and output directories
- Sources every utility file below; uses package constants from hetid where available
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
- `compute_identification_residuals()` - Compute W1/W2 residuals and aligned instruments
- `get_baseline_gamma()` - VFCI unit-norm loading matrix (J x I)
- `get_tau_spec()` - Tolerance specification (`tau_point` and `tau_set`)

### factor_utils.R
Yield-curve PCA factors:
- `compute_yield_factor_loadings()` - PCA loadings with variance explained
- `get_identification_factor_lookup()` - Map component IDs to factor indices and labels
- `compute_w2_factor_residuals()` - Project W2 residuals onto factors
- `get_reduced_form_gamma()` - Per-factor gamma weights from levels-regression slopes

### z_source.R
Instrument-matrix (Z) hook:
- `get_identification_z()` - Return the instrument matrix (default or `HETID_Z_SOURCE` script)
- `z_source_active()` - TRUE if a custom Z source is active
- `assert_z_matches_moments()` - Validate the rebuilt Z reproduces stored moments

### gamma_source.R
Baseline-gamma hook:
- `baseline_gamma_method()` - Current method from `HETID_BASELINE_GAMMA`
- `resolve_baseline_gamma()` - Resolve the gamma matrix (vfci / reduced_form / custom file)

## Set construction and bounds

### profile_bounds.R
Profile-bound API:
- `solve_profile_bound()` - Solve one profile bound (min/max of theta_k) with coordinate tracking
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

## Weight optimization

### optimization_utils.R
- `compute_total_width()` - Sum profile-interval widths from a bounds table
- `normalize_gamma_columns()` - Euclidean unit-normalize gamma columns (display)

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
Per-cell evaluators for the (mode, n_pcs, components, gamma, tau) grid:
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

## tests/

Unit tests for the utility layer (lambda/whitening/optimization, profile bounds, I×J, hetero tests, gamma/Z sources, stats) plus a `fixtures/` directory of capture scripts.

## Usage

All scripts in the analysis pipeline automatically source these utilities through `common_settings.R`. To use in a new script:

```r
source(here::here("scripts/utils/common_settings.R"))
```

## Benefits

1. **Consistency**: All plots and tables have consistent formatting
2. **Maintainability**: Changes to formatting only need to be made in one place
3. **Reliability**: Common functionality is tested and debugged once
4. **Efficiency**: Reduces code duplication across scripts
5. **Clarity**: Scripts focus on analysis logic rather than formatting details
</content>
