# Utility Functions for hetid Scripts

_Last modified: 2026-07-16 16:22 EDT_

This directory contains reusable utility functions that consolidate common patterns
across the analysis scripts, following the DRY (Don't Repeat Yourself) principle.
Citations below are `file:line`. A bare filename (or a bare `:NN` inside a bullet)
refers to a file in this directory (`scripts/utils/`); every other path resolves
against the repository root. A citation points at the relevant call, whose arguments
may run onto the following lines. Re-check any claim directly rather than trusting
this file.

## What gets loaded, and how

`common_settings.R` sources the layer through an explicit `file.exists`-guarded block
(not a loop), in two parts:

- **Core** (`common_settings.R:124-183`), in order: `plotting_utils.R`,
  `stats_utils.R`, `hetero_test_utils.R`, `hetero_plot_utils.R`, `z_source.R`,
  `gamma_source.R`, `news_projection.R`, `baseline_spec.R`, `identification_utils.R`,
  `optimization_utils.R`, `lambda_mask.R`, `lambda_whitening.R`, `lambda_varnorm.R`,
  `lambda_optimization.R`, `profile_bounds_core.R`, `profile_bounds.R`,
  `format_utils.R`, `latex_table_utils.R`, `tau_star_utils.R`,
  `closure_membership.R`.
- **Stage-08 paper-spec layer** (`common_settings.R:188-205`): `hetero_lm_tests.R`,
  `latex_simple_table.R`, `paper_spec_residuals.R`, `paper_spec_estimator.R`,
  `paper_spec_bootstrap.R`, `for_paper_guard.R`. The first two are sourced here
  because the paper spec needs them even though stage 02 and the consumption table
  also source them locally (`common_settings.R:184-187`).

`set_id_inference.R` is loaded transitively as part of that second block:
`paper_spec_bootstrap.R:7` sources it.

Four helpers are **not** sourced by `common_settings.R`; the stage scripts that
consume them source them directly:

- `hetero_panel_meta.R` — `scripts/02_identification_diagnostics/output_results.R:35`
- `hetero_diag_figures.R` —
  `scripts/02_identification_diagnostics/heteroskedasticity_tests.R:157`
- `ixj_identification.R` —
  `scripts/04_identification_without_optimization/compute_identification_ixj.R:13` and
  `scripts/05_identification_with_optimization/spec_comparison.R:27`
- `spec_comparison_eval.R` — `scripts/05_identification_with_optimization/spec_comparison.R:40`

`set_id_bootstrap_core.R` has no consumer in the numbered pipeline today — only its
own test (`scripts/utils/tests/test_set_id_bootstrap_core.R:11`) sources it.

(The spec-comparison grid design `spec_comparison_design.R` is a stage-05-local file,
not a `utils/` helper.)

## Pipeline boundaries

These utilities support the numbered `scripts/` analysis pipeline. The separate paper
pipeline owns independent copies of every support function it needs under
`scripts-paper/support/` and never sources this directory. Run it from the package
root with:

```sh
Rscript scripts-paper/run_pipeline.R
```

See `scripts-paper/README.md` for that pipeline's semantic modules, tests, gates, and
typed artifact layout. Two files here are deliberately paired with independent
implementations over there (`set_id_bootstrap_core.R:9-10`, `set_id_inference.R:7-8`);
see `scripts-paper/support/README.md:16-17` for the mapping.

## Configuration

### common_settings.R
- Defines shared config constants: `NEWS_STEP = 3L` (`:41`),
  `PIPELINE_ACM_MATURITIES` (`:42` — derived as every multiple of `NEWS_STEP` up to
  `HETID_CONSTANTS$MAX_MATURITY`), `SEED = 123` (`:43`),
  `BASELINE_TAU = 0.05` (`:44`), `OPT_TAU_CAP = 0.99` (`:48` — the near-uninformative
  slack cap shared across the tau* oracle and the set probes; admissible slack is tau
  in [0, 1)), `N_Y1_LAGS = 4L` (`:49`), `IMPOSE_NEWS_PROJECTION_ZERO = FALSE` (`:51`),
  `TAU_STAR_N_STARTS = 15L` (`:52`), `N_CORES = parallel::detectCores() - 1` (`:53`),
  `MAX_N_PCS` from the package constants (`:54`), `FOR_PAPER_TABLE_STEMS` (`:59-63`),
  the ACM column prefixes `YIELD_PREFIX`/`TP_PREFIX` (`:67-68`), plot/table settings
  (`:71-79`), and `DATA_RDS_PATH` (`:22`)
- Defines the output directories `OUTPUT_DIR`, `OUTPUT_PAPER_DIR`, `OUTPUT_TEMP_DIR`
  (`:15-18`). `OUTPUT_DIR` is hardcoded to `scripts/output` — there is no environment
  override — and the directory skeleton is created eagerly at `:25-32`
- Sources the utility layer (see "What gets loaded, and how" above); uses package
  constants from hetid where available
- Helper functions: `get_timestamp()` (`:89`), `set_analysis_seed()` (`:94`), and the
  package loaders `load_visualization_packages()` (`:100`),
  `load_timeseries_packages()` (`:109`), `load_web_packages()` (`:116`)

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
- `write_latex_table()` - Write the fragment (`<stem>.tex`) and its standalone
  variant (`<stem>_standalone.tex`) (`:187-193`)
- `compile_latex_pdf()` (`:128`), `clean_latex_sidecars()` (`:157`) - latexmk compile
  and aux cleanup. Nothing under `scripts/` calls either today; stage 08 compiles via
  its own local `compile_pdf()` (`scripts/08_paper_spec/render_paper_tables.R:27`)
- `.brace_s_cell()` - Wrap non-numeric cells in braces for siunitx S columns

### latex_simple_table.R
Plain-column booktabs/threeparttable LaTeX table (`l c c ...`), the non-numeric-cell
counterpart to `build_panel_latex_table`'s siunitx S-columns -- it left-aligns row
labels and centers data columns so interval strings such as `[lo, hi]` render
cleanly:
- `build_simple_latex_table()` - Build the fragment (supports a `rule_after` argument
  to group blocks of rows with `\midrule`); reuses `make_standalone_latex()` /
  `write_latex_table()` from `latex_table_utils.R` for the standalone variant and
  file writing

### format_utils.R
Finite/Inf-aware formatting for identification results (four-state aware:
`valid=FALSE/NA` -> "unreliable"; non-finite -> "unbounded"):
- `year_quarter()` - Calendar quarter label `YYYYQq` for a Date or Date vector,
  shared by the table builders so sample-period labels cannot drift
  (`scripts/06_results_production/create_consumption_equation_table.R:264`,
  `scripts/08_paper_spec/paper_spec_tables.R:29`)
- `format_bound()` - Format bounds as finite/unbounded/"unreliable" strings
- `format_width()` - Format interval widths under the same convention
- `format_reduction()` - Format per-row percent reduction with validity awareness
- `mean_pct_reduction()` - Mean percent reduction over valid, finite,
  positive-baseline components

## Statistics and diagnostics

### stats_utils.R
Statistical analysis and data validation:
- `mbb_index()` - Moving-block bootstrap index: resample a series in contiguous
  blocks, preserving short-run dependence. Shared by the paper-spec and
  results-companion tau* bootstraps (`paper_spec_bootstrap.R:54`,
  `scripts/results_companion/diag_taustar_bootstrap.R:57`); the caller seeds, not this
- `compute_summary_stats()` - Mean, SD, quantiles, skewness, kurtosis,
  autocorrelations
- `kpss_pvalue()` - Interpolate a KPSS p-value from critical values
- `perform_stationarity_tests()` - Run ADF, KPSS, and Ljung-Box tests
- `create_formatted_table()` - Create publication-ready gt tables
- `create_interactive_table()` - Create interactive DT tables
- `check_data_completeness()` - Check for missing values (optionally stop)

### hetero_test_utils.R
Core heteroskedasticity testing:
- `perform_all_hetero_tests()` - Run the requested subset of the skedastic suite on a
  fitted lm. The `tests` argument defaults to all six (White, BP, Goldfeld-Quandt,
  Harvey, Anscombe, Cook-Weisberg; `:19-22`), but callers pass a subset — the
  pipeline never runs Cook-Weisberg, because `select_diagnostics_suite()` excludes it
  from both regimes
- `summarize_hetero_tests()` - Summarize rejection counts and rates over the rows
  where each test produced a non-NA p-value; a missing p-value column errors rather
  than silently returning NA (`:96-102`)

### hetero_lm_tests.R
LM-style tests, regime selection, and the joint-relevance rank test:
- `bp_lm_test()` - Breusch-Pagan LM test: squared residuals on whatever `regressors`
  matrix the caller supplies (the PC levels by default, custom instruments under a
  Z hook)
- `arch1_test()` - ARCH(1) LM test (squared residuals on their one-period lag)
- `w2_refit_fitted_ratio()` - Fitted-SD ratio used to pick the diagnostics regime
  (A vs B)
- `select_diagnostics_suite()` - Select the regime-dependent test suite (`:56-69`).
  Regime B (ratio > 1e-3) = White/BP/GQ/Harvey/Anscombe; Regime A = the same without
  the 0/0-degenerate Anscombe. Cook-Weisberg is excluded in both (it duplicates the
  non-studentized Breusch-Pagan and over-rejects under heavy tails, `:44-46`). The
  Goldfeld-Quandt deflator is fixed per regime rather than data-chosen (column 2,
  falling back to column 1 for single-instrument hooks) and two-sided (`:66-67`)
- `rk_rank_test()` - KP-style joint-relevance rank test of a scalar driver z for the
  columns of y2: tests rank(M_Z) = I - 1 against full rank via a Newey-West t-test at
  the least-moved direction, chi-sq(1) under the null, and returns the spectrum
  summaries (det, condition number, smallest singular value, separation ratio)
  (`:71-104`). No numbered stage calls it today; `tests/test_hetero_lm_tests.R`
  exercises it
- `suite_na_row()` - NA fallback row matching `perform_all_hetero_tests()` columns.
  It resolves the *caller's* `suite_tests` vector at call time (`:4`, `:108-109`)

### hetero_panel_meta.R
Regime-aware metadata for stage-02 exports:
- `hetero_suite_meta()` - Test names, labels, and symbols for the diagnostics table
- `hetero_table_notes()` - LaTeX table notes explaining the test panels

### hetero_plot_utils.R
- `create_hetero_diagnostic_plots()` - Returns (invisibly) a list of exactly four
  ggplot objects — `residuals_vs_fitted`, `scale_location`, `squared_residuals`,
  `residuals_vs_index` (`:71-76`, `:122`). A combined 2x2 grid is additionally saved
  when `save_plots` and `plot_dir` are supplied (`:103`) and printed when
  `display_plots` is set (`:116`), but it is not part of the return value

### hetero_diag_figures.R
- `build_hetero_pvalue_figure()` - `-log10` p-values across maturities, as a
  line-and-point profile (`:17-27`), not a heatmap
- `build_hetero_corr_heatmap()` - Absolute correlations between instruments and
  squared W2 residuals, as a `geom_tile` heatmap (`:41`)

## Identification inputs

### identification_utils.R
Identification setup plumbing. `DEFAULT_ID_MATURITIES <- c(24L, 60L, 108L)` are bond
maturities in **months** (`:3-4`):
- `get_identification_maturity_lookup()` - Map components to bond maturities
- `load_identification_inputs()` - Load data and construct yields/term premia/PCs
  inputs
- `assert_w2_alignment()` - Validate row alignment of W2 residuals across maturities
- `w2_response_dates()` - Recover the response-row dates kept by the W2 complete-case
  filter
- `assert_w1_leading_block()` - Validate that the W1 (consumption) complete-case
  filter kept a **contiguous** block of rows (`:118`) — Y1 own-lags should drop only
  a leading prefix, so interior drops would break the map from residual row t to a
  single calendar period and misalign W1 against the W2 residuals and instruments.
  The assertion is contiguity; it does not separately require the block to reach the
  final input row
- `compute_identification_residuals()` - Compute W1/W2 residuals and aligned
  instruments (honors `impose_news_projection_zero()` for the B=0 vs estimate-B mode;
  also retains the Y2-on-PC `beta2R` coefficients for structural recovery)
- `build_pipeline_quadratic_system()` - Single pipeline front door: assemble the
  identified-set quadratic system through the exported generalized-instrument builder
  `build_general_quadratic_system()`, re-attaching the `hetid_components`
  class/attributes; `HETID_ASSERT_EQUIV` (`:275`) makes it additionally assert
  numeric identity with the legacy `build_quadratic_system()`
- `assert_pipeline_quadratic_equiv()` - Numeric-leaf identity check between the
  generalized and legacy quadratic systems (used under `HETID_ASSERT_EQUIV`)
- `vfci_pc_loading()` - The raw VFCI loading on `pc1..pc4`, recovered from the
  bundled data by regressing `vfci` on the raw PCs at run time rather than frozen as
  a literal (a literal goes stale the moment the upstream import re-estimates the
  PCs). The VFCI is exactly a linear combination of the raw PCs, so this is an
  identity rather than an estimate, and the weights are the regression coefficients
  themselves — NOT unit-normalized — so that the combined instrument `PC %*% gamma`
  literally is `VFCI - mean(VFCI)`. Fail-closed: it aborts if `pc1..pc4` is rank
  deficient, and if the relative residual exceeds `1e-8`, i.e. if vfci ever stops
  being an exact combination (the exact fit sits at ~1e-15)
- `get_baseline_gamma()` - VFCI raw PC loading matrix (J x I; requires 4 PCs), built
  from `vfci_pc_loading()`
- `get_tau_spec()` - Tolerance specification (`tau_point` and `tau_set`)

### z_source.R
Instrument-matrix (Z) hook:
- `get_identification_z()` - Return the instrument matrix (default, or the
  `build_z(data)` defined by the R file `HETID_Z_SOURCE` points at; `:24-62`)
- `z_source_active()` - TRUE if a custom Z source is active (`:64-72`)
- `assert_z_matches_moments()` - Validate the rebuilt Z reproduces stored moments

### gamma_source.R
Baseline-gamma hook:
- `baseline_gamma_method()` - Current method from `HETID_BASELINE_GAMMA`, default
  `"vfci"` (`:12-14`). This is the only place the env var is read; callers pass the
  result on to `resolve_baseline_gamma()`
- `resolve_baseline_gamma()` - Resolve the gamma matrix (vfci / custom file); takes
  the method as an argument (`:136`)
- `build_reduced_form_gamma(beta2r)` - Maturities-mode Y2-on-PC slope gamma from the
  retained `beta2R` coefficients
- `reduced_form_gamma_or_skip()` - `build_reduced_form_gamma()` wrapper that skips
  gracefully when no PC slopes are available
- `classify_common_design_cols()` - Split common-conditioning column names into PC
  slopes vs other (lags/intercept) blocks
- `assert_beta_columns_matched()` - Validate that the `beta1R`/`beta2R` reduced-form
  coefficient blocks share aligned columns

### news_projection.R
News-projection mode hook (mirrors `baseline_gamma_method()`):
- `impose_news_projection_zero()` - Resolve whether the W2-residual construction
  imposes the exact-news projection `B = 0` (TRUE) or estimates `B` from the data
  (FALSE, the default); reads `HETID_IMPOSE_NEWS_PROJECTION_ZERO` (`:9`), falling
  back to the `IMPOSE_NEWS_PROJECTION_ZERO` script constant

### baseline_spec.R
Stage-04 baseline spec stamp and fail-closed consistency guard:
- `current_baseline_spec()` - Resolve the spec the current process is configured for
  — `y1_lags`, `news_projection_mode`, `z_source` (`:15-25`); single source of truth
  for both the Stage-04 stamp and the downstream check
- `baseline_spec_mismatches()` - Pure comparison core returning human-readable
  mismatch lines (an absent field in an old baseline is treated as a mismatch,
  failing toward safety)
- `assert_baseline_spec_current()` - Abort a downstream stage if the loaded baseline
  RDS was produced under different settings. Called by
  `scripts/04_identification_without_optimization/compute_identification_ixj.R:86`,
  `scripts/05_identification_with_optimization/optimize_identification.R:17`,
  `scripts/06_results_production/assemble_results.R:23`, and
  `scripts/06_results_production/create_consumption_equation_table.R:53`

## Set construction and bounds

### profile_bounds.R
Profile-bound API:
- `solve_profile_bound()` - Solve one profile bound (min/max of theta_k) with
  coordinate tracking
- `solve_linear_functional_bound()` - Min/max a linear functional of theta (e.g. a
  recovered structural coefficient `beta1_k(theta) = beta1R_k - c_k'theta`) over the
  quadratically-constrained set, with scale-aware unbounded detection; the engine
  behind the stage-06 consumption-equation interval bounds
- `solve_all_profile_bounds()` - Solve all components in both directions; returns a
  bounds table
- `solve_point_identification()` - Closed-form tau=0 point identification

What the `valid` certificate does and does not mean: it certifies that the returned
point is FEASIBLE and has at least one ACTIVE constraint. It does **not** certify
global optimality — there is no stationarity/multiplier check, and on a non-convex
(indefinite `A_i`) set a premature boundary stall can still pass
(`profile_bounds_core.R:96-99`).

### profile_bounds_core.R
Non-dimensionalized solver internals:
- `.derive_theta_scale()`, `.derive_constraint_scales()` - Variable and
  per-constraint scaling
- `.solve_scaled()` - Solve the scaled quadratic program (SLSQP)
- `.feasibility_residual()`, `.solve_finite()`, `.finalize_bound()` -
  Feasibility/validity certification
- `.finalize_linear_bound()` - Linear-functional analog of `.finalize_bound()`: the
  bound is the theta-units functional value, certified by the same
  solver-independent feasible+active residual check (`:129`)

### ixj_identification.R
I×J separate-instrument set:
- `make_basis_gamma()` - Canonical basis-vector gamma (e_j in column j)
- `build_ixj_quadratic_system()` - One quadratic constraint per (component,
  instrument) pair

### closure_membership.R
Identified-set membership probes (complements the profile-bound interval widths)
built on the package's `make_system_checker` / `make_constraint_checker` closures:
- `make_theta_grid()` - Build a theta grid over the profile-bound box plus its center
  and corners
- `probe_set_membership()` - Probe set membership across the grid via the
  constraint-checker closure

## Weight optimization

### optimization_utils.R
- `compute_total_width()` - Sum profile-interval widths from a bounds table
- `normalize_gamma_columns()` - Euclidean unit-normalize gamma columns (with a
  zero-column guard). Shared: it is the search-space representative inside the
  optimizer (`lambda_optimization.R:39`), backs the duplicate-direction diagnostic
  (`:175`), and is reused by the vcov normalizer (`lambda_varnorm.R:95`)
- `UNBOUNDED_PENALTY` - Constant (`1e12`, `:27`) steering penalty for the inner SLSQP
  objective only. It cannot win selection: `run_lambda_optimization` re-evaluates
  every terminal point with `honest_width_lambda`. It can still surface in
  diagnostics — the per-start objective values written to `optimization_trace.csv`
  come from each start's raw `$value`
  (`scripts/05_identification_with_optimization/optimize_identification.R:146-151`, `:198`)

### lambda_mask.R
Optimizer packing and support masks:
- `lambda_dims()`, `pack_lambda()`, `unpack_lambda()` - Dimensions and vector<->list
  codecs
- `coerce_lambda_start()` - Coerce a legacy matrix start to list form
- `validate_support_mask()`, `support_free_mask()` - Support-mask
  construction/validation
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
- `lambda_variance_diagnostic()`, `lambda_variance_report()` - Per-column variance
  diagnostics/report

### lambda_optimization.R
Outer weight optimizer (the single outer optimizer; the legacy Euclidean gamma
optimizer is retired, `optimization_utils.R:1-10`):
- `normalize_lambda_columns()` - Unit-normalize each component's columns
- `objective_lambda_only()` - Inner total-width objective for a parameter vector
- `run_lambda_optimization()` - Outer multistart `nloptr::slsqp` (`:124`) over lambda
  weights; `whiten` is a required argument with no default (`:75-81`)

## Specification and tau* comparison

### spec_comparison_eval.R
Per-cell evaluators for the (n_pcs, components, gamma, tau) grid, where the gamma
scheme is `vfci` / `optimized` / `separate` (I×J):
- `spec_moments()` - Load and compute moments for one cell
- `eval_fixed()`, `eval_opt()`, `eval_ixj()` - Width for fixed gamma, optimized
  lambda, and I×J
- `compute_group_rows()` - All (gamma, tau) rows for a group; `vfci` is evaluated only
  at `n_pcs == 4` (`:95`), `optimized` and `separate` only at `tau > 0` (`:113`)

### tau_star_utils.R
tau* machinery (slack where the set transitions bounded->unbounded):
- `tau_quadratic_system()` - Quadratic system for fixed gamma at slack tau
- `eval_width_at_tau()`, `.sweep_row()`, `sweep_fixed_gamma()` - Width sweeps over a
  tau grid
- `coef_interval_tables()` - Per-coefficient `beta1(theta)` interval tables at a
  given slack; a zero `beta2r` column is a constant functional needing no solve, and
  is certified only when no theta side failed closed (`:36-42`). Consumed by
  `set_id_bootstrap_core.R:68`
- `fine_tau_grid()` - Fine grid inside the bounded region
- `tau_star_fixed()` - Bisect the transition for fixed gamma, branching on the
  *certified* status rather than the raw bounded flag (`:118`)
- `tau_star_optimized()` - tau* by re-optimizing lambda at each candidate; brackets
  upward by doubling, then bisects (`:154-170`)
- `recession_metric()` - Curvature-degeneracy diagnostic

## Set-identification inference

### set_id_inference.R
Sampling-uncertainty helpers for the set-identified coefficients. Reaches the session
through the paper-spec block, sourced by `paper_spec_bootstrap.R:7`:
- `boot_band()` - Median and 90% percentile band (p05/p95) of the finite draws
- `im_critical()` - Imbens-Manski (2004) critical value, interpolating between the
  two-sided normal quantile at width 0 and the one-sided quantile as the set widens
- `pbvn_le_ge()` - Bivariate-normal orthant probability used by the Stoye
  calibration
- `stoye_critical()` - Stoye (2009)-style joint calibration, used when the endpoint
  correlation is estimable (otherwise the Imbens-Manski interpolation is the
  fallback)
- `robust_scale()`, `robust_endpoint_cor()` - MAD-based endpoint scale and endpoint
  correlation
- `boot_min_reps()` - Minimum usable replication count
- `endpoint_inference()` - Per-coefficient interval for the true coefficient,
  anchored at the identified-set endpoints themselves rather than at a percentile of
  the draws (the endpoints come from the profile/functional solves, so the
  `profile_bounds_core.R:96-99` caveat above applies to them)
- `point_inference()` - Robust two-sided interval for the tau = 0 point draws

### set_id_bootstrap_core.R
Core machinery for the set-identification endpoint bootstrap, parameterized by a spec
list so drivers stay thin and the pieces are testable on synthetic systems. Statuses
are decoupled: a rank-deficient tau = 0 system yields an NA point without discarding
the draw's endpoint evaluations (`:1-10`). No numbered stage sources this file today;
`tests/test_set_id_bootstrap_core.R` exercises it, and `scripts-paper` owns an
independent implementation:
- `estimate_set_id_system()` - Re-estimate the mean-equation system on one data frame
  (W1/W2 residualizations, the de-meaned instrument, the moments, the closed-form
  tau = 0 point); shared by the full-sample and per-draw callers so their recipes
  cannot drift apart
- `set_id_boot_draw()` - One bootstrap draw's re-estimation
- `set_id_boot_collect()` - Collect draws
- `set_id_boot_diagnostics()` - Diagnostics table

## Paper specification (stage 08)

### paper_spec_residuals.R
Collapse the per-maturity SDF news into ONE principal component (Y2, I = 1) and align
the single de-meaned VFCI instrument (Z, J = 1):
- `compute_paper_spec_residuals()` - Reuses `compute_identification_residuals()` for
  all date alignment; the news factor is the RAW (uncentered) PC1 projection
  (`news_mat %*% combo`), NOT prcomp scores, so the reduced-form intercept and `b2R`
  stay correct with no intercept correction (an internal `.reconcile_maturities()`
  matches the news / residual / `beta2R` objects by maturity name)

### paper_spec_estimator.R
From the I = 1 / J = 1 residuals, assemble everything the three `for_paper` tables
need:
- `compute_paper_spec_estimator()` - The theta identified set, the `beta1(theta)`
  structural intervals at the baseline slack `tau_set` (default `BASELINE_TAU`,
  `:25`; each per-coefficient interval recovered via the linear functional
  `beta1_p(theta) = beta1r_p - beta2r_p . theta`, assembled into `coef_table`), tau*,
  the heteroskedasticity-relevance tests of W2 on the VFCI instrument, the relevance
  diagnostics that replace the vacuous kappa(Q) at I = J = 1, the endogeneity
  correlation, and a single exogenous-Y2 OLS benchmark. The returned members are
  `theta_ols` (the Y2 coefficient of that OLS fit) and `ols_r2` (`:124-139`);
  `set_status` is one of `point` / `interval` / `unbounded` / `unreliable`
  (`.classify_set_status()`, `:12-22`)

### paper_spec_bootstrap.R
- `compute_paper_spec_bootstrap()` - Moving-block bootstrap band (`block = 15L`,
  `b_reps = 200L`, deterministic given `SEED`; `:9-10`): resamples quarters via
  `mbb_index()`, recomputes the moments under the fixed unit gamma, and re-derives
  tau*, the set endpoints at `tau_set`, and the relevance moment Cov(Z, W2^2)
  (`:44-49`), summarizing each with `boot_band()`

### for_paper_guard.R
Fail-closed invariant for `output/for_paper/` (exactly the three paper-spec tables,
no aux, no subdirs):
- `for_paper_allowlist()` - The 12 exact basenames permitted
  (`FOR_PAPER_TABLE_STEMS` x {`.tex`, `_standalone.tex`, `_standalone.pdf`, `.csv`};
  `:7-13`)
- `assert_for_paper_allowlist()` - Abort if `for_paper` has any unexpected or missing
  file, or any subdir (`:19-32`; `all.files = TRUE` catches hidden files, and
  `missing` is checked so a permissive setdiff cannot pass when a table is absent).
  Run both to validate the stage-08 staging dir before publishing
  (`scripts/08_paper_spec/render_paper_tables.R:61`) and to re-check `for_paper` at the end
  of the pipeline (`scripts/run_all_scripts.R:87`)

## tests/

Unit tests for the utility layer (28 files). There is no runner; each is a standalone
script meant to be run from the package root, e.g.:

```sh
Rscript scripts/utils/tests/test_profile_bounds.R
```

They cover: lambda packing/whitening/varnorm/optimization (`test_lambda_mask.R`,
`test_lambda_mask_validation.R`, `test_lambda_whitening*.R` (5 files),
`test_lambda_varnorm.R`, `test_lambda_optimization.R`), profile bounds
(`test_profile_bounds.R`), I×J identification (`test_ixj_identification.R`), closure
membership (`test_closure_membership.R`), hetero tests (`test_hetero_test_utils.R`,
`test_hetero_lm_tests.R`), gamma sources (`test_gamma_source.R`), the news-projection
mode (`test_news_projection.R`, `test_impose_b_zero.R`), the Stage-04 baseline spec
stamp (`test_baseline_spec.R`), residual alignment and saved-residual dates
(`test_identification_alignment.R`, `test_saved_residuals_dates.R`), the Z-width
pipeline (`test_z_width_pipeline.R`), the generalized-vs-legacy pipeline equivalence
(`test_pipeline_equivalence.R`, which toggles `HETID_ASSERT_EQUIV`), the tau* utils
(`test_tau_star_utils.R`), set-identification inference and its bootstrap core
(`test_set_id_inference.R`, `test_set_id_bootstrap_core.R`), the stage-08 paper-spec
residuals/estimator (`test_paper_spec_residuals.R`, `test_paper_spec_estimator.R`),
and stats (`test_stats_utils.R`). `fixtures/` holds the capture scripts and their
saved RDS.

## Usage

Sourcing `common_settings.R` loads the core utility layer; scripts that need one of
the four directly-sourced helpers (e.g. `ixj_identification.R`,
`spec_comparison_eval.R`) source it in addition. To pull in the core layer from a new
script:

```r
source(here::here("scripts/utils/common_settings.R"))
```

Sourcing it also loads the installed `hetid` package and the heavy script-only
dependencies (`common_settings.R:5-12`), and creates the `scripts/output/` directory
skeleton (`:25-32`).
