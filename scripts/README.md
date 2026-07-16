# Scripts Directory Structure

_Last modified: 2026-07-16 16:22 EDT_

This directory contains the analysis pipeline for the hetid package, organized by
workflow stage. It is NOT part of the R package (see `.Rbuildignore`, `.lintr`,
`.pre-commit-config.yaml`): it may use heavy dependencies, and it is excluded from
lintr and R CMD check. All outputs are written under `scripts/output/` for
self-contained reproducibility.

Every claim below is tied to code. Citations are `file:line`, and paths resolve
against this `scripts/` directory unless they begin with `R/`, `inst/`, or `docs/`
(which resolve against the repository root). Inside a bullet describing one file, a
bare `:NN` refers to that file. A citation points at the relevant call, whose
arguments may run onto the following lines. Re-check any of them directly rather than
trusting this file.

## News Clock

The identification pipeline runs on a quarterly news clock (`NEWS_STEP <- 3L`,
`utils/common_settings.R:41`): one news period equals one quarter, matching the
quarterly rows of the stage-01 dataset, so the rows-equal-period contract of the
SDF-news construction holds exactly. Stage 01 therefore carries maturities at
3-month spacing (`PIPELINE_ACM_MATURITIES <- seq(3L, 120L, by = 3L)`,
`utils/common_settings.R:42` — 40 maturities), so that a horizon `i` has its
step-adjacent neighbors `i - 3` and `i + 3` on the grid. The top of the grid has no
`i + 3` neighbor, so stage 03 filters to the effective maximum
(`03_variance_bounds/compute_variance_bounds.R:27-31`).

That 3-month spacing is a pipeline choice, not a limit of the data: the bundled
ACM series is a full monthly grid (`inst/extdata/ACMTermPremium_replicated_monthly_1m_120m.csv.gz`;
`HETID_CONSTANTS$MIN_MATURITY = 1L` and `ALL_ACM_MATURITIES = 1:120`,
`R/constants.R:77,89`). Only `01_data_analysis/create_data.R:8` reads `PIPELINE_ACM_MATURITIES`;
the rest of stage 01 takes whatever grid it wrote to `data.rds` by matching column
names.

The stage-03 variance bounds run on the same quarterly clock:
`03_variance_bounds/compute_variance_bounds.R:67-69` passes `step = NEWS_STEP`
explicitly, which resolves the realized leg of
`compute_k_hat()` to the 3-month yield `y3` (`R/compute_k_hat.R:68-70`, `:94`).
This is worth knowing because the package default is `DEFAULT_STEP = 12L`
(`R/compute_k_hat.R:54`) — the quarterly clock holds only because the call site
passes `step`.

One diagnostic deliberately stays on the annual clock: the stage-02 n-hat episode
analysis, which validates an inherently 1-year-ahead prediction. It reads *monthly*
ACM rows (`02_identification_diagnostics/n_hat_episodes.R:29`,
`frequency = "monthly"`) but takes the annual maturity nodes and annual step by
default (`DEFAULT_ACM_MATURITIES = seq(12, 120, 12)`, `DEFAULT_STEP = 12L`), and
`compute_n_hat(..., i = 12)` at
`02_identification_diagnostics/n_hat_episodes.R:34` passes no `step`. Monthly rows,
annual horizon: the two are different axes.

## Sample

`01_data_analysis/create_data.R:90-91` keeps only complete cases over every column it retains (date,
consumption growth, `pc1..pc6`, `vfci`) and merges them into the ACM panel with an
inner join (`:93`). Any NA in any of those columns therefore shortens the sample.
In the current data vintage the column that binds is `vfci`, which reaches the
bundled `variables` dataset a quarter or so behind the other series: its newest rows
are NA, so **the sample ends at vfci's last finite quarter — for every stage, not
just the paper spec that reads the column**. (A carried NA could not survive anyway:
an instrument matrix must be finite on every panel row —
`R/build_instrument_matrix.R:48`.)

`01_data_analysis/create_data.R:42-63` guards the vfci case specifically. Missing
`vfci` is tolerated only as a trailing publication-lag block: an interior NA aborts
(`:46-51`), and a
trailing gap longer than `MAX_VFCI_LAG_QUARTERS <- 4L` (`:42`) aborts as a broken
upstream import (`:52-57`). Otherwise it warns and names the resulting sample end
(`:58-62`). Note the guard only aborts or warns — the trim itself is the
complete-case filter above.

As of this file's timestamp the consolidated dataset is **254 quarterly rows,
1962-06-30 to 2025-09-30**. The end is set by `vfci` alone: ACM quarterly data runs
to 2026-06-30 and `variables` to 2025-12-31, but `vfci`'s last finite quarter is
2025-09-30. The start is set by `variables` (1962-03-31) plus the drop of the first
row, which has NA PC lags (`01_data_analysis/create_data.R:107`) — not by ACM, which begins
1961-06-30. These figures move with the data vintage. Reproduce them from the source
data (no pipeline run required) with:

```r
Rscript -e 'suppressMessages(library(hetid)); suppressMessages(library(dplyr))
  acm <- extract_acm_data(maturities = seq(3L, 120L, by = 3L), frequency = "quarterly")
  data("variables", package = "hetid")
  variables$date <- hetid::to_period_end(variables$date, "quarterly")
  keep <- c("date", HETID_CONSTANTS$CONSUMPTION_GROWTH_COL, paste0("pc", 1:6), "vfci")
  v <- variables[, keep] |> (\(x) x[complete.cases(x), ])()
  d <- merge(data.frame(date = acm$date), v, by = "date")[-1, ]
  cat(nrow(d), "rows:", format(min(d$date)), "to", format(max(d$date)), "\n")'
```

## Directory Structure

### 01_data_analysis/
Data preparation and exploratory analysis. Listed in pipeline order
(`run_all_stage_list.R:17-32`) — note `visualize_raw_data.R` runs before
`time_series_properties.R`.
- `create_data.R` - Load ACM data at `PIPELINE_ACM_MATURITIES` via
  `extract_acm_data(frequency = "quarterly")` (`:10-13`), normalize the bundled
  `variables` dates to the period-end convention with `hetid::to_period_end()`
  (`:24`), guard `vfci` (see "Sample" above), merge by date (`:93`), append lagged
  PCs `l.pc1`..`l.pc6` (`:99-104`), and write the consolidated dataset to
  `output/temp/data.rds` (`DATA_RDS_PATH`, `:165-167`). The PCs are **not**
  standardized: they stay on their native asset-return PCA scale, because rescaling
  them would break `PC %*% gamma_vfci == VFCI - mean(VFCI)` (`:109-118`). The saved
  object is a list, not a data frame (`:121`).
- `summary_statistics.R` - Descriptive statistics (mean, SD, quantiles, skewness,
  kurtosis, autocorrelation) and correlation matrices for yields, term premia, PCs,
  and consumption growth. Writes `output/temp/summary_stats/`:
  `summary_statistics.csv` (`:129`), `correlation_matrices.rds` (`:132`)
- `visualize_raw_data.R` - Exploratory SVG plots of raw data (yield series,
  term-structure snapshots, term premia, PCs, distributions, correlation heatmaps,
  rolling statistics). Writes 15 SVGs to `output/temp/plots/` (`:37-273`)
- `time_series_properties.R` - Unit-root (ADF, KPSS, PP), serial correlation
  (Ljung-Box, ARCH), normality (Jarque-Bera), and heteroskedasticity tests across
  variables, with diagnostic plots and HTML summaries. Writes
  `output/temp/time_series_properties/`: 2 HTML (`:171`, `:523`), 2 CSV
  (`:536-537`), and SVG diagnostic plots

### 02_identification_diagnostics/
Diagnostics of the identification-relevant objects (W2 residuals and n-hat).
- `heteroskedasticity_tests.R` - Test the heteroskedasticity *relevance* condition
  that Lewbel identification needs — that Var(W2 | Z) actually moves with the
  instruments. (The identifying restriction itself is not testable; see
  `08_paper_spec/paper_spec_tables.R:251-257`.) The skedastic suite is regime-aware:
  `select_diagnostics_suite()`
  (`utils/hetero_lm_tests.R:56-69`) picks Regime B (White/BP/GQ/Harvey/Anscombe)
  when the w2-on-Z refit's fitted values carry genuine variation, else Regime A
  (White/BP/GQ/Harvey, dropping the 0/0-degenerate Anscombe). Cook-Weisberg is in
  neither suite. Added outside that suite: Glejser (`:69`), BP LM (`:73`), ARCH(1)
  (`:74`), and squared-residual/instrument correlations (`:137`). Emits a p-value
  *profile* plot (`-log10(p)` vs maturity, a line-and-point chart —
  `utils/hetero_diag_figures.R:17-27`) and a correlation *heatmap*
  (`utils/hetero_diag_figures.R:41`)
- `n_hat_episodes.R` - Detect positive n-hat episodes on monthly ACM data (`:29`),
  map them to crisis/QE event windows, and validate the implied one-year-ahead
  short-rate prediction (the 1-year lead is hard-asserted at `:140-147`); quarterly
  cross-check
- `output_results.R` - Export the diagnostics panel: LaTeX fragment +
  `_standalone.tex` (`:72`) and HTML mirrors (`:100`, `:109`), and copy 4 PNGs into
  a `figures/` subdir (`:139`). It writes no CSV and compiles no PDF — the stage's
  CSVs come from the two compute scripts
  (`02_identification_diagnostics/heteroskedasticity_tests.R:191,193`;
  `02_identification_diagnostics/n_hat_episodes.R:253-255`)

Everything in this stage lands under `output/temp/identification_diagnostics/`.
`OUTPUT_PAPER_DIR` appears nowhere in this stage; the local variable named
`paper_dir` (`02_identification_diagnostics/output_results.R:15`) is bound to
`OUTPUT_TEMP_DIR`, not to `for_paper`.

### 03_variance_bounds/
Variance bound calculations for identification.
- `compute_variance_bounds.R` - Compute the leading-term variance bounds across
  maturities from plug-in `c_hat`, `k_hat`, and `k2_hat` estimators (`:67-69`; the
  bound is `(1/4) * c_hat * (k_hat + k2_hat)`, `:71-78`); assess monotonicity
  (`:144-153`) and component correlations (`:155-178`, which cover `c_hat`/`k_hat`/
  the bound, not `k2_hat`)
- `analyze_bounds.R` - Analyze computed bounds: by-maturity (`:29-44`), component
  (`:60-80`), and log-scale (`:92-101`) plots; growth rates (`:115`); Spearman trend
  tests (`:127`); component dominance (`:221-241`)
- `output_results.R` - Export variance-bound tables as HTML (`:81`, `:115`, `:143`),
  a PNG conditional on `webshot2` being installed (`:84-87`), LaTeX
  `variance_bounds_table.tex` (`:206`), `variance_bounds_data.csv` (`:148`), an RDS
  (`:268`), a text summary (`:303`), and copied figures (`:321-325`)

All stage-03 output goes to `output/temp/variance_bounds/`. Note that this script's
`output_paper_dir` and `output_temp_dir` (`:21-22`) resolve to the *same* path, so
each of its CSV/RDS/text exports is written twice to one destination.

### 04_identification_without_optimization/
Baseline identified set using fixed PC weights (VFCI gamma) and fixed tau values.
- `compute_identification.R` - Compute the baseline identified set from fixed gamma
  and tau (tau=0 point ID; tau=`BASELINE_TAU` set ID; `:50-52`), and stamp the
  resolved baseline `spec` — Y1 lags, news-projection mode, Z source — into the
  saved RDS via `current_baseline_spec()` (`:147-156`). Writes
  `output/temp/identification_baseline/`: `baseline_identification_results.rds`
  (`:191`), `baseline_bounds_tau0.csv` (`:196`), `baseline_bounds_tau_set.csv`
  (`:202`), `baseline_membership_tau_set.csv` (`:208`)
- `analyze_identification.R` - Compare tau=0 vs set-ID bounds (`:55-72`), width
  summaries (`:28-37`), and min-eigenvalue/quadratic diagnostics (`:101-110`), with
  plots (`:152`, `:166`)
- `output_results.R` - Export baseline identification results: HTML (`:77`) and
  LaTeX (`:84`) to `output/temp/identification/`, CSV (`:92`) and a text summary
  (`:164`) to `output/temp/identification_baseline/`
- `compute_identification_ixj.R` - Compute the I×J (separate-instrument) identified
  set: one quadratic constraint per (component, instrument) pair (`:52-54`) across
  the tau grid `c(0.05, 0.1, 0.5, OPT_TAU_CAP)` (`:17`). Runs last in stage 04,
  after `output_results.R` (`run_all_stage_list.R:86-99`). Sources
  `utils/ixj_identification.R` directly (`:13`)

### 05_identification_with_optimization/
Identification under optimized instrument weights: search gamma to minimize total
identified-set width (tau held fixed). The search is a seeded multistart **local**
heuristic, not a certified global minimizer, and the resulting widths are a
computational benchmark rather than a confidence statement
(`utils/lambda_optimization.R:10-14`).
- `optimize_identification.R` - Search PC loadings (gamma) for minimum total set
  width via multi-start (`n_starts_gamma <- 30L`, `:54`) SLSQP
  (`nloptr::slsqp`, `utils/lambda_optimization.R:124`) under variance normalization
  (`whiten = list(z = z_aligned)`, `:66` — required, no default:
  `utils/lambda_optimization.R:75-81`). Writes
  `output/temp/identification_optimized/`:
  `optimized_identification_results.rds` (`:190`), `optimization_trace.csv`
  (`:198`), `optimized_membership.csv` (`:204`)
- `analyze_optimization.R` - Compare baseline vs optimized widths, objective values,
  and gamma-similarity metrics, with plots (`:178`, `:192`, `:195-207`)
- `output_results.R` - Export optimized identification results: HTML (`:84`) and
  LaTeX (`:92`) to `output/temp/identification/`; `optimized_identification_table.csv`
  (`:100`), `optimized_gamma_matrix.csv` (`:118`), and a text summary
  (`05_identification_with_optimization/output_results_summary_section.R:66-69` —
  its in-place helper, sourced at `:120`) to `output/temp/identification_optimized/`
- `tau_star_comparison.R` - Compute identification strength via tau* (the slack
  where the set transitions bounded→unbounded) across gamma choices — VFCI/baseline
  (`:75-82`), reduced-form rank-N (`:95-104`; the rank is computed per run, and this
  arm is skipped entirely under imposed exact news, where the PC slope block of
  `beta2R` is all zeros and the reduced-form gamma is undefined — `:86-104`),
  optimized (`:117-120`) — using a
  coarse grid (`COARSE_TAUS <- seq(0, 0.2, by = 0.005)`, `:28`), bisection
  (`BISECT_ITERS <- 40L`, `:30`), and a fine grid (`FINE_N <- 10L`, `:29`). Tagged
  `full_only` in the stage list (`run_all_stage_list.R:129`): it runs only on the
  full run (`HETID_FULL_RUN=1`), not the default quick run
- `tau_star_report.R` - Generate the tau* report (also `full_only`); orchestrates
  tables/sweeps/summary (`:34-63`) and sources its three helpers (`:15-17`), none of
  which is a stage:
  - `tau_star_report_figures.R` - overlay and VFCI blow-up figures
  - `tau_star_report_text.R` - narrative prose
  - `tau_star_report_utils.R` - cap-aware formatters. Shared, not report-only:
    `05_identification_with_optimization/tau_star_comparison.R:24-26` sources it too
- `spec_comparison.R` - Resumable, parallel specification comparison across
  (n_pcs, components, gamma, tau) cells. Resumable via per-group checkpoints
  (`:45`, `:48-50`, `:69-72`, `:81`); a failed group writes no checkpoint, so a
  re-launch retries it (`:73-79`). Parallel via
  `parallel::mclapply(..., mc.cores = N_CORES)` (`:101-104`), the unit being the
  `(n_pcs, components)` group (`:88-90`), with gamma×tau looped inside
  (`utils/spec_comparison_eval.R:90-144`). The gamma scheme is drawn from
  `vfci` / `optimized` / `separate` (the I×J scheme;
  `SPEC_SCHEME_LEVELS`,
  `05_identification_with_optimization/spec_comparison_report_utils.R:7`). Honors
  `HETID_SPEC_QUICK` (`:30`) to select the quick subgrid.
  `spec_comparison_design.R` is the single source of truth for the full/quick grid
  profiles (`spec_comparison_design()` selector, `:25-27`;
  `spec_comparison_design_cells()` enumerator, `:33-54`)
- `spec_comparison_report.R` - Generate the spec-comparison report from the grid. It
  orchestrates, and writes the one CSV itself (`:69-72`). It sources
  `spec_comparison_design.R` (`:22`, needed for coverage classification) plus five
  helpers (`:23-27`), none of which is a stage:
  - `spec_comparison_report_utils.R` - labels, outcome classification, coverage
  - `spec_comparison_report_stats.R` - aggregation
  - `spec_comparison_report_artifacts.R` - gt HTML, LaTeX, and the booktabs panel +
    standalone + PDF (the PDF is conditional on `latexmk`, `:99-110`). It writes no
    CSV
  - `spec_comparison_report_figures.R` - plots
  - `spec_comparison_report_text.R` - narrative prose

In this stage, `components` is a set of bond maturities **in months**
(`DEFAULT_ID_MATURITIES <- c(24L, 60L, 108L)`, `utils/identification_utils.R:3-4`).

### 06_results_production/
Publication-ready outputs assembled from stages 03-05. Despite the name — and
despite a local variable called `paper_dir` — everything here lands in
`output/temp/identification*/`; `OUTPUT_PAPER_DIR` appears nowhere in this stage.
- `assemble_results.R` - Merge baseline, optimized, and variance-bounds artifacts
  into one final comparison object → `output/temp/identification_results/`:
  `final_identification_results.rds` (`:184-190`),
  `final_identification_comparison.csv` (`:192-199`)
- `create_tables_and_figures.R` - Build publication tables and figures: the main
  interval table and the optimized-gamma appendix as HTML+LaTeX (`:83`, `:109`);
  `create_figures_section.R` is its in-place figure helper (sourced at `:112`) and
  emits the interval plot (`:57`), width-reduction chart (`:93`), and gamma heatmap
  (`:122`), each as PNG+SVG
- `create_theta_panel_table.R` - Build the theta identified-set and
  optimized-loadings panel table (LaTeX fragment + standalone, `:106-110`)
- `create_consumption_equation_table.R` - Build the consumption-growth equation
  structural-coefficient table: recovers `beta1(theta)` via the exported
  `recover_structural_coefficients()` (`beta1(theta) = beta1R - (beta2R)'theta`)
  and tabulates a point-ID column (theta from the reduced-form gamma at tau=0 — this
  column is skipped under imposed exact news, where the reduced-form gamma is
  undefined, and is routed through the point-unreliable path instead, `:111-120`)
  beside a set-ID column (optimized gamma at tau=`BASELINE_TAU`), where each beta1
  coefficient is an interval over Theta computed by `solve_linear_functional_bound`;
  LaTeX fragment + standalone (`:346-350`). Two caveats the code states about these
  intervals: `beta1R` and `beta2R` come from independent full-sample fits while the
  recovery identity assumes a common sample, a pre-existing approximation (`:81-87`);
  and the solver certifies feasibility and an active constraint, not global
  optimality (`utils/profile_bounds_core.R:96-99`)
- `output_results.R` - Write final human-readable summaries and stable
  machine-readable exports: RDS (`:34-37`), CSV (`:40-64`), text (`:227-232`)

### 07_generalized_instruments/
Generalized-instrument identified set on Z = PC^2 (squared principal components). A
demonstration of the exported generalized API on a non-default instrument set; not a
paper deliverable.
- `compute_generalized_identification.R` - Build Z = PC^2 **inline** from the
  aligned PCs via the exported
  `build_instrument_matrix(resid$pcs_aligned, transforms = list(sq = function(z) z^2),
  include_original = FALSE)` (`:22-26`) — it does not go through the
  `HETID_Z_SOURCE` hook, and does not use `z_sources/pc_squared.R`. It assembles
  identity-weight (separate-instrument) quadratic systems with
  `build_general_quadratic_system` at tau=`BASELINE_TAU` (`:37-39`) and tau=0
  (`:40-42`), solves the identified set, and runs a closure membership probe over a
  theta grid via `probe_set_membership` (`:56`), which exercises the exported
  `make_system_checker` / `make_constraint_checker` closures. Because the squaring is
  applied to whatever Z the residual step produced, this stage assumes
  `HETID_Z_SOURCE` is unset so the structural first stage keeps the default level-PC
  residuals and only the instrument role is squared (`:15-16`); the assumption is
  documented rather than checked at run time
- `output_results.R` - Export the labeled identification table and membership
  summary to `output/temp/identification_generalized/` (`:28-29`, `:56-57`), plus a
  second copy of the table CSV into `output/temp/identification/` (`:30-32`)

### 08_paper_spec/
The PAPER specification -- the ONLY thing written to `output/for_paper/`. One single,
clean instance of the identification: the SDF news collapsed to a single principal
component (`I = 1`), the actual de-meaned VFCI as a single instrument (`J = 1`, via
`z_sources/vfci_demeaned.R`), and the common design
`X_t = (1, four return PCs, four consumption-growth lags)`. Produces exactly three
self-contained tables (summary statistics; the structural price-of-risk equation;
estimator properties), each as `.tex` / `_standalone.tex` / `_standalone.pdf` /
`.csv`. Both stages run in the quick and full profiles alike (neither is
`full_only`).
- `compute_paper_spec.R` - Build the residuals (news-PC collapse via
  `compute_paper_spec_residuals`), the estimator (`compute_paper_spec_estimator`:
  the theta identified set, beta1(theta) intervals, tau*, the
  heteroskedasticity-relevance tests of W2 on VFCI, and a single exogenous-Y2 OLS
  benchmark -- returned as `theta_ols`/`ols_r2`), and the moving-block bootstrap
  band; save
  `output/temp/paper_spec/paper_spec_results.rds` (`:65`) together with a
  configuration stamp (`current_paper_spec()`, `:20-40`: scaling, n_pcs, y1_lags,
  step, tau_set, instrument, gamma, news maturities, seed, git SHA) that the table
  captions read from the RDS rather than from live env vars. The stamp does not
  record the news-projection mode, which also affects these residuals
  (`utils/identification_utils.R:166`), so it is not a complete record of the run's
  configuration. The VFCI Z-hook is
  scoped to this computation only via `withr::with_envvar` (`:45-46`), which sets
  exactly one variable, `HETID_Z_SOURCE`, and leaks nothing. The news-PC scaling is
  a separate thing: `PAPER_NEWS_PC_SCALE <- "correlation"` (`:15`) is a plain script
  constant, deliberately not an env var. It is flipped in place to `"covariance"` for
  the sensitivity run — but note the table captions hard-code the correlation-PCA
  wording and its pooled-standard-deviation units
  (`08_paper_spec/paper_spec_tables.R:66-74`), so a covariance run changes the
  numbers without changing those captions
- `paper_spec_tables.R` - The three table builders (`:32`, `:98`, `:208`), with
  pdfLaTeX-safe self-contained captions whose bottom-line titles are computed from
  the result (the set status — `point`, `interval`, `unbounded`, or `unreliable`, per
  `utils/paper_spec_estimator.R:12-22`; the relevance verdict; tau*). Sourced in
  place by `08_paper_spec/render_paper_tables.R:9`; not a stage
- `render_paper_tables.R` - Render + compile, then publish. Requires `latexmk` and
  aborts without it (`:17-19`). Each table's PDF is compiled in a throwaway
  `-outdir` under `tempdir()` with only the `.pdf` copied back, so no LaTeX aux ever
  reaches the output dir (`:29`, `:36`, `:44`). All 12 files are built into a
  staging dir (`output/temp/paper_spec/for_paper_staging`, `:21`) and validated
  there by `assert_for_paper_allowlist(staging)` (`:61`) **before** anything
  destructive happens; `for_paper` is then replaced wholesale — `unlink()` →
  `dir.create()` → `file.copy()` (`:62-64`) — and re-validated (`:68`)

### utils/
Shared utility functions; see `utils/README.md` for the per-file function lists.
`common_settings.R` sources the core layer through an explicit `file.exists`-guarded
block (not a loop), in two parts: the core utilities (`:124-183`) and then the
stage-08 paper-spec layer (`hetero_lm_tests.R`, `latex_simple_table.R`,
`paper_spec_residuals.R`, `paper_spec_estimator.R`, `paper_spec_bootstrap.R`,
`for_paper_guard.R`; `:188-205`). `set_id_inference.R` arrives with that second
block, transitively — `utils/paper_spec_bootstrap.R:7` sources it.

Four helpers are NOT sourced by `common_settings.R`; the stage scripts that consume
them source them directly:
- `hetero_panel_meta.R` — `02_identification_diagnostics/output_results.R:35`
- `hetero_diag_figures.R` —
  `02_identification_diagnostics/heteroskedasticity_tests.R:157`
- `ixj_identification.R` —
  `04_identification_without_optimization/compute_identification_ixj.R:13` and
  `05_identification_with_optimization/spec_comparison.R:27`
- `spec_comparison_eval.R` — `05_identification_with_optimization/spec_comparison.R:40`
A fifth, `set_id_bootstrap_core.R`, has no consumer in the numbered pipeline today —
only its own test (`utils/tests/test_set_id_bootstrap_core.R:11`) exercises it. (The
spec-comparison grid design, `spec_comparison_design.R`, lives with stage 05, not in
`utils/`.)

`utils/tests/` holds the assertion suites for those helpers. Run them all with
`Rscript scripts/utils/tests/run_tests.R` from the package root: it discovers every
`test_*.R` in that directory, runs each in a fresh R process, and exits non-zero naming
any file that failed. The suites are not a pipeline stage and `run_all_scripts.R` does
not invoke them, so they have to be run deliberately.

### examples/
Standalone demonstrations (not part of the pipeline).
- `custom_z_demo.R` - End-to-end generalized-instrument workflow on a custom Z
  (nonlinear PC transform + volatility); runs offline on the bundled `variables`
  dataset plus synthetic Y2. It writes no files — output is console-only. (It does
  source `common_settings.R` (`:60`), which creates the `output/` directory skeleton
  as a side effect.)

### z_sources/
Drop-in payloads for the `z_source.R` hook. Each defines `build_z(data)`; neither is
sourced statically — a payload is selected at run time by pointing `HETID_Z_SOURCE`
at its path.
- `vfci_demeaned.R` - The de-meaned VFCI as the single instrument (`vfci_dm`,
  `J = 1`; `build_z` at `:16`). The stage-08 paper spec scopes `HETID_Z_SOURCE` to
  this payload via `withr::with_envvar` at `08_paper_spec/compute_paper_spec.R:45-46`
  (the wiring is documented from this end at `:12-15`). De-meaning is
  set-*invariant* because the moments are centered (`:5-10`); it is kept for
  interpretability
- `pc_squared.R` - Squared principal components as instruments (`build_z` at `:9`;
  hard-codes the first four PCs, `:10`). Being outside span(1, PC) is what makes
  Regime B reachable, which is when the stage-02 suite includes Anscombe (`:1-4`);
  the regime itself is still selected empirically from the fitted-SD ratio
  (`utils/hetero_lm_tests.R:56-63`). Nothing in the repo selects
  it today — it is a manual opt-in via `HETID_Z_SOURCE` (in particular, stage 07
  builds its squared Z inline instead)

### results_companion/
Companion diagnostics that back the results write-up
(`docs/run_all_scripts_results.tex:65` points at this directory by name, and
`\includegraphics` the ridge figure at `:499-500`). **Not part of the pipeline** —
none of these appear in `run_all_stage_list.R`, and nothing sources them. Each
recomputes from `output/temp/data.rds` and is deterministic (each calls
`set.seed(SEED)` directly, with `SEED = 123`). Run one directly, e.g.
`Rscript scripts/results_companion/diag_taustar_bootstrap.R`.
- `diag_identified_functional.R` - Which linear combinations `c'theta` are
  identified over the fixed-VFCI set, plus the near-rank-one Q geometry (W2
  cross-maturity correlations `:30`, SVD/condition number `:50-51`, profile bounds
  `:63`, functional bounds `:76-92`) → `temp/results_companion/identified_functional.rds`
  (`:133-141`)
- `diag_single_maturity_and_recovery.R` - Show each maturity alone is bounded and
  well-conditioned, isolating the *joint* collinearity as the cause; plus
  `beta1(theta)` recovery over the set (`:106-115`) →
  `temp/results_companion/single_maturity_and_recovery.rds` (`:123-128`)
- `diag_taustar_bootstrap.R` - tau* for fixed VFCI weights with a moving-block
  bootstrap band (B = 200, block = 15, `:50-51`) →
  `temp/results_companion/taustar_bootstrap.rds` (`:95-101`)
- `make_ridge_figure.R` - The headline figure: the identified set projected on
  (theta_5y, theta_9y) with theta_2y profiled out, at tau=0.02 vs 0.05. Note it
  writes into `temp/identification/` (`:10`, `:115-116`), the same directory stages
  06 and 07 publish to — not into `temp/results_companion/` like its three siblings

### output/
All script outputs organized by purpose. `OUTPUT_DIR` is hardcoded to
`scripts/output` (`utils/common_settings.R:15-18`) — there is no environment
override.

#### for_paper/
The three paper-spec tables -- and NOTHING else. Stage 08 owns this directory (see
`render_paper_tables.R` above), and `assert_for_paper_allowlist()`
(`utils/for_paper_guard.R:15`), run at the end of `run_all_scripts.R:87`, fails the
run if anything outside the allowlist appears, if anything is missing, or if any
subdirectory exists. Every other stage writes to `temp/`. Exactly 12 files
(3 stems x {`.tex`, `_standalone.tex`, `_standalone.pdf`, `.csv`};
`FOR_PAPER_TABLE_STEMS` at `utils/common_settings.R:59-63`, expanded by
`for_paper_allowlist()` at `utils/for_paper_guard.R:7-13`):
- `table1_summary_statistics.*` - Summary statistics of the set-identification
  variables (+ news-PC loadings in the CSV only, `08_paper_spec/paper_spec_tables.R:88-94`)
- `table2_structural_equation.*` - The structural price-of-risk equation: two
  columns -- a single exogenous-Y2 OLS benchmark (`08_paper_spec/paper_spec_tables.R:113-117`) and
  the tau=`BASELINE_TAU` (0.05) identified-set interval for every coefficient
  (`:119-126`), subject to the two caveats noted under stage 06
- `table3_estimator_properties.*` - Heteroskedasticity-relevance tests
  (`08_paper_spec/paper_spec_tables.R:220-222`), tau* with a bootstrap 90% band (`:224`, `:234`),
  and identification diagnostics (`:225-228`)

#### temp/
Working outputs and intermediate results.
- `data.rds` - Consolidated analysis dataset, created by stage 01 and read by all
  later stages
- `summary_stats/`, `time_series_properties/`, `plots/` - Stage-01 artifacts
- `identification_diagnostics/` - Stage 02 (including its `figures/` subdir)
- `variance_bounds/` - Stage 03
- `identification_baseline/` - Stage 04 baseline results
- `identification_ixj/` - Stage 04 I×J results
- `identification_optimized/` - Stage 05 optimized weights, spec-comparison grids
  (and per-group checkpoint dirs), tau* sweeps
- `identification/` - Shared publication-style tables and figures from stages 04-07
  (the `paper_dir` variable in those scripts resolves here)
- `identification_results/` - Stage 06 final comparison
- `identification_generalized/` - Stage 07 generalized-instrument (Z = PC^2) results
- `paper_spec/` - Stage 08 results bundle (`paper_spec_results.rds`) and the
  `for_paper_staging/` dir
- `results_companion/` - Companion diagnostics (not pipeline output)
- `tables/`, `figures/`, `other/` - Created eagerly by `utils/common_settings.R:30-32`

## Top-Level Scripts

- `run_all_scripts.R` - Runs the complete analysis pipeline. Sources
  `run_all_stage_list.R` for the ordered script list (`:61`), then runs each stage
  via the `run_script(path, desc, env)` helper (`:22-59`). For a stage with an `env`
  vector, `run_script()` saves each variable's prior value, sets the stage's env with
  `Sys.setenv` **inside the running process** (`:30`), sources the script, and
  restores the prior state on exit — unsetting variables that were previously unset
  (`:31-40`). So a per-stage switch neither leaks into later stages nor wipes a value
  the caller exported before launching; note the corollary that a stage-list `env`
  entry *overrides* whatever the caller exported, for the duration of that stage.
  Stages are wrapped in `tryCatch` and timed (`:45-56`); after the run it asserts the
  `for_paper` invariant (`:87`) and prints a count of files by type under `temp/`
  (`:97-117`).
- `run_all_stage_list.R` - Defines `scripts_to_run`: the ordered
  `list(path, desc, optional env, optional full_only)` stages that constitute the
  pipeline; edit this to add, remove, or reorder stages. It also sets the run
  profile: `quick_run <- !nzchar(Sys.getenv("HETID_FULL_RUN"))` (`:12`; the default
  is the quick run), and at the bottom it `Filter`s out every stage tagged
  `full_only = TRUE` when `quick_run` is in effect (`:228-233`).

## Run Profiles and Environment Switches

The default `Rscript run_all_scripts.R` is the **quick run**: it runs every stage
except the two `full_only` tau* stages. The complete set of `HETID_*` switches read
anywhere under `scripts/` is below — reproduce the list with:

```bash
grep -rn 'Sys.getenv("HETID_' scripts/ --include='*.R'
```

- `HETID_FULL_RUN` (read at `run_all_stage_list.R:12`) - any non-empty value selects
  the **full run**, which additionally keeps the `full_only = TRUE` stages
  `tau_star_comparison.R` and `tau_star_report.R` (the most expensive part of the
  pipeline: a multi-start optimizer oracle bisected over a tau grid). Unset (the
  default) drops them. It does **not** widen the spec-comparison grid — see the next
  entry.
- `HETID_SPEC_QUICK` (read at
  `05_identification_with_optimization/spec_comparison.R:30`) - selects the quick
  spec-comparison subgrid via `spec_comparison_design("quick")`, whose artifacts
  carry a `quick` token in their basenames (`spec_comparison_quick.rds` etc.);
  otherwise the full grid (`spec_comparison_full.*`) is computed. Relative to the
  full profile, quick shrinks the tau grid to `c(0, 0.05, 0.1)`, `n_pcs` to `4`, and
  the optimizer starts to 6 — the maturity sets are identical in both
  (`05_identification_with_optimization/spec_comparison_design.R:10-23`). **The stage
  list hard-pins it on**
  (`env = c(HETID_SPEC_QUICK = "1")`, `run_all_stage_list.R:145`), and because
  `run_script()` applies that with `Sys.setenv` inside the process
  (`run_all_scripts.R:30`), exporting a different value before launching the pipeline
  does not defeat it. The in-pipeline spec comparison is always the quick subgrid, on
  both the quick and full runs; to compute the full grid, run the stage directly.
- `HETID_SPEC_SOURCE` (read at
  `05_identification_with_optimization/spec_comparison_report.R:35`) - path to the
  saved grid (`.rds`/`.csv`) the spec-comparison report reads instead of recomputing.
  The stage list points it at `spec_comparison_quick.rds`
  (`run_all_stage_list.R:156-159`) so the report refreshes the quick artifacts.
  Unset, the report takes the first that exists of `spec_comparison_full.rds`,
  `spec_comparison_full.csv`, `spec_comparison_quick.rds`, `spec_comparison_quick.csv`
  — i.e. full is preferred over quick, `.rds` over `.csv` — and errors if none exist
  (`:36-48`).
- `HETID_BASELINE_GAMMA` (read at `utils/gamma_source.R:13`, default `"vfci"`) - selects
  the baseline gamma: `"vfci"` (the fixed raw VFCI PC loading, which requires exactly
  4 instruments) or a path to an R file defining `build_gamma(moments)` returning a
  J×I matrix (the arbitrary-width escape hatch). It is read only in
  `baseline_gamma_method()`; the stages that honor it call that helper and pass the
  result to `resolve_baseline_gamma()` —
  `04_identification_without_optimization/compute_identification.R:9,49` and
  `05_identification_with_optimization/tau_star_comparison.R:75-77`.
- `HETID_Z_SOURCE` (read at `utils/z_source.R:25` and `:65`) - path to an R file defining
  `build_z(data)` returning a named numeric T×K instrument matrix; unset, the
  pipeline uses the default level-PC instruments. Both branches funnel through the
  exported `build_instrument_matrix`. It is also read by `utils/baseline_spec.R:23` to
  record the Z source in the stage-04 spec stamp. Stage 08 sets it, scoped, to
  `z_sources/vfci_demeaned.R` (`08_paper_spec/compute_paper_spec.R:45-46`).
- `HETID_IMPOSE_NEWS_PROJECTION_ZERO` (read at `utils/news_projection.R:9` via
  `impose_news_projection_zero()`) - `TRUE`/`1` imposes the exact-news projection
  `B = 0` (residual Y2 = the raw news); `FALSE`/`0` estimates `B` from the data (the
  default, "let the data speak"). Unset, it falls back to the
  `IMPOSE_NEWS_PROJECTION_ZERO` script constant in `utils/common_settings.R:51` (default
  `FALSE`). The resolved value is part of the Stage-04 baseline spec stamp.
- `HETID_ASSERT_EQUIV` (read at `utils/identification_utils.R:275`) - diagnostic shadow
  flag: when non-empty, every `build_pipeline_quadratic_system()` call additionally
  asserts numeric-leaf identity with the legacy `build_quadratic_system()`. Off by
  default (zero overhead).

Examples:

```bash
# From the package root

# Quick run (default): skips the tau* stages, quick spec grid
Rscript scripts/run_all_scripts.R

# Full run: adds the tau* identification-strength stages
HETID_FULL_RUN=1 Rscript scripts/run_all_scripts.R
```

## Workflow

- **Data Preparation**: Start with scripts in `01_data_analysis/` to prepare and
  explore the data
- **Identification Diagnostics**: Run `02_identification_diagnostics/` to test the
  heteroskedasticity relevance condition and interpret n-hat
- **Core Computations**: Run `03_variance_bounds/` for fundamental calculations
- **Identification Analysis**: Execute `04_identification_without_optimization/` for
  baseline results
- **Optimization**: Run `05_identification_with_optimization/` for the
  width-minimizing weight search
- **Results Production**: Use `06_results_production/` to generate final outputs
- **Generalized Instruments**: Run `07_generalized_instruments/` for the
  generalized-instrument (Z = PC^2) demonstration
- **Paper Specification**: Run `08_paper_spec/` to build the three `for_paper` tables

## Running the Scripts

### Prerequisite: the installed package

`utils/common_settings.R:7` does `library(hetid)`, so every script runs against the
**installed** hetid package, not the sources in `R/`. After changing package code,
reinstall before re-running the pipeline (`R CMD INSTALL .`), or the scripts will
keep using the previously installed version. The heavy script-only dependencies are
loaded alongside it (`utils/common_settings.R:5-12`, plus the on-demand loaders
`load_visualization_packages()` / `load_timeseries_packages()` /
`load_web_packages()`).

### Run All Scripts

To run the pipeline end to end (the default is the quick run, which skips the
`full_only` tau* stages -- see "Run Profiles and Environment Switches" above):

```bash
# From the package root

# Quick run (default)
Rscript scripts/run_all_scripts.R

# Full run (adds the tau* identification-strength stages)
HETID_FULL_RUN=1 Rscript scripts/run_all_scripts.R
```

### Run Individual Scripts

Each stage script can also be run individually:

```bash
# Example: Run only summary statistics
Rscript scripts/01_data_analysis/summary_statistics.R
```

This does not apply to the in-place helpers. Two of them read variables straight out
of the caller's environment and are explicitly not standalone:
`06_results_production/create_figures_section.R` (`:1-5`, expects `comparison_table`,
`gamma_mat`, `n_pcs`, `n_components`, `save_plot`, `paper_dir`) and
`05_identification_with_optimization/output_results_summary_section.R` (`:1-5`). The
rest — `08_paper_spec/paper_spec_tables.R` and the `tau_star_report_*` /
`spec_comparison_report_*` helper sets — only define functions for a sibling to call,
so running one on its own does nothing. Order also matters within a stage; see the
stage-02 example below.

### Run the Identification Diagnostics (`02_identification_diagnostics/`)

These scripts read the consolidated dataset that stage 01 writes to
`output/temp/data.rds`, so create it first by running
`01_data_analysis/create_data.R` (or the full pipeline through stage 01). Then run
the three scripts in order: both compute scripts must finish before
`output_results.R`, which loads the `*_results.rds` files they save.

```bash
# From the package root

# Prerequisite: writes output/temp/data.rds
Rscript scripts/01_data_analysis/create_data.R

# Stage 02, in order:
Rscript scripts/02_identification_diagnostics/heteroskedasticity_tests.R
Rscript scripts/02_identification_diagnostics/n_hat_episodes.R
Rscript scripts/02_identification_diagnostics/output_results.R
```

All of it -- RDS objects, CSVs, paired PNG/SVG figures, the LaTeX panel (fragment +
standalone), and the HTML mirrors -- is written under
`output/temp/identification_diagnostics/`.

## Notes

- Instruments and predictors `pc1..pc6` are principal components of **asset
  returns**, never of yields. Yields and term premia enter in exactly one place:
  constructing the SDF news `Y2`. Do not describe the PCs as PCs of yields, and do
  not equate PC1/PC2/PC3 with the level/slope/curvature of the yield curve
  (`08_paper_spec/paper_spec_tables.R:63-64` states the convention).
- All scripts source what they need from `utils/`, normally via
  `common_settings.R`
- Outputs are organized by whether they are the paper deliverable
  (`output/for_paper/`, stage 08 only, exactly three tables) or working artifacts
  (`output/temp/`, everything else)
- Each downstream stage folder (`02`-`07`) contains its own `output_results.R` to
  ensure modularity; stage `01` writes its artifacts directly from its analysis
  scripts, and stage `08` publishes via `render_paper_tables.R`
- Scripts are numbered to indicate the recommended execution order; the authoritative
  order is the list in `run_all_stage_list.R`
- The main runner script (`run_all_scripts.R`) handles dependencies and runs scripts
  in the correct order via that list
- `scripts_old/` is legacy and is not part of this pipeline
