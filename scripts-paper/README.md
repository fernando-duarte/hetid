# Paper analysis pipeline

`scripts-paper/` builds the paper's consumption-growth mean equation, identified sets,
log-variance estimates, diagnostics, tables, figures, and descriptive report. Its source
modules live entirely under this directory and call the installed `hetid` package for
shared estimation and inference primitives.

Run every command below from the `hetid` package root. The analysis entrypoint is
[run_pipeline.R](run_pipeline.R); individual production modules depend on its source order
and shared objects.

## Run the pipeline

A production run uses 10,000 bootstrap draws and validates existing caches before reuse:

```sh
Rscript scripts-paper/run_pipeline.R
```

To use one worker explicitly:

```sh
HETID_BOOT_CORES=1 Rscript scripts-paper/run_pipeline.R
```

The runner overwrites publication artifacts in `scripts-paper/output/`, including tracked
TeX tables and SVG figures. Use a separate worktree for experimental settings or input
changes. A reduced-bootstrap run requires explicit acknowledgment:

```sh
HETID_BOOT_REPS=8 HETID_BOOT_CORES=1 HETID_ALLOW_DRAFT_RUN=1 \
  Rscript scripts-paper/run_pipeline.R
```

This reduces bootstrap depth, not the full-sample estimation work. Its output is draft
output. Without the acknowledgment, any draw count other than 10,000 stops before output
is created or cleaned.

| Environment variable | Default | Effect |
|---|---|---|
| `HETID_BOOT_REPS` | `10000` | Draw count for the unified bootstrap stage and the mean-specification comparison; integer at least 2. |
| `HETID_BOOT_CORES` | Available logical cores minus 2 on macOS, minus 1 elsewhere; minimum 1 | Worker count; integer at least 1. Set 1 for serial execution. |
| `HETID_BOOT_MODE` | `reuse` | Reuse a valid draw cache; `rerun` forces the stage to recompute. |
| `HETID_ALLOW_DRAFT_RUN` | Unset | Set to `1` to acknowledge output overwrites at a non-production draw count. |

### Prerequisites and inputs

Use an installed `hetid` built from the intended package revision. The pipeline loads the
installed namespace; editing `R/` alone does not update it. Analysis dependencies include
`dplyr`, `tidyr`, `purrr`, `tibble`, `tsibble`, `tidyquant`, `ggplot2`, `svglite`, `nloptr`,
`sandwich`, `skedastic`, `moments`, `tseries`, `urca`, and `knitr`. The FRED transport uses
`quantmod`, system `curl`, and `jsonlite` when an API key is supplied. Standalone table PDFs
and the descriptive report require a working LaTeX installation with `latexmk`. Region
figure labels also require `latex` and `dvisvgm` on `PATH`.

[config/analysis.R](config/analysis.R) selects the input vintages and sample window:

- Consumption defaults to `fred_source = "frozen"`, which reads the committed
  [data/pcecc96.csv](data/pcecc96.csv). Selecting `"live"` pulls the current FRED vintage and
  rewrites that snapshot. A frozen run never silently falls back to live consumption data.
- Daily ACM data defaults to `acm_daily_source = "frozen"`, using the exact release tag and
  SHA-256 recorded in the configuration. Preflight verifies the pinned user-cache snapshot;
  it can copy a matching existing asset or download the pinned release when needed.
- Quarterly ACM data uses the package's monthly data source, with automatic download
  disabled. A fresh installation can use the bundled monthly asset; an existing user cache
  takes precedence. This input is separate from the daily release pin.
- The configured analysis window is 1962 Q1–2025 Q4. Series are joined by quarter. The SDF
  panels supply expected-SDF and SDF-news components; the separate lagged return regressors
  use bundled principal components of **nominal financial asset returns**.

Frozen consumption and daily-ACM verification run before output creation or conditional
cleanup. With valid cached inputs, neither requires a fresh network pull. LAD also requires
the exact approved `quantreg` version in [config/decisions/lad.dcf](config/decisions/lad.dcf).

## Configuration and module layout

Scientific settings are declared in source, rather than command-line flags:

| Owner | Settings |
|---|---|
| [config/analysis.R](config/analysis.R) | Input vintages, sample window, bootstrap settings, and specification plan. |
| [config/analysis_contract.R](config/analysis_contract.R) | Model axes, slack grids, preprocessing, inference targets, and active instrument. |
| [config/instrument_choices.R](config/instrument_choices.R) | Available heteroskedasticity instruments and their descriptions. |
| [config/inference_search_control.R](config/inference_search_control.R) | Numerical search, fitting, and bootstrap budgets. |
| [config/reporting.R](config/reporting.R) | Standard-error choices, significance levels, precision, and table style. |
| [config/logvar_estimators.R](config/logvar_estimators.R) | Estimator identities, capabilities, dependencies, and artifact assignments. |
| [config/artifact_manifest_data.R](config/artifact_manifest_data.R) | Artifact paths, producers, consumers, and lifecycle statuses. |

The current specification plan publishes B (estimated `beta2R`) for both equations and
computes A (`beta2R = 0`) for the mean-equation comparison. Only the first mean specification
is published; the volatility specification must match it. The active heteroskedasticity
instrument is `y60_vol_log`, the de-meaned log of realized quarterly five-year yield
volatility. Changing the instrument also changes the dynamics-gate evidence bound by the
committed EGARCH decision; see the instructions in
[tools/regen_egarch_decision.R](tools/regen_egarch_decision.R) before changing that record.

```text
scripts-paper/
├── config/             scientific, computational, reporting, and artifact contracts
├── data/               committed consumption snapshot
├── data_preparation/   consumption, SDF panels, yield volatility, and component series
├── mean_equation/      OLS, identified sets, variance shares, inference, and figures
├── log_variance/       estimator engine, estimators, diagnostics, tables, and figures
├── inference/          unified mean/volatility bootstrap stage
├── variance_bounds/    SDF variance bounds and quoted approximation-error numbers
├── reports/            descriptive-statistics report builder
├── support/            paper-owned shared calculations and publication helpers
├── tests/              isolated suites and structural checks
├── validation/         read-only comparison of final TeX table numbers and stars
├── tools/              standalone scientific-configuration utilities
├── output/             generated artifacts
├── reset_pipeline_state.R
└── run_pipeline.R
```

See [support/README.md](support/README.md) for the shared module catalog and
[validation/README.md](validation/README.md) for the table-comparison contract.

## Execution order and inference

The entrypoint preserves this dependency order:

```text
Input verification and conditional-output cleanup
  -> data preparation
  -> mean OLS, identified sets, variance shares, and log-OLS foundation
  -> mean bounds-by-tau, PPML/Harvey sets, analytic SEs, regularized log-OLS sets,
     and residual diagnostics
  -> joint-null, joint-GMM, and residual-dynamics diagnostics
  -> EGARCH decision validation/routing and approved LAD estimation
  -> unified mean/volatility bootstrap and mean-specification comparison
  -> combined mean-over-PPML table from the stage's results
  -> estimator pages, variance-panel fragments, and the tuning appendix table
  -> bounds, fitted-volatility, region, and heteroskedasticity exhibits
  -> SDF variance bounds, quoted-number checks, and descriptive report
```

Modules share the global environment. Core results include `set_id_mean_eq`, `set_id_boot`,
`mean_eq_bounds_tau`, `var_share`, `log_var_eq`, `log_var_eq_ppml`, `log_var_eq_harvey`,
conditional `log_var_eq_lad`, and `log_var_eq_set_boot`. Their names and serialized schemas
are part of the pipeline contract. Production dependencies load through
`paper_source_once()`; topology checks reject direct `source(paper_path(...))` calls.

### Combined mean-over-PPML table

[log_variance/tables/render_combined_inference_table.R](log_variance/tables/render_combined_inference_table.R)
produces `output/tables/structural_var_inference.tex` after the unified bootstrap stage. It
holds the PPML estimator page's two panels, from the same builders and the same stage draws,
in the paper's decimal-aligned template. That template prints finite numbers only, so a
withheld statistic, a status word or a half-infinite set stops publication with the cell
named. The stage has already saved its validated cache by then, so a refusal costs no draws.

### Unified bootstrap and estimator pages

[inference/run_bootstrap_stage.R](inference/run_bootstrap_stage.R) shares each primary
circular moving-block resample and its system estimate between mean and volatility
inference. A second family doubles the block length for volatility sensitivity checks.
The seed is `20260708`; the primary block rule is `ceiling(1.5 * T^(1/3))`, giving 10
quarters at `T = 256`. Indices are generated before callbacks under a pinned RNG kind, so
worker count does not change the resampled observations. The indexed runner restores the
caller's RNG state afterward.

The package supplies circular indices, endpoint calibration, and point-statistic
summaries. Paper adapters own parallel execution, status handling, stability gates,
pointwise Target P intervals, and publication. The alternative mean specification uses
the same primary index family in a separate comparison calculation after the unified
stage; those extra draws are not stored in its cache.

[log_variance/tables/render_estimator_pages.R](log_variance/tables/render_estimator_pages.R)
publishes `structural_var_estimators.tex`, with a repeated mean panel above each variance
estimator. Publication follows the bootstrap because PPML, Harvey, log-OLS, and the two
regularized log-OLS projections report a bootstrap `tau = 0` statistic. PPML, Harvey, and
projection estimates and analytic SEs are computed before the stage and remain unchanged
by it. LAD reports neither that variance statistic nor `tau > 0` variance confidence
intervals; its page appears only when LAD ran. The regularized projections (additive
threshold and two-pass Fuller) report the same inference as PPML and Harvey (analytic
HAC SEs, the bootstrap `tau = 0` statistic, and `tau > 0` intervals at `m = 1`); their
tuning sensitivity, without intervals, is in `log_var_eq_log_projection_tuning.tex`.

### Cache reuse

`HETID_BOOT_MODE=reuse` validates the stage's cache. A missing, unreadable, malformed, or
stale cache triggers the complete calculation.

| Cache under `output/state/` | Reuse checks |
|---|---|
| `bootstrap_stage_draws.rds` | Stored index families, canonical inputs and draw specification, draw-code/runtime identities, payload, and schema. |

The unified cache records presentation-code hashes for audit but does not use them to
invalidate draws. It rebuilds result objects from accepted cached draws using the current
presentation code. Its temporary cache is validated before atomic promotion, with recovery
of a valid prior cache if validation after promotion fails.

## Endpoint geometry and conditional stages

Coordinate and structural bounds use `hetid::compute_quadratic_set_evidence()`. Checked
tail directions establish infinite sides; positive definite combinations of quadratic
constraints establish boundedness. An unsuccessful search leaves the side unresolved.
Finite reported endpoints are numerical approximations at checked feasible points.

Theta tables distinguish attained `set_lower`/`set_upper` from containing
`outer_lower`/`outer_upper`. The latter define volatility and variance-share search domains,
region frames, and residual-crossing exclusions. Their rounding margins and checked
candidate weights are numerical safeguards, rather than interval-arithmetic proofs.
Bounded geometry can coexist with an unresolved endpoint. The volatility engine requires
valid attained coordinate sides as well as finite containing bounds. Tau-star output
retains a bracket and status; sweep limits use the bounded lower endpoint, never an
unresolved midpoint. The current run computes these values instead of using a recorded
historical plotting cap.

Conditional stages have distinct roles:

- **Joint-GMM** always runs its moment-specification and graph-replication diagnostics.
  [config/decisions/joint_gmm.R](config/decisions/joint_gmm.R) records the no-answer default;
  the diagnostic does not skip downstream work.
- **Residual dynamics** always runs a base-R Ljung-Box screen on the `tau = 0` residual and
  writes gate/status records. It sources no dynamic estimator.
- **EGARCH** validates the committed decision against freshly regenerated gate evidence
  and routes the workstream. The current decision records `non_reject`. The production
  graph contains no EGARCH estimator producer, so routing permission alone never establishes
  execution; the final conditional-status record retains `egarch_producer_ran = FALSE`.
- **LAD** runs only when the DCF decision is `approved` and the installed `quantreg` version
  matches its record (currently `6.1`). Missing, declined, or unanswered decisions skip LAD
  without adding an estimator stub. An approved decision with a missing or mismatched
  dependency stops the run.

The runner removes manifest-defined LAD and EGARCH conditional artifacts before routing
so stale outputs cannot represent the new run. Preserve these scientific and dependency
gates when changing settings.

## Generated artifacts and reset

The artifact manifest supplies paths and lifecycle status; `config/artifacts.R` exposes
queries and `config/artifact_lifecycle.R` owns cleanup. Output uses these roots:

```text
scripts-paper/output/
├── tables/       TeX fragments, standalone TeX, and table PDFs
├── figures/      analytical figures and descriptive-statistics components
├── reports/      descriptive report TeX/PDF and quoted-numbers Markdown note
├── diagnostics/  inference, specification, gate, and quoted-number diagnostics
└── state/        draw caches, gate/status records, and conditional pilot state
```

Descriptive tables and figures use dedicated subdirectories. The runner creates missing
output directories, overwrites produced artifacts, and cleans LaTeX sidecars at completion.
It does not clear draw caches before attempting reuse.

Reset is a separate, destructive operation, rather than a prerequisite for a normal run:

```sh
Rscript scripts-paper/reset_pipeline_state.R --keep-tracked
```

This clears the draw cache, other state, diagnostics, ignored table/figure/report
artifacts, and LaTeX sidecars. Omitting `--keep-tracked` also deletes the manifest's tracked
publication outputs. The reset covers registered artifacts, not arbitrary files in
`output/`. It does not change scientific configuration or input snapshots.

## Checks and cross-run comparison

Run the isolated paper suites plus topology and contract-ownership checks:

```sh
Rscript scripts-paper/tests/run_tests.R
```

Compare the displayed numbers and significance stars in two existing output trees:

```sh
Rscript --vanilla scripts-paper/validation/compare_output_tables.R \
  path/to/reference/scripts-paper/output \
  path/to/candidate/scripts-paper/output
```

This reads `.tex` files under each `tables/` directory and does not run the pipeline.
See [validation/README.md](validation/README.md) for precision rules, parser limits, and
what a passing comparison establishes.

The package quality suite is separate from the paper pipeline. In checkouts that contain
its local script, run `Rscript docs/quality-check.R` from the package root. Package tests
use `Rscript -e 'devtools::test()'`.

`log_variance/figures/bounds_by_tau_test_support.R` is test support and remains outside
the production source graph. The reset, validation command, and `tools/` utilities are
standalone commands rather than analysis stages.
