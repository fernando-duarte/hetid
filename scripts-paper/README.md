# Paper analysis pipeline

`scripts-paper/` is the reproducible analysis used to build the paper's consumption-growth
mean equation, identified sets, log-variance estimators, diagnostics, tables, figures, and
descriptive report. It is separate from both the R package and the numbered `scripts/`
pipeline. Its R source graph is entirely contained within `scripts-paper/`; it never sources
the sibling `scripts/` tree. Run it only from the package root.

The sole entrypoint is:

```sh
Rscript scripts-paper/run_pipeline.R
```

There are intentionally no compatibility wrappers for the former flat source paths or
entrypoint.

## Module tree

```text
scripts-paper/
├── config/                 paths, artifact manifest, analysis settings, decisions
├── data_preparation/       FRED patch and construction of all analysis series
├── mean_equation/
│   ├── inference/          bootstrap and bounds-by-tau inference
│   ├── variance_shares/    share definitions, computation, and tables
│   ├── figures/            projections and three-dimensional region rendering
│   ├── tables/             structural-equation table and caption
│   └── diagnostics/heteroskedasticity/
├── log_variance/
│   ├── core/               residual map and endpoint polishing
│   ├── engine/             estimator-neutral scan and endpoint engine
│   ├── estimators/         log-OLS, PPML, Harvey, and gated LAD implementations
│   ├── diagnostics/        joint-null, joint-GMM, and dynamics diagnostics
│   ├── extensions/egarch/  gated EGARCH decision, cleanup, and routing
│   ├── inference/          set bootstrap, envelopes, and standard-error utilities
│   ├── figures/            bounds and fitted-volatility figures
│   └── tables/             estimator panels, notes, and renderers
├── variance_bounds/        per-maturity SDF-news variance bounds figure and table
├── reports/                descriptive-statistics report builder
├── support/                paper-owned identification, statistics, LaTeX, and diagnostics
├── tests/                  isolated suites, topology checks, and comparison support
└── run_pipeline.R          ordered source orchestrator
```

The support layer contains independent paper-owned implementations. The numbered pipeline
retains its own copies under `scripts/utils/`; neither pipeline sources the other's copy.
See `support/README.md` for the source-to-module extraction map.

## Dependency flow

The runner preserves the established source order:

```text
FRED patch and data construction
  -> mean-equation OLS, identified set, bootstrap, tables, and variance shares
  -> log-OLS foundation and mean-equation bounds
  -> PPML and Harvey sets, standard errors, and tables
  -> joint-null and joint-GMM diagnostics
  -> EGARCH cleanup, residual-dynamics gate, decision validation, and routing
  -> optional LAD estimator and table
  -> combined panels
  -> L'Ecuyer-CMRG log-variance bootstrap bracket and RNG restoration
  -> inference panels, analytical figures, diagnostics, and descriptive report
```

Modules still evaluate in the shared global environment. Important products include
`set_id_mean_eq`, `set_id_boot`, `mean_eq_bounds_tau`, `var_share`, `log_var_eq`,
`log_var_eq_ppml`, `log_var_eq_harvey`, conditional `log_var_eq_lad`, the estimator
registry `logvar_bounds_tau_registry`, `log_var_eq_set_boot`, joint-diagnostic objects,
dynamics-gate records, fitted-volatility envelopes, and the final table/report objects.
Their names and serialized schemas are part of the pipeline contract.

## Gates and decisions

- `config/decisions/joint_gmm.R` records the committed joint-GMM scope decision.
- The base-R residual-dynamics diagnostic always runs and writes its gate record.
- `config/decisions/egarch.R` is validated against the fresh gate and pinned protocol
  hashes. The EGARCH router does not activate an estimator after a non-rejecting gate.
- LAD runs only when the external decision DCF approves it and the installed `quantreg`
  version matches the recorded version.

Do not weaken or bypass these scientific and dependency gates. The EGARCH cleanup runs
before the gate so stale conditional artifacts cannot look like current results.

## Generated artifacts

`config/artifacts.R` is the authoritative artifact registry. The runner creates missing
directories and preserves existing output except for the four conditional EGARCH artifacts,
which the pre-gate cleanup removes to prevent stale results. Writers and readers use these
typed roots:

```text
scripts-paper/output/
├── tables/       fragments, standalone TeX, and standalone table PDFs
├── figures/      analytical and descriptive figures
├── reports/      descriptive report TeX and PDF
├── diagnostics/  inference, joint-diagnostic, LAD, and EGARCH diagnostics
└── state/        bootstrap draws, gate/status records, and conditional pilot state
```

Descriptive component tables and figures have their own subdirectories. LaTeX sidecars,
`.DS_Store`, and the obsolete `set_id_region.json` are not artifacts.

## Prerequisites

Use an installed `hetid` package and the analysis dependencies already required by the
paper scripts, including the tidyverse/time-series packages, `nloptr`, `skedastic`,
`ggplot2`, `gt`, and `sandwich`. LAD additionally requires the approved `quantreg`
version. A working LaTeX installation with `latexmk` is required for standalone tables and
the descriptive report. The daily ACM asset must be available in the package cache or be
downloadable; FRED access is needed for fresh data pulls.

## Commands

Run a deterministic quick pipeline:

```sh
HETID_BOOT_REPS=8 HETID_BOOT_CORES=1 Rscript scripts-paper/run_pipeline.R
```

Run the full pipeline serially:

```sh
HETID_BOOT_REPS=200 HETID_BOOT_CORES=1 Rscript scripts-paper/run_pipeline.R
```

Run topology checks and all isolated paper suites:

```sh
Rscript scripts-paper/tests/run_tests.R
```

## Inactive and test-support modules

- `log_variance/tables/legacy_log_ols_caption.R` retains the inactive legacy log-OLS
  caption beside the table code.
- `log_variance/estimators/lad/offline_refinement.R` is an offline refinement sourced only
  by its owning test.
- `log_variance/figures/bounds_by_tau_test_support.R` is test support for bounds plot data
  and is not production-reachable.

These exclusions are documented in the topology audit; do not wire them into production
without an explicit scientific change.
