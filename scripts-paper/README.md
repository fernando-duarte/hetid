# Paper analysis pipeline

`scripts-paper/` is the reproducible analysis used to build the paper's consumption-growth
mean equation, identified sets, log-variance estimators, diagnostics, tables, figures, and
descriptive report. It is separate from the R package, and its R source graph is entirely
contained within `scripts-paper/`. Run it only from the package root.

The paper pipeline's sole entrypoint is:

```sh
Rscript scripts-paper/run_pipeline.R
```

The separate package quality suite lives under `docs/` and is not part of the paper source
graph. Run it from the package root:

```sh
Rscript docs/quality-check.R
```

There are intentionally no compatibility wrappers for the former flat source paths or
entrypoint.

## Module tree

```text
scripts-paper/
├── config/                 paths, contracts, artifact registry/lifecycle, decisions
├── data_preparation/       FRED patch and construction of all analysis series
├── mean_equation/
│   ├── inference/          bootstrap and bounds-by-tau inference
│   ├── variance_shares/    share definitions, computation, and tables
│   ├── figures/            projections and three-dimensional region rendering
│   ├── tables/             structural-equation inference table renderer
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
├── variance_bounds/        per-maturity SDF-news and expected-SDF variance bounds figure, table, and quoted-numbers note
├── reports/                descriptive-statistics report builder
├── support/                paper-owned identification, statistics, LaTeX, reporting, runtime, and diagnostics helpers
├── tests/                  isolated suites, topology checks, and comparison support
└── run_pipeline.R          ordered source orchestrator
```

The support layer contains paper-owned implementations. See `support/README.md` for the
current module catalog.

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
  -> log-variance set bootstrap via the shared circular-MBB runner (Mersenne-Twister, HETID_BOOT_CORES)
  -> inference panels, analytical figures, diagnostics, and descriptive report
```

Modules still evaluate in the shared global environment. Important products include
`set_id_mean_eq`, `set_id_boot`, `mean_eq_bounds_tau`, `var_share`, `log_var_eq`,
`log_var_eq_ppml`, `log_var_eq_harvey`, conditional `log_var_eq_lad`, the estimator
registry `logvar_bounds_tau_registry`, `log_var_eq_set_boot`, joint-diagnostic objects,
dynamics-gate records, fitted-volatility envelopes, and the final table/report objects.
Their names and serialized schemas are part of the pipeline contract.

Production dependencies are loaded through `paper_source_once()`; direct
`source(paper_path(...))` calls are rejected by the topology check.

## Gates and decisions

Three distinct decision points guard the conditional log-variance workstreams. They are
not interchangeable:

- **Joint-GMM: always-run diagnostic, not a run/skip gate.** `config/decisions/joint_gmm.R`
  is the committed no-answer-default record; `log_variance/diagnostics/joint_gmm/run.R`
  always evaluates the moment-specification and graph-replication checks and writes its
  artifacts. Nothing downstream is skipped based on it.
- **Residual-dynamics diagnostic: always runs.** The base-R Ljung-Box screen on the
  `tau = 0` benchmark residual (`log_variance/diagnostics/dynamics/run_gate.R`) always runs,
  uses base R only, and writes both the gate record and the always-present status manifest.
  It sources no dynamic estimator; it only supplies the gate verdict.
- **EGARCH: routing/status gate.** `config/decisions/egarch.R` binds a scientific SHA-256 of
  the freshly regenerated dynamics-gate record, plus the sample id, gate fields, plan hashes,
  and recorded prompt hashes. The router (`log_variance/extensions/egarch/run_route.R`)
  validates that record against the fresh gate, routes every ladder branch, and rewrites the
  status manifest. On the committed non-rejecting gate it closes the dynamic workstream
  without sourcing any extension estimator.
- **LAD: the one actually-gated executable optional estimator.** The tri-state DCF
  `config/decisions/lad.dcf` is read by `log_variance/estimators/lad/dependency_gate.R`; the
  median (LAD) set map is sourced only when the decision is `approved` and the installed
  `quantreg` version matches the recorded `quantreg_version`. A missing, declined, or
  unanswered decision sources no LAD code and adds no registry stub; an approved decision
  with `quantreg` absent or version-mismatched hard-fails in the reader.

Do not weaken or bypass these scientific and dependency gates. The conditional-artifact
cleanup runs before routing (for both the LAD and EGARCH statuses) so stale conditional
outputs cannot look like current results.

## Generated artifacts

`config/artifact_manifest_data.R` is the artifact metadata owner;
`config/artifacts.R` exposes its query API, and
`config/artifact_lifecycle.R` owns conditional cleanup/status. The runner creates missing
directories and preserves existing output except for conditional artifacts, which are
removed before routing to prevent stale results. Writers and readers use these typed roots:

```text
scripts-paper/output/
├── tables/       fragments, standalone TeX, and standalone table PDFs
├── figures/      analytical and descriptive figures
├── reports/      descriptive report TeX and PDF, quoted-numbers markdown note
├── diagnostics/  inference, joint-diagnostic, LAD, EGARCH, and quoted-numbers diagnostics
└── state/        bootstrap draws, gate/status records, and conditional pilot state
```

Descriptive component tables and figures have their own subdirectories. LaTeX sidecars and
`.DS_Store` files are not artifacts.

## Prerequisites

Use an installed `hetid` package and the analysis dependencies already required by the
paper scripts, including the tidyverse/time-series packages, `nloptr`, `skedastic`,
`ggplot2`, and `sandwich`. LAD additionally requires the approved `quantreg`
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
HETID_BOOT_REPS=10000 HETID_BOOT_CORES=1 Rscript scripts-paper/run_pipeline.R
```

`HETID_BOOT_REPS` (default 10000; overriding it prints a message naming the value used) sets
the replication count for both the mean-equation endpoint bootstrap and the log-variance set
bootstrap. Both are seeded at 20260708 and resample a circular moving block whose length
follows the rule `ceiling(1.5 * T^(1/3))` (10 quarters at T = 256), and both parallelize
through `HETID_BOOT_CORES` (default `detectCores() - 1`) via the shared MBB runner, which
pins Mersenne-Twister for the draw and restores whatever generator kind was active
beforehand. The log-variance set bootstrap additionally reruns at the full replication count
with a doubled block length as a sensitivity check.

The resampling indices are drawn once, up front, under the pinned seed, so they are
identical at any core count — `HETID_BOOT_CORES` changes runtime, not which observations are
resampled. Whether the reported numbers also match at every core count further depends on
the draw callback itself consuming no additional randomness; `mbb_checks.R` tests that
directly rather than relying on index determinism alone.

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
