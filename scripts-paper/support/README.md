# Paper-owned support modules

This directory holds the support modules owned and sourced by the paper pipeline
(`scripts-paper/`). The former numbered analysis pipeline was deleted, so there is no live
sibling implementation to keep in sync. Every file is loaded through `paper_source_once()`.
Several files chain-source the split implementation files that sit beside them — for example
`identification/api.R`, `identification/profile_bounds_api.R`, `statistics/api.R`, and
`latex/table_pipeline.R` act as facades — keeping each file within the paper tree's 200-line
limit while preserving the original definition order and global symbols.

## `identification/`

| Module | Responsibility |
|---|---|
| `api.R` | Facade for the active quadratic assembly (sources `quadratic_system.R`) |
| `quadratic_system.R` | Single entry point assembling the identified-set quadratic system via `hetid::build_general_quadratic_system()` (the `K_i = 1` date-t specialization); re-attaches the `hetid_components` class and attributes |
| `quadratic_evaluation.R` | Canonical evaluation of the quadratic inequality systems |
| `scaled_quadratic_program.R` | Generic scaled quadratic-program adapter |
| `profile_solver_core.R` | Non-dimensionalized profile-bounds solver with scale-aware unbounded detection (sources `quadratic_evaluation.R`, `scaled_quadratic_program.R`) |
| `profile_bounds_api.R` | Public profile-bound facade (sources the classifier, coordinate, functional, and linear-objective bound modules) |
| `bound_search_classifier.R` | Shared box-growth classifier for coordinate and linear-functional bounds |
| `coordinate_bounds.R` | Coordinate profile bounds over the quadratic identified set |
| `functional_bounds.R` | Linear-functional and aggregate profile bounds |
| `linear_objective_bounds.R` | Facade adapter for linear objectives over a quadratic set |
| `tau_star.R` | Fixed-gamma bounded/unbounded sweep, bisection, re-optimizing oracle, and recession degeneracy diagnostic for the tau* threshold |
| `identified_set_bootstrap.R` | One-draw re-estimation, draw collection, and diagnostics table for the set-endpoint bootstrap |
| `identified_set_inference.R` | Percentile bands and Stoye (2009) / Imbens-Manski (2004) endpoint confidence intervals (sources `inference_calibration.R`) |
| `inference_calibration.R` | Calibrations and robust (MAD-based) endpoint summaries used by identified-set inference |
| `status_contract.R` | Closed endpoint-state vocabulary and precedence |

## `statistics/`

| Module | Responsibility |
|---|---|
| `api.R` | Facade for statistics helpers (sources `bootstrap_and_stationarity.R`, `mbb_runner.R`, and `reporting_and_validation.R`; `normalizations.R` is sourced directly by its consumers, not here) |
| `bootstrap_and_stationarity.R` | Bootstrap sampling, summary statistics, stationarity tests, and the circular moving-block index with its automatic block-length rule (`paper_mbb_block_len`) |
| `mbb_runner.R` | Deterministic moving-block draw orchestration: indices are drawn up front under a pinned Mersenne-Twister (the caller's RNG kind is restored afterward), then run through a serial loop or chunked `parallel::mclapply`, reporting progress under either |
| `reporting_and_validation.R` | Statistical reporting and data-validation functions |
| `normalizations.R` | Named distributional normalization constants shared by execution and prose |

## `latex/`

| Module | Responsibility |
|---|---|
| `table_pipeline.R` | Booktabs multi-panel bare-tabular builder (plain-math scientific notation, no siunitx; the paper supplies float, caption, and notes) and standalone-document variant (sources `table_environment.R`, `artifact_publication.R`) |
| `table_environment.R` | Shared table/threeparttable environment and notes renderer |
| `artifact_publication.R` | Manifest-directed fragment, standalone-source, and PDF publication |
| `simple_table.R` | Simple booktabs/threeparttable table with plain `l c c ...` columns for non-numeric cells (e.g. interval strings) |

## `reporting/`

| Module | Responsibility |
|---|---|
| `cells.R` | Policy-driven rendering primitives for publication-table cells |
| `inference.R` | Shared significance, row-layout, and Newey-West table helpers |

## `artifacts/`

| Module | Responsibility |
|---|---|
| `typed_artifacts.R` | Shared typed CSV and exact-RDS artifact serialization |
| `diagnostic_schema.R` | Generic typed-row and artifact protocol for diagnostic outputs |

## `diagnostics/`

| Module | Responsibility |
|---|---|
| `heteroskedasticity_tests.R` | Heteroskedasticity testing utilities |
| `identification_diagnostics.R` | LM-style heteroskedasticity tests, the W2 diagnostics NA fallback row, and the joint-relevance rank test |

## `data/`

| Module | Responsibility |
|---|---|
| `acm_inputs.R` | Canonical validated quarterly ACM inputs used by paper computations |

## `runtime/`

| Module | Responsibility |
|---|---|
| `core.R` | Shared serialization, hashing, condition, and evaluation-capture primitives |

## `graphics/`

| Module | Responsibility |
|---|---|
| `device.R` | Fail-safe SVG device lifecycle shared by publication figures |
