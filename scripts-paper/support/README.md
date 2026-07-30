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
| `identified_set_bootstrap_collect.R` | Collects the per-draw bootstrap results into the unified bootstrap stage's endpoint tables (sourced by `scripts-paper/inference/run_bootstrap_stage.R`) |
| `identified_set_inference.R` | Percentile bands and Stoye (2009) / Imbens-Manski (2004) endpoint confidence intervals (sources `inference_calibration.R`) |
| `inference_calibration.R` | Calibrations and robust (MAD-based) endpoint summaries used by identified-set inference |
| `status_contract.R` | Closed endpoint-state vocabulary and precedence |

## `statistics/`

| Module | Responsibility |
|---|---|
| `api.R` | Facade for statistics helpers (sources `bootstrap_and_stationarity.R`, `mbb_protocol_authority.R`, `mbb_rng_state.R`, `mbb_index_family.R`, `mbb_execution_core.R`, `mbb_runner.R`, `boot_freshness.R`, `boot_cache.R`, and `reporting_and_validation.R`; `normalizations.R` is sourced directly by its consumers, not here) |
| `bootstrap_and_stationarity.R` | Bootstrap sampling, summary statistics, stationarity tests, and the circular moving-block index with its automatic block-length rule (`paper_mbb_block_len`) |
| `mbb_protocol_authority.R` | Single source of truth for the moving-block bootstrap protocol (draw count, block rule, RNG kind/seed) shared across index generation and execution |
| `mbb_rng_state.R` | Save/restore of the caller's RNG state around the pinned Mersenne-Twister draw |
| `mbb_index_family.R` | Generation of the circular moving-block index family from the pinned draw |
| `mbb_execution_core.R` | Shared serial/`parallel::mclapply` execution core used by the moving-block runner |
| `mbb_runner.R` | Deterministic moving-block draw orchestration: indices are drawn up front under a pinned Mersenne-Twister (the caller's RNG kind is restored afterward), then run through a serial loop or chunked `parallel::mclapply`, reporting progress under either |
| `reporting_and_validation.R` | Statistical reporting and data-validation functions |
| `normalizations.R` | Named distributional normalization constants shared by execution and prose |
| `boot_freshness.R` | Runtime and executed-source hashes used by unified-stage provenance and cache freshness |
| `boot_cache.R` | Validated atomic replacement for the unified bootstrap cache, including restoration of a prior valid cache after a post-promotion failure |

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

## `inference/`

| Module | Responsibility |
|---|---|
| `bootstrap_stage_execution.R` | Assembles the mean/log-variance collection specs and runs the unified bootstrap stage candidate |
| `bootstrap_stage_result_inputs.R` | Extracts per-estimator set/SE fields from an estimator's results for the stage output tables |
| `bootstrap_stage_mean_result_inputs.R` | Extracts the mean-equation point/set fields feeding the stage's mean tables |
| `bootstrap_stage_result_helpers.R` | Selects the mean/log-variance provenance fields carried into the stage result |
| `bootstrap_stage_provenance.R` | Builds the stage's provenance axes and validates a provenance record against them |
| `bootstrap_stage_provenance_validation.R` | Per-family (design, sample size, seed, RNG kind) index-provenance validation used by cache freshness checks |
| `bootstrap_stage_cache_validation.R` | Generic payload field/class/validator check used by the stage's cache freshness gate |
| `bootstrap_stage_logvar_cache.R` | Volatility set-endpoint bootstrap anchor gate and cache handling |
| `bootstrap_stage_logvar_contract.R` | Enforces the log-variance estimator dependency/complete-case contract and builds per-row inputs |
| `bootstrap_stage_logvar_controls.R` | Validates the volatility PC-preprocessing policy and search-control records stored by the stage owner |
| `bootstrap_stage_mean_cache.R` | Reports failed-draw causes and enforces the mean-equation failure-rate gate |
| `bootstrap_stage_code_manifest.R` | Lists the directories/files whose edits invalidate the primary bootstrap draw cache |
| `bootstrap_stage_spec_assertions.R` | Named-check assertion helper and structured `bootstrap_stage_error` condition constructor |

## `runtime/`

| Module | Responsibility |
|---|---|
| `core.R` | Shared serialization, hashing, condition, and evaluation-capture primitives |

## `graphics/`

| Module | Responsibility |
|---|---|
| `device.R` | Fail-safe SVG device lifecycle shared by publication figures |
