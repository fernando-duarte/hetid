# Paper-owned support modules

This directory contains shared helpers for the paper pipeline. Run the analysis through
[`../run_pipeline.R`](../run_pipeline.R); see the [pipeline README](../README.md) for
configuration, execution order, artifacts, and tests. These helpers are paper-owned code,
separate from the R package API. They call the installed `hetid` package for identification,
estimation, and bootstrap calibration.

Consumers load R modules through `paper_source_once()`. Facades such as
`identification/api.R`, `identification/profile_bounds_api.R`, and `statistics/api.R` load
their implementation files in dependency order.
`latex/table_pipeline.R` also loads its publication and environment helpers. Support files
are not separate pipeline entrypoints.

The unified bootstrap stage distinguishes draw code from deterministic post-draw summaries.
`inference/bootstrap_stage_code_manifest.R` defines both source inventories: draw-code edits
invalidate its cache; edits under `inference_post/` update the presentation hash without
discarding draws.

## `identification/`

| Module | Responsibility |
|---|---|
| `api.R` | Loads `quadratic_system.R` and `profile_evidence.R` for active quadratic assembly and geometry evidence |
| `quadratic_system.R` | Single entry point assembling the identified-set quadratic system via `hetid::build_general_quadratic_system()` (the `K_i = 1` date-t specialization); re-attaches the `hetid_components` class and attributes |
| `quadratic_evaluation.R` | Canonical evaluation of the quadratic inequality systems |
| `scaled_quadratic_program.R` | Generic scaled quadratic-program adapter |
| `profile_solver_core.R` | Non-dimensionalized profile-bound candidate solver (sources `quadratic_evaluation.R`, `scaled_quadratic_program.R`) |
| `profile_bounds_api.R` | Loads the classifier, coordinate, functional, and linear-objective bound modules; requires `profile_solver_core.R` loaded first |
| `bound_search_classifier.R` | Accepts coordinate and linear-functional endpoints only with the corresponding geometry evidence and checked member points |
| `profile_evidence.R` | Adapter to `hetid::compute_quadratic_set_evidence()`, including bounded candidate correction toward checked points |
| `profile_point_pool.R` | Shares checked points across eligible objective sides |
| `containing_box.R` | Produces and validates the containing bounds used by search domains and residual-crossing screens |
| `coefficient_interval_tables.R` | Builds coordinate and structural endpoint tables from shared geometry evidence |
| `widen_beta1_from_args.R` | Widens eligible structural coefficient bounds using the structural images of checked member points |
| `coordinate_bounds.R` | Coordinate profile bounds over the quadratic identified set |
| `functional_bounds.R` | Linear-functional and aggregate profile bounds |
| `linear_objective_bounds.R` | Facade adapter for linear objectives over a quadratic set |
| `tau_star.R` | Fixed-gamma geometry sweep, re-optimizing oracle, and recession degeneracy diagnostic |
| `tau_star_bracket.R` | Bisection with explicit bounded, unbounded, unresolved and capped brackets for the tau* threshold |
| `identified_set_bootstrap.R` | One-draw re-estimation, draw collection, and diagnostics table for the set-endpoint bootstrap |
| `identified_set_bootstrap_collect.R` | Collects the per-draw bootstrap results into the unified bootstrap stage's endpoint tables (sourced by `scripts-paper/inference/run_bootstrap_stage.R`) |
| `status_contract.R` | Closed endpoint-state vocabulary and precedence |

## `statistics/`

| Module | Responsibility |
|---|---|
| `api.R` | Loads the bootstrap, stationarity, reporting, and cache helpers listed below; consumers load `normalizations.R` directly |
| `bootstrap_and_stationarity.R` | Bootstrap sampling, summary statistics, stationarity tests, and the circular moving-block index with its automatic block-length rule (`paper_mbb_block_len`) |
| `mbb_protocol_authority.R` | Owns the circular moving-block rule, RNG kind, index-family names, and provenance schema shared across generation and execution |
| `mbb_rng_state.R` | Save/restore of the caller's RNG state around the pinned Mersenne-Twister draw |
| `mbb_index_family.R` | Generation of the circular moving-block index family from the pinned draw |
| `mbb_execution_core.R` | Shared serial/`parallel::mclapply` execution core used by the moving-block runner |
| `mbb_runner.R` | Executes a pre-generated index family serially or in parallel, installs its recorded RNG state, restores the caller's state afterward, and reports progress |
| `reporting_and_validation.R` | Statistical reporting and data-validation functions |
| `normalizations.R` | Named distributional normalization constants shared by execution and prose |
| `boot_freshness.R` | Runtime and executed-source hashes used by unified-stage provenance and cache freshness |
| `boot_cache.R` | Validated atomic replacement for the unified bootstrap cache, including restoration of a prior valid cache after a post-promotion failure |

## `inference_post/`

These modules summarize stored draws. Published endpoint calibration delegates to
`hetid::bootstrap_set_interval()`; the paper retains its inference controls, publication
gates, and diagnostic schema. Stoye and Imbens-Manski normal-theory calibrations are
diagnostic cross-checks, not the source of published confidence intervals.

| Module | Responsibility |
|---|---|
| `identified_set_inference.R` | Percentile summaries and the contract-owned minimum valid repetition count; loads `inference_calibration.R` |
| `inference_calibration.R` | Robust MAD scales, trimmed endpoint correlations, and normal-theory critical values for diagnostics |
| `endpoint_targets.R` | Prepares named coefficient axes for the package endpoint API |
| `endpoint_target_cells.R` | Builds published pointwise endpoint cells through the package API and enforces the paper's calibration and feasibility gates |
| `endpoint_alternatives.R` | Normal, percentile, and basic interval alternatives computed from the same draws for diagnostics only |
| `endpoint_point_statistic.R` | Delegates tau = 0 point statistics and their normal and empirical p-values to `hetid::bootstrap_point_statistics()` |
| `logvar_point_summaries.R` | Log-variance tau = 0 point summaries and bootstrap-versus-analytic standard-error diagnostics |
| `set_id_diagnostics_rows.R` | Tau = 0 diagnostics rows, display-schema padding, and normal-theory calibration cross-checks |

## `latex/`

| Module | Responsibility |
|---|---|
| `table_pipeline.R` | Generic booktabs panel fragments and standalone documents; compiles PDFs and enforces sidecar cleanup (loads `table_environment.R`, `dropbox_ignore.R`, and `artifact_publication.R`) |
| `table_environment.R` | Shared table/threeparttable environment and notes renderer |
| `artifact_publication.R` | Manifest-directed fragment, standalone-source, and PDF publication |
| `simple_table.R` | Simple booktabs/threeparttable table with plain `l c c ...` columns for non-numeric cells (e.g. interval strings) |
| `overleaf_scaffold.R` | Shared manuscript table fonts, column headers, and booktabs rule scaffolds |
| `overleaf_panel_table.R` | Shared inference-panel layout, coefficient blocks, and summary rows |
| `structural_var_inference.R` | Fills the decimal-aligned combined structural-inference table using the adjacent `structural_var_inference_template.tex`, and refuses unprintable cells by name |
| `dropbox_ignore.R` | Best-effort macOS File Provider ignore flags for generated files; sidecar removal remains enforced by the compilation helper |

The combined structural-inference fragment contains the table body and layout; its
manuscript float supplies the caption and notes. Estimator pages include their own table
environments, captions, and notes. Standalone sources wrap the supplied content for PDF
inspection.

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
| `identification_diagnostics.R` | LM-style heteroskedasticity tests, the $\omega_2$ diagnostics NA fallback row, and the joint-relevance rank test |

## `data/`

| Module | Responsibility |
|---|---|
| `acm_inputs.R` | Canonical validated quarterly ACM inputs used by paper computations |
| `frozen_inputs.R` | Verifies the consumption snapshot and pinned ACM input before conditional artifact cleanup |

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
| `bounds_axis.R` | Shared bounds-by-tau display cap; changes the plotted viewport while retaining every sampled row in data and artifacts |
| `latex_labels.R` | Typesets labels with `latex` and `dvisvgm`, then places glyph outlines in finished SVGs |
