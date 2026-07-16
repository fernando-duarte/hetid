# Paper-owned support modules

This directory makes the paper pipeline source-independent of the numbered `scripts/`
pipeline. The numbered pipeline retains its implementations under `scripts/utils/`; the
paper pipeline owns the modules below and sources only files inside `scripts-paper/`.

The copies are intentionally independent. Changes in one pipeline do not propagate
automatically and must be ported deliberately when both implementations should evolve.

| Numbered-pipeline copy | Paper-owned module |
|---|---|
| `identification_utils.R` | `identification/{inputs_and_alignment,residual_construction,quadratic_system}.R` via `identification/api.R` |
| `profile_bounds_core.R` | `identification/profile_solver_core.R` |
| `profile_bounds.R` | `identification/{coordinate_bounds,functional_bounds}.R` via `identification/profile_bounds_api.R` |
| `tau_star_utils.R` | `identification/tau_star.R` |
| `set_id_bootstrap_core.R` | `identification/identified_set_bootstrap.R` |
| `set_id_inference.R` | `identification/identified_set_inference.R` |
| `stats_utils.R` | `statistics/{bootstrap_and_stationarity,reporting_and_validation}.R` via `statistics/api.R` |
| `latex_table_utils.R` | `latex/table_pipeline.R` |
| `latex_simple_table.R` | `latex/simple_table.R` |
| `hetero_test_utils.R` | `diagnostics/heteroskedasticity_tests.R` |
| `hetero_lm_tests.R` | `diagnostics/identification_diagnostics.R` |

The three source files that exceeded the paper tree's 200-line limit were split at function
boundaries. Their API files preserve the original definition order and global symbols.
