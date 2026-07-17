# Paper-owned support modules

This directory contains the support modules owned and sourced by the paper pipeline. The
former numbered analysis pipeline has been deleted; there is no live sibling implementation
to synchronize.

| Paper-owned module | Responsibility |
|---|---|
| `identification/api.R` | Input alignment, residual construction, and quadratic assembly |
| `identification/profile_solver_core.R` | Scaled profile optimization primitives |
| `identification/profile_bounds_api.R` | Coordinate and functional bound facade |
| `identification/tau_star.R` | Fixed-gamma bounded/unbounded sweep and threshold search |
| `identification/identified_set_bootstrap.R` | Identified-set bootstrap draws |
| `identification/identified_set_inference.R` | Endpoint inference and calibration |
| `statistics/api.R` | Bootstrap, stationarity, reporting, and validation helpers |
| `latex/table_pipeline.R` | Shared LaTeX table writing and compilation |
| `latex/simple_table.R` | Simple table builder |
| `diagnostics/heteroskedasticity_tests.R` | Heteroskedasticity test implementations |
| `diagnostics/identification_diagnostics.R` | Identification diagnostic implementations |

The three source files that exceeded the paper tree's 200-line limit were split at function
boundaries. Their API files preserve the original definition order and global symbols.
