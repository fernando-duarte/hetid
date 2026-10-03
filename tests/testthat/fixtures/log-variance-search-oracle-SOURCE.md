# Log-variance search oracle provenance

2026-10-02 18:41 EDT

These are test-only outputs from frozen macro_dynamics donor revision
`bfc02e940dc80625fa3bc1eff03ff394cec707c6`. They must not be read by production code.

`log-variance-search-oracle.rds` is the unchanged `donor-oracle-v1.rds` captured
before any candidate evaluation. SHA256 `9b5e511d6b60d1157ca74fcd9f901f5f88988f681134f536591756c3238b666d`.
`log-variance-path-oracle.rds` is the unchanged supplemental
`donor-path-oracle-v1.rds`, captured before the first candidate path comparison.
SHA256 `a76a65e7a708e0f040ec4942fc27ba9c9dcf8c8fa7d7d90c5b2b057e70cc6422`.

The first fixture contains a two-dimensional radius-0.5 ball, a linear map with
independently known extrema, a bounded grid/budget engine run, a starved run,
and 48-observation PPML/Harvey/log-OLS responses generated with seed 6102. It
retains inputs, fit records, analytic Jacobians, grid crossings, Morton selection,
PPML pilot and three Harvey recession cases. The second fixture uses the same
fixed data and radius-0.1/radius-0.5 balls labelled tau 0.05/0.1, with grid_n=7,
grid_floor=3, grid caps=30 and primary/coverage/sensitivity fit budgets=1000.
Only the donor's two data-access functions are replaced by those explicit inputs;
estimator, engine, solver, audit, reconciliation and path bodies remain unchanged.

Capture environment: R 4.6.1, `aarch64-apple-darwin25.4.0`, macOS 26.6.2,
OpenBLAS 0.3.34, LAPACK 3.12.1, nloptr 2.2.1. The existing shared nloptr is read
from `/Users/fduarte/Library/R/arm64/4.6/library/nloptr`. The donor resolves hetid
through the initial accepted private installation, whose inventory digest is
`e1cda58d6db2770313b9a033d7c84f1305cfef33ad73f36effbd020937330248`.
The complete `.libPaths` and `sessionInfo` are in the saved capture log.

Exact equality applies to the linear engine result, PPML/Harvey fit/Jacobian and
multi-tau donor core outputs, recession records, selector and pilot. The generic
sample identity intentionally hashes all retained prepared data and replaces the
paper-specific sample label; added domain and selector-traversal fields are tested
separately. The independent analytic ball endpoints allow absolute error 1e-6.
Current hetid log-OLS uses its stable QR projection and log evaluator: point
coefficients/Jacobians allow 1e-10 against the donor, endpoints absolute 1e-6,
with exact labels, statuses, source labels, NA/Inf patterns and signs. Its direct
comparison to the current package evaluator is exact. Finite-difference Jacobian
checks use step 1e-6 and tolerance 1e-5.

Neither fixture refreshes the historical paper pins in
`tests/fixtures/paper_figures/set_figures_pins.csv` of the donor. Those pins have
a separate historical data-vintage and platform contract. Capture sources and
source hashes are retained in the untracked execution packet. Any refresh requires
an explicit independent-oracle decision; never replace expected values from the
candidate implementation.

Cross-platform comparison, 2026-10-03 16:27 EDT, authorized by the package author: the
stored hex bits are unchanged, but computed doubles are now compared through
`expect_oracle_equal()` (tests/testthat/helper-oracle_tolerance.R) rather than
`expect_identical()`, because R CMD check on the CI matrix (macOS, Windows, Ubuntu)
differs from the capture runtime by floating-point noise. A finite double passes when
the absolute gap is at most the tolerance times max(|expected|, 1), with tolerance 1e-8
for closed-form evaluations and fits at a given point and 1e-5 for nloptr search
outputs. Statuses, NA/NaN/Inf patterns and all other non-double fields remain exact.
Expected values are still never regenerated from candidate output.

The search oracles no longer pin the search-effort tallies (`n_attempted`,
`n_evaluated`, `n_cached`, `counters`, `cache_hits`), the labels naming which pass found
an endpoint (`origin`, `lower_source`, `upper_source`) or the fitted-volatility cache
contents. These record the route a search took, which floating-point noise changes
across platforms; the tallies' internal arithmetic is checked on the fresh run instead.

The Harvey `rcond_info` diagnostic is the LAPACK 1-norm condition estimate, whose search
steps move it by up to 0.03 across platforms; it is checked separately with absolute
tolerance 0.1.
