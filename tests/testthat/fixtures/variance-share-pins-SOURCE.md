# Variance share fixture provenance

2026-10-02 19:30 EDT

Fixture: tests/testthat/fixtures/variance-share-pins.csv.
Exact original header and all 399 original synthetic_seed3/26/34 row bytes
from frozen M tests/fixtures/paper_variance_shares/pins.csv. Source SHA256
`d50aed5f14b7e9a9180646acfa7772df925cbc196c472664c16fdc0bc79b70e3`. Subset SHA256 `c73780d2587b906775717a78028d93dcad00eaf438e0f56bda1ca8ef5f00f255`.
No recomputation or reformatting. Original SOURCE.txt attributes the expected values
to unmodified hetid scripts `b74ee9e4ec6767ee825f5bc07d3b97d35e1ad36d`, package layer
`f022c4b703101bc02fffc0679ade45c484d8d634`, 2026-09-29, R4.6.1/nloptr2.2.1/OpenBLAS0.3.34.
Live unchanged-M/current-private-H run passed every selected pin before product
implementation; see shares-donor-baseline.R/log/command and full RDS.
This original independent expected-value provenance is distinct from current-H
regression evidence and does not imply the historical renv runtime was launched.

Cross-platform comparison, 2026-10-03 16:27 EDT, authorized by the package author: the
stored hex bits are unchanged, but computed doubles are now compared through
`expect_oracle_equal()` (tests/testthat/helper-oracle_tolerance.R) rather than
`expect_identical()`, because R CMD check on the CI matrix (macOS, Windows, Ubuntu)
differs from the capture runtime by floating-point noise. A finite double passes when
the absolute gap is at most the tolerance times max(|expected|, 1), with tolerance 1e-8
for closed-form evaluations and fits at a given point and 1e-5 for nloptr search
outputs. Statuses, NA/NaN/Inf patterns and all other non-double fields remain exact.
Expected values are still never regenerated from candidate output.
