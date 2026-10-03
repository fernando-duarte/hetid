# Mean profile fixture provenance

2026-10-02 17:19 EDT

Proposed target: tests/testthat/fixtures/mean-profile-pins.csv.

The CSV retains the original header and original row bytes from macro_dynamics
tests/fixtures/paper_variance_shares/pins.csv at `bfc02e940dc80625fa3bc1eff03ff394cec707c6`.
Source SHA256: `d50aed5f14b7e9a9180646acfa7772df925cbc196c472664c16fdc0bc79b70e3`.
Selected scenarios: synthetic_seed3, synthetic_seed26, synthetic_seed34.
Selected fields: theta_lower, theta_upper, outer_lower, outer_upper, beta1_lower, beta1_upper.
Selected rows: 180. No values are recomputed or reformatted.
Subset SHA256: `08384057eee414fdc9b8e4465b0c118a2cb910028bea4125a0b87f8774d0d74e`.

Original SOURCE.txt attributes these values to unmodified hetid paper scripts at
`b74ee9e4ec6767ee825f5bc07d3b97d35e1ad36d` on 2026-09-29. The package layer matched
`f022c4b703101bc02fffc0679ade45c484d8d634`. The recorded environment was R 4.6.1,
nloptr 2.2.1, OpenBLAS 0.3.34 and R LAPACK. The present port is not the source of
these expected values. Hexadecimal bits, status strings and row order are retained.
Disagreement beyond the cross-platform tolerance below blocks acceptance; no automatic
regeneration or platform skip is permitted.

Cross-platform comparison, 2026-10-03 16:27 EDT, authorized by the package author: the
stored hex bits are unchanged, but computed doubles are now compared through
`expect_oracle_equal()` (tests/testthat/helper-oracle_tolerance.R) rather than
`expect_identical()`, because R CMD check on the CI matrix (macOS, Windows, Ubuntu)
differs from the capture runtime by floating-point noise. A finite double passes when
the absolute gap is at most the tolerance times max(|expected|, 1), with tolerance 1e-8
for closed-form evaluations and fits at a given point and 1e-5 for nloptr search
outputs. Statuses, NA/NaN/Inf patterns and all other non-double fields remain exact.
Expected values are still never regenerated from candidate output.
