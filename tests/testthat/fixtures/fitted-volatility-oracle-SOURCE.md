# Fitted-volatility oracle provenance

2026-10-02 22:35 EDT

These test fixtures are independent captures from macro_dynamics donor revision
`bfc02e940dc80625fa3bc1eff03ff394cec707c6`. They were frozen and accepted before
any item12 product implementation or candidate numerical evaluation.

`fitted-volatility-boundary-oracle.rds` retains the original boundary capture,
SHA256 `a701abf86a331e4b24fe3c794cbaa81ca9a11215b9083fcdad1e90437eab028b`.
It records projected fits and Jacobian, successful and failed cache hits, cold
bypass, warm starts, source counters, precheck and exponential edge cases.
The original donor envelope source has SHA256
`c4a830f676c97005b475a5a28562fdf8373a71bac0999638864f5ceb88251593`.

The full synthetic endpoint capture is preserved as two ordered binary parts.
The files are `fitted-volatility-endpoint-oracle.part-1.rds` and
`fitted-volatility-endpoint-oracle.part-2.rds`. Each is a binary segment requiring
ordered concatenation; neither is an independently readable RDS file.
Each part is 995287 bytes, below the unchanged 1000-KiB large-file hook limit.
The test helper verifies hashes and joins raw bytes before gzip/RDS decoding.
No observations, fit records, diagnostics, environments or source caches are dropped.

| Part | SHA256 |
| --- | --- |
| 1 | `f888b98e67e2e4389a5ed2bf0252b894eeaf0c151b71aec2a840be59aa38461d` |
| 2 | `d6da4ab1e5614a04c0282a77d4ad7e433c9c3b5966b83e1697ee3e6fd68622ee` |

Ordered concatenation reproduces the original 1990574 compressed bytes, SHA256
`c1046c3145e6c3c5d489fcfdcccc7aac756a50b73a69886d17a6905439997ea2`.
Gzip decoding reproduces the original 15736156 serialized bytes, SHA256
`e30f7331c92ec342ae31324f9e5020d4a43ce1258bfe7ec4742ac25d492faf3e`.
Exact compressed and serialized byte identity is decisive. Naive whole-object
identical() is false for separately restored closure/cache environment identities;
ignore.environment=TRUE is also false. The independent admitted-namespace decoding
probe instead passed exact version-3 reserialization equality. Tests consume the
complete capture and compare numerical records, rather than closure pointers.

Inputs remain the original 48-observation, two-news-column b1/b2 and pc1/pc2 sample
from log-variance-search-oracle.rds, SHA256
`9b5e511d6b60d1157ca74fcd9f901f5f88988f681134f536591756c3238b666d`.
Calendar labels are the 48 quarter ends from 2010-03-31 to 2021-12-31.
The two tau labels are .05 and .1, with radius-.1 and radius-.5 balls, A=diag(2),
b=0 and c=-radius squared. Containing bounds are symmetric and supplied independently.
Grid_n=7, grid_floor=3, primary/coverage caps=30, primary/coverage/sensitivity
budgets=1000 and envelope budget=80000 with one start per side are unchanged.
All other original controls and both complete histories are retained in the RDS.

PPML history is display/audit, grid/nesting/point, then ascending envelopes.
The separate fresh Harvey process runs PPML display only, Harvey display/audit,
Harvey grid/nesting/point, then ascending Harvey envelopes. Source caches are
shared within each history; every dated target search starts with its own cache
and budget. PPML and Harvey final source caches contain 4936 and 5341 full fits.
The original scalar donor body was evaluated once in its admitted lexical observation
frame to preserve otherwise local schema, target result, point fit and source counters.
No candidate values construct an expected endpoint.

Capture runtime: R 4.6.1, `aarch64-apple-darwin25.4.0`, macOS 26.6.2,
OpenBLAS 0.3.34, LAPACK 3.12.1, nloptr 2.2.1. Donor namespace is the accepted private
initial hetid 0.6.0 library, inventory digest
`e1cda58d6db2770313b9a033d7c84f1305cfef33ad73f36effbd020937330248`.
PPML/Harvey processes took 3.483190/4.352974 seconds and used peak RSS
130531328/128778240 bytes. Full `argv`, namespace paths, `sessionInfo`, all source/library
hashes, logs and exits are retained in the ignored execution evidence.

Boundary coefficients, Jacobian, cache/warm/cold/phase records and transforms are
exact. Synthetic endpoint values, attaining points, finite/missing/infinite patterns,
statuses, source labels, tau keys and dates are exact on the pinned runtime.
The generic prepared-sample identity intentionally replaces the donor sample label;
its equality to the supplied prepared sample is tested separately. Accepted upstream
closure/domain/selector additions retain their own explicit checks. Independent analytic
unit-ball eta endpoints permit absolute error 1e-6; transformed endpoints permit
1e-6 * `pmax(1,abs(expected))`. A canceling date permits eta error 1e-10.
Finite-difference Jacobians use step 1e-6 and tolerance 1e-5. Point containment uses
the unchanged donor volatility-scale relative slack 1e-6.

Historical full reference captures are separately frozen under ignored validation,
not bundled in these synthetic fixtures. The exact original paper pins remain donor
set_figures_pins.csv, SHA256
`6e006f6c1a7284ba6287d64293753411da3086e1640dfcf571396a378df22eb5`.
The PPML/Harvey historical captures have SHA256
`ab25d3f23ac0edb0360bc4e55d49e34dbc9b5e3b8f2b4a98dcbb48b42b2dca2f` and
`8796452c85f36138491c908f785d2c7c1882669cbea7b9dfa589abef29bdaec1`.
They use the accepted exact historical hetid 0.4.0 namespace and original 255 dates,
five taus and cap .615. No fixture refresh is authorized from candidate outputs.
