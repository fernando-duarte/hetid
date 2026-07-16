# hetid (development version)

## Breaking changes

* `load_term_premia()` now raises a `hetid_error_insufficient_data`
  condition when the data is not available, matching the `"nyfed"`
  source's existing behavior; the `"auto"`/`"github"` path formerly
  emitted a message and returned `NULL`. Callers that tested the return
  value for `NULL` should `tryCatch` on the condition class instead.

## Improvements

* `compute_w2_residuals()` now surfaces skipped maturities in its return
  value: list mode gains a `skipped` element (named character vector of
  skip reasons) and data-frame mode carries the same information as a
  `skipped_maturities` attribute. Each skip removes a constraint and can
  only widen the identified set, so the skip set can now be checked
  programmatically instead of parsing the warning stream.
* The `hetid_moments` and `hetid_components` containers now share one
  constructor/validator structure: a cheap `new_*()` constructor (type
  and length checks) plus a full `validate_*()` shape sweep that the
  public boundaries always run. `new_hetid_components()` no longer
  accepts malformed parts silently, and validated containers can be
  rebuilt on hot paths without re-running the per-maturity sweep.

# hetid 0.3.0

## Breaking changes

* Maturity indices are now denominated in **months** everywhere. Every
  maturity argument (`i`, `maturities`) and every package column suffix
  (`y12`, `tp60`, `rny120`, ...) denotes months: `i = 60` is the 5-year
  bond, valid maturities run 3-120, and per-maturity outputs are named
  `maturity_<months>` (e.g. `maturity_24`). The old year-style names
  `y1`-`y5` no longer exist and fail loudly; **`y6`-`y10` silently
  changed meaning** from 6-10 years to 6-10 months. The former
  `EFFECTIVE_MAX_MATURITY` constant is replaced by
  `effective_max_maturity(step)`.
* The bundled and downloaded ACM data now come from the validated
  replication released at fernando-duarte/ACM_term_premium: maturities
  at one-month steps from 3 to 120 months (whole years keep the
  official raw names `ACMY01`-`ACMY10`; sub-annual months use names
  like `ACMY003M`), ISO dates, and a sample through the latest release.
  It reproduces the official NY Fed workbook to within 0.0026 basis
  points at the annual nodes; historical values also pick up the NY
  Fed's own vintage revisions relative to older snapshots.
* `download_term_premia()` now takes a `source` argument
  (`"github"` default, `"nyfed"` fallback) as its first parameter,
  fetches the GitHub release with sha256 digest verification (failing
  closed without caching on any mismatch), and writes a provenance
  sidecar. The NY Fed xls path lives on as the opt-in fallback with its
  own cache file and provides annual maturities only.
* The package now requires R >= 4.5.0 (for `tools::sha256sum()`).

## New features

* The term-structure chain (`compute_n_hat()`, `compute_price_news()`,
  `compute_sdf_innovations()`, `compute_c_hat()`, `compute_k_hat()`,
  `compute_variance_bound()`, `compute_w2_residuals()`) accepts a
  `step` argument: the number of maturity-index units (months) per news
  period, defaulting to 12 (an annual news clock). Sub-annual steps
  become possible wherever the maturity grid supports them; with the
  3-month maturity floor, a quarterly clock (`step = 3`) supports every
  horizon from the boundary `i = 3` upward.
* `load_term_premia()` and `extract_acm_data()` accept
  `source = c("auto", "github", "nyfed")`; `"auto"` resolves the GitHub
  user cache then the bundled copy and never loads the NY Fed source
  implicitly.
* `extract_acm_data()` defaults to the annual maturity nodes
  (`HETID_CONSTANTS$DEFAULT_ACM_MATURITIES`); the full monthly grid is
  available via `HETID_CONSTANTS$ALL_ACM_MATURITIES`. Requesting
  sub-annual maturities against the annual-only NY Fed source raises a
  structured error naming the fix.
* Loading gained a post-read schema guard against stale or corrupt
  caches, and character-date repair accepts ISO dates.
* `compute_expected_sdf()` and `compute_expected_sdf_variance_bound()`
  now accept the horizon-zero index `i = 0`. The expected SDF returns the
  realized one-period price `exp(-y^(1)_t)` exactly (observed at `t`, not a
  forecast, no bias correction); the variance bound returns `0` (no
  approximation error to bound). Both signal a classed
  `hetid_warning_horizon_zero` warning.

## Notes

* The identification layer's "maturities" remain positional w2 column
  indices (1..n components); only ACM-facing interfaces switched to
  month units. `validate_maturities()` gained a `min_value` argument
  (default 1) to keep the two conventions apart.
* With the default `step = 12`, all numerical results are bit-identical
  to 0.2.0 on the same data: `i = 12k` reproduces the old `i = k`
  exactly.
* `compute_c_hat()` now requires `i` to be a positive multiple of
  `step` (matching `compute_k_hat()` and `compute_k2_hat()`), raising
  `hetid_error_bad_argument` instead of silently flooring `i/step` for a
  non-multiple `i`. Results on the step-multiple maturity grids the
  pipeline uses are unchanged.

# hetid 0.2.0

* Generalized-instruments layer: arbitrary instrument matrices with
  optional transforms, per-instrument or combined constraints, and
  masked and whitened weight optimization.
* Centered `1/T` moments throughout the identification chain.
* Structured conditions (`hetid_error`, `hetid_warning_*`) across the
  package.

# hetid 0.1.0

* Initial release: ACM data access, bond-pricing chain, Lewbel (2012)
  identification moments and identified-set machinery for the VFCI
  application.
