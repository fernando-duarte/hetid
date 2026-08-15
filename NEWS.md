# hetid 0.4.0

## Breaking changes

* `compute_expected_sdf_variance_bound()` now returns
  `min{(1/4) * C_hat * K_hat, Var(q)}`: the fourth-order component of the
  envelope/Taylor bound (envelope `max(exp(2 * n_hat))` times the sample
  fourth moment of the log forecast error) paired with the
  first-order-cancelled q-bound. The projection (gap-variance) arm is no
  longer part of the returned min; on the shipped ACM data the q arm binds
  at every maturity, so reported values are unchanged there, but the two
  contracts differ on other data. The function can now also return `Inf`
  when both arms overflow on a nonempty paired sample (`NA_real_` remains
  reserved for an empty sample).
* `load_term_premia()` now raises a `hetid_error_insufficient_data`
  condition when the data is not available, matching the `"nyfed"`
  source's existing behavior; the `"auto"`/`"github"` path formerly
  emitted a message and returned `NULL`. Callers that tested the return
  value for `NULL` should `tryCatch` on the condition class instead.

## New features

* New `compute_news_q_bound()`: the two-leg first-order-cancelled
  (Minkowski) bound on the variance of the centered quadratic SDF-news
  approximation error, `[sd(q1) + sd(q0) + sd(g)]^2` over the paired news
  sample. Neither it nor the envelope bound of `compute_variance_bound()`
  dominates the other, so the reported news bound is their pointwise
  minimum, taken by the caller.
* New `compute_tau0_system()` and `compute_tau0_point()`: the tau = 0 special case of
  the Lewbel (2012) triangular system, where every maturity constraint degenerates from
  a quadratic inequality to a linear equality and the stacked system solves in closed
  form for a point rather than a set. Results are wrapped in a new `hetid_tau0_fit`
  container (with a `print` method) that reports the point when one exists and stays
  generic about which condition (rank-deficient, under-determined, inconsistent) ruled
  it out otherwise.
* New `fit_log_variance()` and `fit_log_variance_at_b()`: a PPML (Poisson pseudo-maximum
  likelihood) estimator for the log-variance equation, dispatched through an estimator
  registry seam so other estimators can be added later without touching the boundary
  wrappers. `fit_log_variance_at_b()` completes the tau = 0 chain by forming the residual
  at a fixed structural parameter and feeding its square to `fit_log_variance()`. Fits
  are returned as a new `hetid_log_variance_fit` container (with a `print` method).
* New `compute_log_variance_vcov()` and `compute_log_variance_se()`: naive, HC0, HC1,
  and HAC (Newey-West) covariance matrices and standard errors for a log-variance fit,
  with the Newey-West lag truncation controlled by the new
  `LOG_VARIANCE_CONTROL$HAC_LAGS`.
* New `LOG_VARIANCE_CONTROL`: numerical controls (GLM tolerance/iterations, score and
  rank tolerances, HAC lag truncation) for the log-variance estimator, ported from the
  paper pipeline's `LOGVAR_PPML_CONTROL`. The paper pipeline keeps its own copy of this
  estimator for now (it is on the bootstrap-cache content manifest); consolidating the
  paper and package copies is left for future work.
* New `compute_identified_set_box()` and `profile_log_variance_set()`: the tau > 0 case,
  where each maturity constraint is a genuine quadratic inequality and the system defines
  a set rather than a point. The box search grids all but one coordinate and solves the
  remaining one in closed form, so every reported bound is attained at a point that
  satisfies every constraint and is returned alongside it; the grid lives in a frame in
  which the set is locally a cube, which is what keeps ill-conditioned (near-collinear)
  systems from falling between nodes. Results come back in a new `hetid_theta_box`
  container (with a `print` method) that carries the reduced forms it was built from, so
  a profile cannot be run against a different system by accident.
  The box also reports the structural coefficients (the intercept and the
  `x` coefficients, `b_0` and `b_E` in the paper) as `beta1_bounds`: the
  extremes of `beta1r - beta2r' theta` over the same set, read off the same
  line hulls, so every finite bound is again attained by a returned witness
  (`beta1_arg_lower`, `beta1_arg_upper`). The window grows for the
  coordinates first and only then for the structural coefficients, so the
  theta block is never narrowed by the addition. A loading that is zero up to
  rounding is treated as exactly zero (tolerance `null_loading_rtol`, the
  decision recorded in the `null_loading` attribute), so a point-identified
  coefficient stays a point even when the set is unbounded; once a recession
  direction is found, every coordinate and every structural coefficient with a
  non-zero loading is unbounded. `n_grid` must now be odd, so the grid always
  contains the search centre, and the print method counts unbounded sides
  rather than rows.
  `profile_log_variance_set()` then fits the log-variance equation across that set
  through the same estimator registry, reporting the range each coefficient spans.
  Both are inner approximations and are documented as such: the box is a bounding box of
  a non-convex set, so `make_system_checker()` remains the membership test, and an
  unbounded side is reported as infinite only on the strength of a witnessing recession
  direction rather than a search window.
* New `IDENTIFIED_SET_CONTROL`: grid density, growth schedule, feasibility tolerance,
  null-loading tolerance and direction-sample size for the identified-set search.

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
