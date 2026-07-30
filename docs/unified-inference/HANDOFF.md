# Unified identified-set inference: what changed and what the paper should now say

Stamped 2026-07-30 19:49 EDT. Branch `feature/unified-bootstrap-inference`.

Written to be actionable without reading any code. Section 1 is the substance. Section 8 is the
one finding that could change a claim in the paper, and it is the section to read if you read only
one.

**Status of the numbers in this memo.** All of them come from the completed from-scratch pipeline
run, or from the draw cache it produced. Numbers attributed to the *superseded* construction come
from the artifacts committed on `main`.

---

## 1. What was wrong, and what replaced it

The two panels of the main table reported confidence objects built from **two different targets**
and **two different reference distributions**.

Panel A's `tau>0` parentheses fitted a bivariate normal to the two endpoint estimators and solved
a Stoye (2009) calibration for covering a **parameter**. Panel B's quantiled a bootstrap root to
contain a whole **identified set**. Those are different questions answered against different
reference distributions, so the two panels' parentheses were not comparable — not approximately,
but in principle. The `tau=0` column compounded it: Panel A reported a Wald interval, Panel B an
analytic sandwich t-statistic that conditioned on a plug-in news vector.

Now both panels compute **both** targets from **one** shared implementation driven by **one**
shared bootstrap reference distribution — the existing circular moving-block bootstrap, unchanged.
The published `tau>0` cells report pointwise coverage. Containment is still computed and reported
per cell in the diagnostics. The `tau=0` column reports a bootstrap t-statistic in both panels.

No new draws, no new resampling, no new estimator. `B = 10,000`, block length 10, seed `20260708`,
the doubled-block sensitivity family and every index hash are untouched. Both targets are
different *functions of the same stored per-draw endpoints*.

---

## 2. The construction, cell by cell

Notation: a coefficient's estimated identified interval is `[L, U]` with width `w = U - L`; `s_L`
and `s_U` are the normal-consistent median absolute deviations of that endpoint's bootstrap draws;
`alpha = 0.10`.

Per draw, the two **inward** studentized deviations are

    z_L = (L* - L) / s_L          z_U = (U - U*) / s_U

each positive when that draw's interval is *narrower* than the full-sample one on that side.
`q_{1-alpha}` is the conservative Politis–Romano–Wolf order statistic: the
`ceiling((n+1)(1-alpha))`-th smallest of `n` finite values, capped at the largest.

### Panel A and Panel B, `tau>0` — published: pointwise coverage (Target P)

Guarantees coverage of the true coefficient **uniformly over its position in the identified set**:

    inf over phi_0 in [L, U] of P{ phi_0 in C } >= 1 - alpha

Writing the truth as `phi_0 = L + lambda*w` for `lambda` in `[0,1]`, each side earns a **width
credit** — truth sitting away from an endpoint leaves that endpoint room to spare before it can
fail:

    R_P(lambda) = max{ 0, z_L - lambda*d_L, z_U - (1-lambda)*d_U },   d_L = w/s_L, d_U = w/s_U
    c_P         = sup over lambda in [0,1] of q_{1-alpha}( R_P(lambda) )

Reported interval `[L - c_P*s_L, U + c_P*s_U]`.

The supremum is over the whole interval, not an endpoint search and not a grid. That matters
empirically, not just in principle: **an interior `lambda` attains it in 9 of 30 volatility cells
and 2 of the 9 non-degenerate mean cells**, so an endpoint-only search would have been wrong on
about a third of the table. It is found by a certified branch-and-bound that brackets the supremum from both sides and
reports the upper bracket, so the numerical approximation is conservative rather than optimistic.

### Panel A and Panel B, `tau>0` — computed, in diagnostics: containment (Target S)

Guarantees covering the **whole** identified set, `P{ [L,U] subset C } >= 1 - alpha`:

    R_S = max{ 0, z_L, z_U }
    c_S = q_{1-alpha}( R_S )

The width never enters: how far apart the endpoints are is irrelevant to clearing each of them.
Reported per cell as `c_s` in both diagnostics CSVs.

### Panel A and Panel B, `tau=0` — published: bootstrap t-statistic

    t = phi_hat / s,    s = the robust bootstrap scale of that same point's draws

with a two-sided p-value and stars from the standard normal. Exactly consistent with a Wald
interval `phi_hat ± z_{1-alpha/2}*s`, which excludes zero if and only if `|t| > 1.645`.

The denominator is a median absolute deviation rather than a sample standard deviation because the
`tau=0` estimator solves a linear system in estimated moments: resamples in which that system is
nearly singular produce arbitrarily large values, so the standard deviation does not settle as `B`
grows while the MAD does. Measured here, the ratio is **11.3, 19.5 and 10.3** for the three news
coefficients against **1.02 to 1.14** for the point-identified block. See section 8.

### What the retired `tau=0` search was actually doing

Worth recording, because it makes concrete why searching that slot was wrong rather than merely
redundant. At `tau=0` the news set is one point, so the endpoint search built its grid as
`seq(lo, hi, length.out = 41)` with `lo == hi` — **41 identical values per axis**, hence 1,681
identical grid rows, coarsened to 841, every one of them the same point. The scan therefore
resolved to one real model fit and 840 cache hits. The fits came from the polish stage instead: five
coefficients times two sides times up to four starts, each an SLSQP run whose line search perturbs
the news vector and so misses the cache on every step. That is on the order of **80 to 400 real
model fits per draw per estimator**, against a ceiling of 8,000.

The direct evaluation replaces all of it with **one** fit for the quasi-maximum-likelihood
estimator and **zero** for Harvey, which reads the fit its constructor already performed. Across
10,000 draws and two estimators that removes roughly **1.6 to 8 million model fits** from the stage,
along with the grid construction and an O(m²) nearest-neighbor tour over 841 copies of a single
point. The reported gain in the `tau=0` column is statistical, but the cost of the thing it replaced
was not small.

### The `OLS` column — unchanged

Newey–West t-statistics with four lags, both panels. Untouched.

---

## 3. Why the ordering matters, and the economics of it

For every draw and every `lambda`, `R_P(lambda) <= R_S` pointwise, because each argument of the
maximum is reduced by a non-negative credit. Order statistics of pointwise-dominated collections
are ordered, so

    c_P <= c_S

always, with equality when the estimated width is zero. This is a **deterministic identity of the
construction**, not a statistical expectation — and it holds *only because both targets quantile
the same reference distribution*. The old table violated the analogous comparison across panels
precisely because one fitted a normal and the other quantiled a bootstrap root.

The economics is worth stating in the paper. A wide identified set **helps** you cover a point,
because wherever the truth sits inside it, the far endpoint has room to spare. It does nothing to
help you cover the whole set, because both endpoints must be cleared however far apart they are.
That asymmetry is the entire content of `c_P <= c_S`.

---

## 4. The gate policy — one policy, both panels

Previously Panel A required only that at least half the draws yield a finite value, while Panel B
additionally required a bounded share of 0.85 among non-failed draws. Both panels now apply the
**stricter combination**, declared once in the analysis contract rather than once per panel. A cell
is reported only if all of:

1. its full-sample object is certified — a `tau=0` point accepted and finite, a `tau>0` interval
   with its required certified side or sides and positive width;
2. the count of draws contributing a finite accepted point, relevant side, or pair of sides is at
   least `minimum_valid_draw_share * B` = 5,000. This is an **absolute count against `B`**, so
   failed draws count against it;
3. the certified share among **non-failed** draws is at least `stability_share` = 0.85. Unbounded
   and unreliable draws stay in that denominator; failed draws are excluded from it;
4. the relevant robust scales are finite and strictly positive.

For a `tau=0` cell, "certified share" means the share whose point status is `bounded`; `unreliable`
stays in the denominator, and `unbounded` is impossible for a point evaluation and is treated as an
implementation error rather than a data condition.

A cell failing any condition is blank and the reason is recorded in the diagnostics.

The stability gate is not decorative. A comment in the multistart module records that box escapes
once pushed the bounded share below it and suppressed an entire column of confidence cells.

### Draw pools — the detail that makes the numbers reproduce

For a two-sided cell, both targets quantile over draws bounded on **both** sides (9,909 of 10,000
at `tau=0.05`; 9,341 to 9,360 at `tau=0.20`). But each side's robust scale is taken over **all**
draws bounded on *that* side, including draws the two-sided pool excludes because the other side is
not bounded. A draw with one bounded side therefore contributes to that side's scale but not to a
two-sided root quantile.

For a half-infinite cell, both targets instead use the live side's own pool, and the two targets
coincide there: on an infinite ray the worst position for the truth is the finite endpoint, so the
width credit vanishes.

---

## 5. `tau=0` point-status counts, valid-point counts and gate denominators

Measured from the existing draw cache. All seven mean-equation coefficients:

| coefficient | `bounded` | `unbounded` | `unreliable` | `failed` | valid points | non-failed denominator |
|---|---|---|---|---|---|---|
| `b_0` | 10,000 | 0 | 0 | 0 | 10,000 | 10,000 |
| `b_{1,E}` | 10,000 | 0 | 0 | 0 | 10,000 | 10,000 |
| `b_{2,E}` | 10,000 | 0 | 0 | 0 | 10,000 | 10,000 |
| `b_{3,E}` | 10,000 | 0 | 0 | 0 | 10,000 | 10,000 |
| `b_{1,N}` | 10,000 | 0 | 0 | 0 | 10,000 | 10,000 |
| `b_{2,N}` | 10,000 | 0 | 0 | 0 | 10,000 | 10,000 |
| `b_{3,N}` | 10,000 | 0 | 0 | 0 | 10,000 | 10,000 |

Every gate is satisfied with a wide margin: 10,000 valid points against a required 5,000, and a
bounded share of 1.00 against a required 0.85.

The volatility panel, from the completed run, for both estimators and all five coefficients:
**10,000 `bounded`, 0 `unbounded`, 0 `unreliable`, 0 `failed`, 10,000 valid points, 10,000 in the
non-failed denominator.** Every cell reports.

That is the headline result of the `tau=0` change. Under the superseded construction the slot ran a
full endpoint search and about **1.43 percent** of sides came back `unreliable` from search failure
alone (1.428% for the quasi-maximum-likelihood estimator, 1.477% for Harvey, over 50,000
draw-coefficient cells each). Those failures were an artifact of searching for the endpoints of a set
that is a single point. The direct evaluation removes their cause, and the unreliable count goes to
zero.

**Do not carry the old side-specific `unreliable` counts forward.** They described the retired
search. Every `tau=0` count is now recomputed from the point status.

---

## 6. Before and after, cell by cell

From the completed from-scratch run. Of the 70 tracked artifacts, **18 changed**: the 14 inference
tables and standalones, plus the four bounds-by-tolerance figures, whose only change is the caption
wording. Every other artifact regenerated byte-identical.

### Panel A, `tau=0`: a Wald interval becomes a t statistic

| coefficient | estimate | before | after | star |
|---|---|---|---|---|
| `b_0` | 0.796 | `(0.704, 0.887)` | `(14.32)` | gains `***` |
| `b_{1,E}` | 0.000 | `(-0.008, 0.010)` | `(0.18)` | none |
| `b_{2,E}` | -0.471 | `(-0.687, -0.256)` | `(-3.60)` | gains `***` |
| `b_{3,E}` | 0.834 | `(0.275, 1.394)` | `(2.45)` | gains `**` |
| `b_{1,N}` | 0.016 | `(0.001, 0.030)` | `(1.76)` | gains `*` |
| `b_{2,N}` | 0.009 | `(-0.121, 0.140)` | `(0.12)` | none |
| `b_{3,N}` | -0.453 | `(-0.960, 0.054)` | `(-1.47)` | none |

The stars are new only because the column previously printed an interval, which carries no star. No
coefficient's zero-exclusion changed here, which is the point of acceptance check 8.3: a Wald
interval excludes zero exactly when `|t| > 1.645`, and the four that did — `b_0`, `b_{2,E}`,
`b_{3,E}`, `b_{1,N}` — are exactly the four that now exceed it.

### Panel A, `tau>0`: the fitted normal is replaced

| coefficient | `tau` | before | after |
|---|---|---|---|
| `b_{1,N}` | 0.05 | `(-0.001, 0.032)` | `(-0.006, 0.037)` |
| `b_{1,N}` | 0.10 | `(-0.007, 0.037)` | `(-0.008, 0.039)` |
| `b_{1,N}` | 0.20 | `(-0.024, 0.056)` | `(-0.019, 0.050)` |
| `b_{2,N}` | 0.05 | `(-0.136, 0.151)` | `(-0.214, 0.225)` |
| `b_{2,N}` | 0.10 | `(-0.175, 0.190)` | `(-0.210, 0.223)` |
| `b_{2,N}` | 0.20 | `(-0.298, 0.343)` | `(-0.256, 0.301)` |
| `b_{3,N}` | 0.05 | `(-1.038, 0.038)` | `(-1.699, 0.580)` |
| `b_{3,N}` | 0.10 | `(-1.165, 0.070)` | `(-1.866, 0.539)` |
| `b_{3,N}` | 0.20 | `(-1.511, 0.254)` | `(-1.989, 0.521)` |

Wider at `tau = 0.05` and `0.10`, narrower at `0.20`. **No cell's zero-exclusion changed**: every one
of these intervals contained zero before and contains zero after.

### Panel B, `tau=0`: three stars change, and one cell's zero-exclusion changes

| coefficient | estimate | before | after | star |
|---|---|---|---|---|
| `theta_0` | -1.245 | `(-5.56)` | `(-4.59)` | `***` unchanged |
| `theta_{1,R}` | 0.184 | `(1.85)` | `(1.15)` | **loses `*`** |
| `theta_{2,R}` | -0.018 | `(-0.08)` | `(-0.09)` | none either way |
| `theta_{3,R}` | 0.766 | `(4.18)` | `(1.87)` | `***` becomes `*` |
| `theta_{4,R}` | 1.330 | `(3.65)` | `(2.17)` | `***` becomes `**` |

**`theta_{1,R}` is the one cell in the table whose zero-exclusion status changes.** At `|t| = 1.15`
it no longer clears the 10 percent threshold, where the analytic statistic did at 1.85. This is the
intended consequence of propagating the first-stage error: the bootstrap scale for these five
coefficients is 1.6 to 6.1 times the analytic one (2.71, 1.59, 2.14, 4.09, 6.12 for the
quasi-maximum-likelihood estimator). The old and new statistics are not comparable, and the change
is not evidence that anything was previously miscomputed.

### Panel B, `tau>0`: essentially unchanged

The cells move in the third decimal at most — `theta_{1,R}` at `tau = 0.05` goes from
`(-0.179, 0.531)` to `(-0.179, 0.530)`. As anticipated from `w/sigma` running 0.01 to 0.77, the
width credit has almost nothing to work with here. No zero-exclusion changes.

### Why the direction differs across panels

**The volatility panel barely moves.** Its identified sets are narrow relative to sampling noise —
`w/sigma` runs about 0.01 to 0.77 — so the width credit has little to work with. The largest
critical-value change among its thirty cells is `theta_{2,R}` at `tau=0.20`, from 1.231 to 1.168.

**The mean panel moves substantially, and mostly wider.** Measured on the existing draws, with the
superseded normal-theory calibration beside the two bootstrap targets:

| `tau` | coefficient | `w/sigma` | published (normal fit) | containment `c_S` | published now `c_P` |
|---|---|---|---|---|---|
| 0.05 | `b_{1,N}` | 0.93 | 1.349 | 2.118 | 1.853 |
| 0.05 | `b_{2,N}` | 0.79 | 1.373 | 2.622 | 2.315 |
| 0.05 | `b_{3,N}` | 0.52 | 1.439 | 3.381 | 3.376 |
| 0.10 | `b_{1,N}` | 1.71 | 1.289 | 1.753 | 1.413 |
| 0.10 | `b_{2,N}` | 1.45 | 1.299 | 2.073 | 1.685 |
| 0.10 | `b_{3,N}` | 0.93 | 1.349 | 3.188 | 3.179 |
| 0.20 | `b_{1,N}` | 2.52 | 1.282 | 1.184 | 0.936 |
| 0.20 | `b_{2,N}` | 2.29 | 1.283 | 1.321 | 0.963 |
| 0.20 | `b_{3,N}` | 1.48 | 1.297 | 2.283 | 2.281 |

Two things to read off this. First, the bootstrap demands a **much larger** critical value than the
fitted normal at small tolerance — the fitted normal was understating the root's tails. Second, the
sign of the difference **flips** by `tau=0.20`, where the bootstrap value is smaller. That is the
width credit working as designed: a set wide relative to sampling noise means only one side can
fail at a time, so the pointwise critical value falls toward the one-sided quantile.

The normal-theory value is retained per cell in the Panel A diagnostics as a cross-check, precisely
so this comparison stays legible rather than becoming folklore.

---

## 7. Exact wording for the table notes

Panel A's note generators no longer exist in the analysis repository — they were deleted when those
tables became bare tabulars, so the float, caption and notes now live in the manuscript. The
wording below is therefore for you to paste. The volatility panel's notes are generated and have
already been rewritten in code.

### For the `tau>0` parentheses

> Parenthesized intervals beneath the set cells are nominal 90 percent confidence intervals for the
> coefficient, calibrated to cover it wherever the truth lies in the identified interval. They are
> computed from a circular moving-block bootstrap (B = 10,000 replications, 10-quarter blocks): each
> endpoint is studentized by a robust median-absolute-deviation scale of its bootstrap draws, each
> side is credited by the estimated set width at the assumed position of the truth, and the critical
> value is the conservative Politis–Romano–Wolf order statistic of the resulting root, maximized over
> that position. It is not a normal-quantile approximation, and no distribution is fitted, so the
> dependence between the two endpoint estimators is carried by the joint bootstrap distribution
> rather than estimated. Coordinatewise intervals do not describe the joint geometry of the
> identified set. The corresponding critical value for covering the entire identified interval is
> never smaller and is reported per cell in the diagnostics.

### For the `tau=0` parentheses

> Parentheses beneath the tau = 0 estimates are bootstrap t statistics: the closed-form point
> estimate divided by a robust median-absolute-deviation scale of its moving-block bootstrap draws,
> with stars from the standard normal. Because every draw re-estimates the mean equation, these
> statistics propagate the first-stage sampling error in the news vector.

### For the `OLS` parentheses — unchanged

> Newey–West heteroskedasticity- and autocorrelation-consistent t statistics with four lags.

### Wording that is now false and must be removed

- "with the critical value calibrated against the joint normal distribution of the endpoint
  estimators at the correlation estimated from the draws" — no distribution is fitted now.
- Any citation of Stoye (2009) or Imbens–Manski (2004) **as the method behind a published cell**.
  Both remain accurate as descriptions of what the bootstrap construction converges to under a
  bivariate-normal limit, and may be cited that way.
- Any description of the volatility panel's parentheses as an "outer" envelope "covering the entire
  population identified interval". That is containment, which is no longer what the cells report.
- "The tau = 0 statistics condition on the plug-in news vector and do not propagate its first-stage
  sampling error." The new statistics do propagate it. This sentence is exactly inverted.
- The word "slack" for the validity tolerance, in any paper-facing text.

Two staleness bugs were already present in the deleted Panel A notes and should not be carried
forward: they described the `tau=0` padding as using the *one-sided* normal quantile where the code
computed a two-sided one, and reused the same coverage figure for two different clauses.

---

## 8. The one finding that could change a claim

**Read this section even if you skip the rest.**

The `tau=0` statistic divides by a median absolute deviation and reads stars off the standard
normal. That is what the specification asks for, and it is what has been implemented. But the MAD
estimates the *normal-equivalent* scale, and for the news block the bootstrap distribution of the
point estimator is very far from normal. So the reference distribution of `point / MAD` is not
standard normal there, and the stars are approximate in a direction that matters.

Measured from the stored draws, using the centered studentized bootstrap roots
`t* = (b* - b_hat) / MAD`:

| coefficient | `t = b_hat/MAD` | `p` (normal) | empirical 90th pct of `abs(t*)` | `p` (bootstrap) |
|---|---|---|---|---|
| `b_0` | 14.320 | 0.0000 | 1.689 | 0.0001 |
| `b_{1,E}` | 0.176 | 0.8604 | 1.844 | 0.8585 |
| `b_{2,E}` | -3.599 | 0.0003 | 1.704 | 0.0015 |
| `b_{3,E}` | 2.452 | 0.0142 | 1.698 | 0.0212 |
| **`b_{1,N}`** | **1.756** | **0.0791** | **2.585** | **0.1716** |
| `b_{2,N}` | 0.120 | 0.9048 | 3.296 | 0.9001 |
| `b_{3,N}` | -1.470 | 0.1415 | 3.673 | 0.2546 |

The normal reference is accurate exactly where the draws are near-normal — the point-identified
block, whose empirical critical values sit at 1.69 to 1.84 against the normal 1.645 — and badly off
exactly where sd/MAD runs 10 to 20.

**The consequence.** Under the specified construction, `b_{1,N}` carries a single star at
`p = 0.079`. Its bootstrap p-value is **0.172**. If the paper's Panel A message rests on `b_{1,N}`
being distinguishable from zero at `tau=0`, that claim is materially weaker than the star suggests.

Both p-values are now written to the Panel A diagnostics so the comparison is auditable rather than
buried. Nothing was silently substituted: the published statistic and its stars are the specified
ones. **This is a decision for you**, and the honest options are to report the bootstrap p-value
alongside, to drop the star, or to state the caveat in the notes.

---

## 9. Does an interior position bind?

Yes, and it is not marginal. An interior `lambda` attains the pointwise supremum, by more than the
search tolerance above **both** endpoint values, in **9 of 30** volatility cells and **2 of 21** mean
cells — 2 of the 9 whose identified set has positive width, since the other 12 are the
point-identified block where the two targets coincide by construction. An endpoint-only search, or
the five-node grid an earlier design considered, would therefore have reported a too-small critical
value on roughly a third of the cells that have a supremum to find.

The optimizer is certified rather than heuristic: it brackets the supremum from below by an
evaluated value and from above by a Lipschitz bound, and reports the upper bracket. Every reported
cell certifies a bracket gap within `1e-4`, recorded per cell in the diagnostics along with the
best position found, the evaluation count, and whether that position is interior.

---

## 10. Limitations

1. **Moving-block validity** requires strict stationarity, a mixing condition with enough moments,
   and a block length satisfying `m -> infinity`, `m/T -> 0`. Here `m = ceiling(1.5*T^(1/3)) = 10`.
   The doubled-block family is the sensitivity check on that choice.
2. **Bootstrap consistency for extremum statistics is not automatic.** The endpoints solve
   optimization programs, and the bootstrap can fail when the argument is not unique or the active
   constraint changes discontinuously across resamples. The multistart and certification machinery
   detects some of that and not all of it.
3. **The width is plugged in.** The estimated width replaces the population width, exactly as the
   superseded normal-theory path did. Holding the anchors and scales fixed, the pointwise critical
   value is non-increasing in the width, so an over-estimated width reduces the padding. It does
   **not** follow that the interval is anti-conservative on net: since
   `w_hat - w = (U_hat - U) - (L_hat - L)`, the same endpoint movement that inflates the width also
   moves the anchors the padding is applied to, and those effects work in opposite directions. The
   net finite-sample distortion is sign-indeterminate. Coverage is in any case not uniform in a
   neighborhood of zero width.
4. **Endpoints come from a finite search**, so a reported identified-set range can understate the
   exact projection, and that carries into both intervals.
5. **Proxy-construction uncertainty is not propagated.** The principal components are held at their
   full-sample values in every draw, so all reported uncertainty is estimation uncertainty in the
   two equations, not uncertainty in building the SDF panels.
6. **The volatility panel's `tau=0` statistics now propagate first-stage error**, where the previous
   analytic statistics conditioned on the plug-in news vector. They are not comparable to the old
   ones and will generally be **smaller in magnitude**. That is the intended improvement, not a
   defect. Under the old construction the printed values were `-5.56, 1.85, -0.08, 4.18, 3.65`; the
   new ones come from the from-scratch run.
7. **Both targets' quantiles are conditional on endpoint certification.** The root pool is the
   both-sides-bounded draws, so `1 - alpha` is a quantile *given* that both endpoints certified, and
   worst-case unconditional coverage is lower by at most the non-certified share (0.9 percent at
   `tau=0.05`, 6.6 percent at `tau=0.20`). The gates bound that share but do not remove the
   conditioning. Per-cell pool sizes and all four status counts are in the diagnostics.
8. **The `tau=0` stars read a MAD-denominated statistic against normal quantiles.** See section 8.

---

## 11. One thing to check in the manuscript

Eight fitted-volatility sweep panels ship with no in-figure caption by design — the LaTeX caption
and notes carry them — yet they redraw the same identified-set bands as their captioned
per-tolerance siblings. They therefore have no in-repository guard on how they are labeled. No
figure in the pipeline plots a confidence object, so none of them should be described as a
confidence band. Worth confirming the manuscript's notes for those panels say so.
