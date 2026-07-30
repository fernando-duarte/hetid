# Unified bootstrap inference for the identified-set tables

Stamped 2026-07-30 00:40 EDT. Branch `feature/unified-bootstrap-inference`, worktree
`/Users/fduarte/hetid-worktrees/unified-inference` (kept outside Dropbox on purpose — the
project memory records that worktrees checked out inside the Dropbox folder mmap-stall).

Baseline evidence for every number quoted here is `docs/unified-inference/00-verified-baseline.md`,
committed as `0f7c8c5`.

## Goal

Today the two panels of the main identification table report confidence objects built from two
different targets **and** two different reference distributions. Panel A's `tau>0` parentheses
calibrate a fitted bivariate normal to cover a *parameter*; Panel B's quantile a bootstrap root
to contain a whole *identified set*. The numbers are not comparable across panels.

After this change there is one shared construction, driven by the one existing bootstrap draw
set, computing both inference targets for both panels:

| | Target P (`tau>0`) | Target S (`tau>0`) | `tau=0` |
|---|---|---|---|
| Panel A (mean equation, `b_N`) | published | diagnostics | bootstrap t-statistic |
| Panel B (log volatility equation, `theta`) | published | diagnostics | bootstrap t-statistic |

The `OLS` column is untouched: Newey–West t-statistics with four lags, in both panels.

## Non-negotiables

- **No new resampling randomness.** `circular_mbb`, `B = 10000`, block 10, seed `20260708`,
  the doubled-block sensitivity family, and every index hash stay exactly as they are. Both
  targets are different *functions of the same stored per-draw endpoints*. Recording additional
  functions of those draws (the `tau=0` direct evaluation, per-side statuses, new diagnostics
  columns) is in scope; adding a draw family is not.
- **No new package dependencies.** `renv.lock` untouched.
- **Never bypass pre-commit hooks.** No `--no-verify`, no loosened linter config.
- Files under 200 lines, lines under 100 characters, no hard-wired numbering in comments,
  American spelling, no Claude attribution in any commit message.
- Paper-facing prose says "tolerance" or "validity tolerance", never "slack". Existing
  identifiers containing "slack" are left alone.

## Naming discipline

Two distinct equations, never conflated in internal text (this plan, code, comments, commits):

- **log volatility equation** — `log eps^2 = theta_0 + PC_R' theta_R + xi`, the paper's projection.
- **exp-volatility equation** — `E[eps^2 | PC_R] = exp(theta_0 + PC_R' theta_R)`, the conditional
  moment the estimator targets.

They differ by a conditional Jensen gap that shifts the intercept and leaves `theta_R` unchanged.
Paper-facing text says "the volatility equation" for both; the table's Panel B stub stays
"Log-variance equation (quasi-maximum likelihood)".

## The mathematics being implemented

`alpha = 0.10`, `B = 10000`, `T = 256`. For a scalar parameter with identified projection
`[l, u]`, estimated `[l_hat, u_hat]`, width `w_hat = u_hat - l_hat`:

Per-side inward studentized roots, per draw `b`:

    z_l^(b) = (l_hat^(b) - l_hat) / s_l
    z_u^(b) = (u_hat - u_hat^(b)) / s_u

with `s_l`, `s_u` the normal-consistent MADs. Both are positive when the draw's interval is
narrower than the full-sample one on that side.

**Target S** (containment, `P{[l,u] subset C} >= 1-alpha`):

    R_S^(b) = max{0, z_l^(b), z_u^(b)}
    c_S     = q_{1-alpha}({R_S^(b)})

**Target P** (pointwise, `inf over phi_0 in [l,u] of P{phi_0 in C} >= 1-alpha`), truth at
`phi_0 = l + lambda w`:

    R_P^(b)(lambda) = max{0, z_l^(b) - lambda d_l, z_u^(b) - (1-lambda) d_u}
    d_l = w_hat / s_l,  d_u = w_hat / s_u
    g(lambda) = q_{1-alpha}({R_P^(b)(lambda)})
    c_P*      = sup over lambda in [0,1] of g(lambda)

Each side gets a **width credit**: truth sitting away from an endpoint gives that endpoint room
to spare before it can fail. Reported interval `[l_hat - c s_l, u_hat + c s_u]`.

`q_{1-alpha}` is the conservative Politis–Romano–Wolf order statistic: the
`ceiling((n+1)(1-alpha))`-th smallest of the `n` finite values, capped at the largest.

**Ordering.** `R_P^(b)(lambda) <= R_S^(b)` pointwise for every draw and every `lambda`, because
each argument of the `max` is reduced by a non-negative credit. Order statistics of pointwise
dominated collections are ordered, so `c_P* <= c_S` — a deterministic identity, valid only
because both targets quantile the *same* reference distribution. Equality when `w_hat = 0`;
equality at positive width is possible when the order statistic is attained on draws where the
uncredited side binds.

### Draw pools (this is where §8.1 is won or lost)

- **Two-sided cell.** Both targets quantile over draws bounded on **both** sides — the current
  envelope's `common` pool (9909 draws at `tau=0.05`), not `B`. Each side's MAD is taken over
  **all** draws bounded on that side, including draws the common pool excludes because the other
  side is unbounded. Quantiling the two-sided root over all draws will not reproduce today's
  numbers and fails acceptance check 8.1.
- **Half-infinite cell.** Both targets use the **live-side pool**: every draw bounded on the
  full-sample interval's finite side, MAD over that pool, with the opposite side's status
  irrelevant. No `lambda` optimization; report the live side and mark the other unbounded.
- Within a cell shape, Target S and Target P use the same root pool at every `lambda`. That is
  what makes the ordering hold draw by draw.
- Every draw stays in the gate accounting even when its status excludes it from a pool.

### Certified continuum optimizer

The supremum is over the whole interval, not an endpoint search and not a fixed grid: the
empirical order statistic can attain its maximum at an interior point and need not be unimodal.
**Verified empirically** — an interior `lambda` binds in 9 of 30 Panel B cells and 3 of 9 Panel A
cells, so an endpoint-only search is wrong on about a third of the table.

Each draw root is a max of three affine functions with slopes `0`, `-d_l`, `+d_u`, so it is
`L = max{d_l, d_u}`-Lipschitz, and `g` inherits that constant. Branch and bound:

1. start from `[0,1]`, evaluate `g` at both endpoints, `M` = best value so far;
2. certified upper bound on any interval: `U[a,b] = (g(a) + g(b) + L (b-a)) / 2`;
3. split the interval with the largest `U` at its midpoint, evaluate, update `M`; break ties by
   the smallest left endpoint so reruns are bit-identical;
4. stop when **`min{c_S, max U} - M <= eps_lambda`** — the gap on the *reported* quantity.

Then `c_p_lower = M`, `c_p_upper = min{c_S, max U}`, with
`c_p_lower <= c_P* <= c_p_upper` and `c_p_upper - c_p_lower <= eps_lambda`. **Report
`c_p_upper`** — the upper bracket makes the numerical approximation conservative, and the cap at
`c_S` is valid by the ordering and preserves it. If `L = 0`, return `c_p_lower = c_p_upper = c_S`
without subdividing.

The certificate is airtight on all three legs: `M` is an evaluated `g` value so `M <= c_P*`;
`c_P* <= max U` by the branch bound and `c_P* <= c_S` by the ordering, so
`c_P* <= min{c_S, max U}`; and the gap is at most `eps_lambda` by the stopping test.

**Terminating on the reported quantity rather than on `max U` is load-bearing, not cosmetic.**
`U[a,b] - M` shrinks only as `L(b-a)/2`, so a flat `g` forces uniform refinement to width
`2 eps_lambda / L` — measured at **8,193 / 16,385 / 60,000+** evaluations for `L = 1 / 3 / 10`.
Folding in the `c_S` cap ends those cases in **2** evaluations, because a flat credited quantile
is exactly the case where `M` already equals `c_S`. Verified to change nothing real: across all
30 Panel B cells the two criteria give bit-identical `c_p_upper` (max difference `0.000e+00`) and
identical evaluation counts (median 8, max 30), since no real cell hits the pathology. This
removes the blowup at its cause; an iteration cap would only have truncated the symptom and left
an arbitrary constant behind.

`eps_lambda` is declared once as
`PAPER_ANALYSIS_CONTRACT$inference$target_p_lambda_tolerance = 1e-4`.

### Why this is not a new method

Under the bivariate-normal limit `((l_hat-l)/s_l, (u_hat-u)/s_u) => (X,Y) ~ N_2(0, [[1,rho],[rho,1]])`,
the continuum supremum is attained at `lambda in {0,1}` and `c_P*` solves

    min{ P(X <= c, Y >= -c - w/s_u), P(X <= c, Y >= -c - w/s_l) } = 1 - alpha

which is exactly the Stoye (2009) calibration Panel A publishes today. So Target P replaces a
*fitted* bivariate normal by the bootstrap joint distribution of the two endpoints, and `rho`
never has to be estimated. Setting `rho = 1`, `s_l = s_u` collapses it to Imbens–Manski.

### Unified gate policy

One policy, both panels, declared once in the contract. A cell reports only if:

1. the full-sample object is certified — a `tau=0` point accepted and finite, a `tau>0` interval
   with its required certified side(s) and positive width;
2. the count of draws contributing a finite accepted point / relevant side / pair of sides is at
   least `minimum_valid_draw_share * B` — an **absolute count against `B`**, so failed draws
   count against it;
3. the certified share among **non-failed** draws is at least `stability_share`. Unbounded and
   unreliable draws stay in that denominator; failed draws are excluded. For a `tau=0` point,
   "bounded share" means `point_status == "bounded"`, `unreliable` stays in the denominator,
   `unbounded` is forbidden;
4. the relevant robust scales are finite and strictly positive.

This adopts the **stricter combination** for both panels (Panel A previously required only (2)).
The stability gate exists for a real reason: a comment in
`mean_equation/inference/theta_box_multistart.R:148-156` records that box escapes once pushed the
bounded share below it and suppressed an entire column of confidence cells. A cell failing any
condition is blank, with the reason recorded.

## Verified facts the plan rests on

1. **Target S already is §A.7.** An independent reimplementation reproduces all 30 published
   Panel B `c_value`s to `2.22e-16`. This task relocates Target S, it does not change it.
2. **`fit_at_b(b, start)` already exists** on both estimator contexts
   (`log_variance/estimators/ppml/estimator.R:146`,
   `log_variance/estimators/harvey/estimator.R:114`), with `logvar_fit_ok(fit)` as the acceptance
   predicate. §5.2's direct `tau=0` evaluation is a call to existing machinery, not new code.
3. **`logvar_root_critical` is already generic** (`set_envelope.R:15-22`) — a root vector and
   `alpha`, with the conservative rank. It moves to shared ground unchanged.
4. **`point_se` is already computed and nothing renders it** (`boot_results.R:38`). Most of
   Panel A's `tau=0` t-statistic is wiring.
5. **Per-side flags already exist** from `solve_all_profile_bounds`
   (`functional_bounds.R:25-43`, returning `bounded_lower/bounded_upper/valid_lower/valid_upper`).
   Panel A collapses them at `tau_star.R:52-55` and `72-76`; Panel B keeps them separate.
6. **The `tau=0` volatility slot is a point search pretending to be an interval**: 0 of 50,000
   cells exactly degenerate per estimator, median gap `1.6e-05`, 1.43% of sides `unreliable`.
7. **The gate arithmetic must handle four statuses.** `bounded`, `unbounded`, `unreliable` from
   the solver; `failed` injected by collection (`status_contract.R:3-8`).

## The cache-invalidation constraint on sequencing

`support/inference/bootstrap_stage_code_manifest.R:1-7` hashes `support/identification` as a
whole **directory**, and lines 9-69 name `config/analysis_contract.R`,
`config/inference_search_control.R`, `set_envelope.R`, `boot_results.R`,
`theta_box_multistart.R`, `bootstrap_stage_result_helpers.R` and `support/reporting/inference.R`
among others. **Every edit this task requires invalidates the 10,000-draw cache**, and a new file
inside a hashed directory also changes the manifest. There is no cheap partial regeneration.

Consequences, which the task order below respects:

- Development validates read-only against the existing `output/state/bootstrap_stage_draws.rds`
  in throwaway scripts under the scratchpad. Never in-tree.
- **When validating stream A against Panel A data in the existing cache, duplicate the collapsed
  `status` into both `lower_status` and `upper_status` in the throwaway script's memory.** The
  per-side fields do not exist in Panel A's cached draws until stream B lands, so a shared
  builder that expects them hits a `NULL` otherwise. Panel B's cached draws already carry both.
- The single full run at the end (roughly eight hours) regenerates everything.
- Do not fight the invalidation by pinning or faking a hash.

## Traps to respect

- **`run_pipeline.R` calls `cleanup_conditional_artifacts()` at startup**, before any stage. A
  stopped partial run therefore leaves the tracked LAD and EGARCH conditional artifacts
  *deleted*, because only the producing stage puts them back. After any truncated run, check
  `git status` and restore deletions before staging. **Never `git add -A`.**
- **A clean merge skips all 16 file-based pre-commit hooks while printing green.** Only
  `pre-push` closes it. Plan on the push running the full suite over the range.
- **`isTRUE(NA < x)` is `FALSE`** and nloptr never raises — it reports failure through
  `$convergence`. Gate arithmetic must not rely on a comparison against `NA`.
- **`stopifnot("msg" = logical(0))` passes silently.** Check lengths before asserting.
- **`inference_version`** is stamped into the diagnostics from the contract via
  `support/reporting/inference.R:24-38`. A change of construction must bump it.

## File-level shape

New:

    scripts-paper/support/identification/endpoint_targets.R        primitives
    scripts-paper/support/identification/endpoint_target_cells.R   cell/table builder
    scripts-paper/tests/inference/endpoint_targets_checks.R        unit tests

Both new files sit inside the hashed `support/identification` directory, which is expected and
fine. Two files because the 200-line cap is real: the primitives (order statistic, side stat,
Target S, the certified optimizer) plus the four-shape cell builder with its gate and reasons
does not fit in one.

Retired from the published path (kept as tested pure functions, and `robust_endpoint_cor` still
feeds the diagnostics cross-check): `stoye_critical`, `im_critical`, `pbvn_le_ge`,
`robust_endpoint_cor` in `support/identification/inference_calibration.R`. The normal-theory
critical value is emitted **alongside** the bootstrap one in the Panel A diagnostics CSV as a
cross-check on the normal approximation. Exactly one live path reaches a published cell.

## External review: what was rejected, and why

Recorded so these are not re-litigated. Both reviewers lacked the paper brief, which is where three
of these are settled.

- **"Delete the permanent normal-theory cross-check; do it once in scratch."** Rejected. The brief
  requires the normal-theory critical value emitted *alongside* the bootstrap one in the Panel A
  diagnostics as a standing cross-check on the normal approximation, explicitly as "a one-column
  addition, not an extra". A one-off scratch check would not survive the next run.
- **"Delete `logvar_simultaneous_critical`."** Rejected. Target S stays a first-class computed
  object available to any exhibit that wants containment, and the diagnostics report simultaneous
  coverage. Removing a working diagnostic is outside this task.
- **"Remove the `/simplify` phase."** Rejected. It is a required step of the final sequence.
- **"Replace the both-bounded pool with a `B`-aligned worst-case rank."** Rejected as a
  *construction* change: it would break acceptance check 8.1, which requires reproducing the
  published envelope exactly. Accepted as a *limitation* — see limitation 8.
- **"Calibrate the `tau=0` stars off the empirical root distribution instead of the normal."**
  Rejected as a construction change: §A.10 mandates the normal reference and acceptance check 8.3
  pins the 1.645 correspondence to it. Accepted as a limitation, with the measured discrepancy and
  new diagnostics columns — see limitation 7.
- **"Add an iteration cap to the optimizer."** Rejected in favor of folding the `c_s` cap into the
  stopping test, which removes the cause rather than truncating the symptom.

## Tasks

Ordering is by genuine dependency, and it is **not** a free-for-all fan-out. The real graph:

    A  (shared module, contract)          -- blocking, no dependencies
    B+C  (per-side statuses, Panel A tau=0, cache validator)   -- ONE stream: B and C edit the
         same two files, and B5's validator change gates both
    D  (Panel B tau=0 at the logvar_run_estimator seam)        -- parallel with B+C
    E  (call sites)      -- needs A, B+C, D schemas
    F  (diagnostics)     -- needs E
    G  (tables, notes, figures)  -- needs F
    H  (tests)           -- tracks each stream; H2 waits for the from-scratch run

Streams G's note and caption text and H's retargeting can be drafted against the agreed signatures
before the internals land, but they cannot be verified until F does.

### Stream A — the shared construction (blocking; everything else keys off its signatures)

**A1. Contract fields.** In `config/analysis_contract.R`, extend
`PAPER_ANALYSIS_CONTRACT$inference` with `stability_share = 0.85` (moved in from
`PAPER_INFERENCE_SEARCH_CONTROL$logvar_endpoint$stability_share`, which is **removed** so the
policy is declared once) and `target_p_lambda_tolerance = 1e-4`; bump `version` to `"2.0.0"`.
Add `stopifnot` coverage matching the file's existing style. Update
`config/inference_search_control.R` to drop the `logvar_endpoint` entry and its assertions.

**A2. `endpoint_targets.R`.** Pure primitives, no rendering, no I/O:

    root_critical(root, alpha)                      # relocated logvar_root_critical, verbatim
    endpoint_side_stat(vals, status, anchor, inward_sign, min_reps, stability)
    target_s_critical(z_lower, z_upper, pool, alpha)
    target_p_critical(z_lower, z_upper, pool, d_lower, d_upper, alpha, tolerance, c_s)
    point_t_statistic(point_hat, point_draws, point_status, min_reps, stability)

`target_p_critical` returns `list(c_p_lower, c_p_upper, evals, best_lambda, interior)` and takes
`c_s` because the stopping test folds in the cap. `endpoint_side_stat` generalizes
`logvar_side_stat` unchanged in behavior: `ok` is finite and `bounded`, the non-failed denominator
drives `frac`, the scale is `robust_scale` over `ok`, and `z` is defined only on `ok`. The scale
**must** be `robust_scale` (`stats::mad` at its defaults, including the fewer-than-two-finite-inputs
`NA` rule) — a hard-coded 1.4826 that misses the small-`n` rule drifts §8.1.

`point_t_statistic` is the **one** `tau=0` builder both panels call, so the shared-implementation
mandate covers the `tau=0` cell too and not only the `tau>0` cells. Panel A feeds it
`point_draws` / `point_status`; Panel B feeds it the `point` / `point_status` fields stream D
introduces. Per coefficient it returns `se` (the robust scale of the accepted point values),
`t = point_hat / se`, the two-sided normal `p`, `stars` from the existing `sig_stars`, the four
status counts, both gate denominators, and the blank `reason`.

**The pool must be fixed across `lambda`.** `target_p_critical` computes the pool once and
quantiles over it at every `lambda`; the roots are finite there by construction, so the order
statistic never silently drops a draw and `n` never changes. A `lambda`-dependent pool would
break the monotonicity the Lipschitz argument needs. Assert the pool size once rather than
re-filtering inside the objective.

**A3. `endpoint_target_cells.R`.** One cell builder both panels call, handling the four shapes
(two-sided, upper-only, lower-only, both unbounded) plus the suppression path, applying the
unified gate, and returning both targets per cell:

    endpoint_target_row(lower, upper, lower_status, upper_status, full_row,
                        alpha, min_reps, stability, tolerance)
    endpoint_target_table(draws, full, alpha, min_reps, stability, tolerance)

Row fields: `se_lower`, `se_upper`, `n_lower`, `n_upper`, `n_common`, `frac_lower`,
`frac_upper`, `gate_lower`, `gate_upper`, `side`, `c_s`, `c_p_lower`, `c_p_upper`, `c_p_gap`,
`c_p_evals`, `c_p_lambda`, `c_p_interior`, `ci_lower`, `ci_upper`, `reason`. `ci_lower`/`ci_upper`
are the **Target P** interval (`c_p_upper`); Target S rides along as `c_s` for the diagnostics.

**A4. Unit tests** in `tests/inference/endpoint_targets_checks.R`, descriptive section headers,
no numbering:

- **the ordering identity**: `c_p_upper <= c_s` on randomized synthetic draws;
- **degenerate width**: `w = 0` gives `c_p_lower = c_p_upper = c_s` with no subdivision;
- **the adversarial interior optimum** (§8.2), constructed as `d_l = d_u = 1` with 84% of draws
  at `z_l = z_u = 0`, 8% at `z_l = 1/4, z_u = 0`, and 8% at `z_l = 0, z_u = 1`. This realizes
  roots `0`, `max(0, 1/4 - lambda)`, `lambda`. **Verified**: the former
  `{0, 1/4, 1/2, 3/4, 1}` grid returns 0 at all five nodes while the true supremum is `1/8` at
  `lambda = 1/8`; branch and bound finds it in 6 evaluations with a zero gap. The test must fail
  for an endpoint-only search or that grid;
- **an asymmetric-credit interior optimum**, with `d_l` and `d_u` deliberately unequal (e.g.
  `d_l = 1`, `d_u = 5`). The symmetric case above cannot distinguish `L = max(d_l, d_u)` from
  `L = d_l` or `L = (d_l + d_u)/2`, so a mis-derived Lipschitz constant passes it silently while
  producing a non-conservative bound on real asymmetric data — Panel A's scales differ across
  sides by up to a factor of two (`se_lower = 0.5095` against `se_upper = 0.2707` at
  `tau = 0.20`, `b_{3,N}`), so this is a live risk, not a hypothetical one. Assert both that the
  reported `c_p_upper` brackets the supremum found by a dense reference grid and that the
  certified gap holds;
- **stopping-test economy**: on a flat credited quantile with a large `L`, the optimizer must
  terminate in a handful of evaluations rather than thousands, which pins the stopping test to the
  reported quantity and prevents a regression back to the `max U` form;
- **conservative rank**: `root_critical` returns the `ceiling((n+1)(1-alpha))`-th smallest,
  capped;
- **half-infinite shapes**: a one-live-side cell reports the live side and does not blank;
- **gate arithmetic** over all four statuses: failed draws count against the absolute minimum
  and are excluded from the non-failed denominator, while unbounded and unreliable stay in it.

### Stream B — per-side status threading (Panel A)

**B1.** In `support/identification/tau_star.R::coef_interval_tables_from_quadratic`, add
`lower_status` and `upper_status` columns to both the `theta` and `beta1` frames, built from the
already-available per-side flags, **keeping** the collapsed `status` column that the `tau*` sweep
and the set-cell renderer consume. Leave the coarser `all()` collapse inside `eval_width_at_tau`
(`tau_star.R:36-37`) alone — it serves the `tau*` sweep, which must not move.

**B1a. THE BETA ROWS REVERSE THEIR SIDES. Read this before writing the mapping.** For the `theta`
rows the mapping is direct: `set_lower = tb$lower` comes from the `"min"` solve and
`set_upper = tb$upper` from `"max"`, so `lower_status` takes `bounded_lower`/`valid_lower` and
`upper_status` takes `bounded_upper`/`valid_upper`. For the `beta1` rows it is **inverted**, because
the bound enters with a minus sign (`tau_star.R:68-77`):

    set_lower = beta1r[p] - fmax$bound        # the LOWER side is governed by fmax
    set_upper = beta1r[p] - fmin$bound        # the UPPER side is governed by fmin

So `lower_status` must be built from **`fmax`**'s flags and `upper_status` from **`fmin`**'s. The
natural-looking mapping is wrong here and would silently swap the two sides' statuses, scales and
credits on the `b_0`/`b_E` block. Add a fixture with deliberately asymmetric `fmin`/`fmax` flags
that fails under the swapped mapping.

**B2.** `widen_theta_box` (`theta_box_multistart.R:115-146`) mutates only `set_lower`/`set_upper`
and reads `status`, so the new columns pass through. But note the widening is **skipped entirely
unless the collapsed row status is bounded** (`theta_box_multistart.R:133`,
`if (theta_tab$status[k] != bounded) next`). A half-infinite row's finite side therefore bypasses
multistart widening. Do not "fix" that here — it is the `tau*`-safe behavior — but record it, since
it means a half-infinite Panel A cell's live endpoint is the un-widened one.

**B3.** In `identified_set_bootstrap.R::set_id_boot_draw_from_est`, record `lower_status` and
`upper_status` per draw instead of one `status`, and NA-mask each side by **its own** status
rather than the shared mask at lines 70-75.

**B4.** In `identified_set_bootstrap_collect.R`, stack four matrices per `tau` instead of three,
and inject `failed` into both status matrices for wholesale draw failures.

**B5. The cache validator will reject the new payload — update it in the same commit.**
`support/inference/bootstrap_stage_mean_cache.R` validates with **exact** field lists:
`bootstrap_stage_cache_exact_list(collected, fields)` over
`point_draws / n_point_deficient / endpoint_draws / tau_star_draws / n_capped / n_failed /
failure_causes` (lines 21-26), and `exact_list(cell, c("lower", "upper", "status"))` per endpoint
cell (lines 48-51). Adding `point_status` and replacing `status` with the two per-side matrices
fails both, and cache promotion then fails in `inference/bootstrap_stage_cache.R:80-88`. So:
extend the top-level field list, validate each side's status matrix separately, carry the
per-side value/status consistency check (`non-bounded implies NA`) side by side rather than
jointly, and **bump `BOOTSTRAP_STAGE_CACHE_SCHEMA`** (`inference/bootstrap_stage_cache.R:1`).
Update the cache fixtures with it. This is a blocking dependency for streams C and D, not an
afterthought.

Note for the memo: this **changes Panel A's stored draws**, hence its published numbers, because
a draw bounded on one side only now contributes to that side's scale. Acceptance check 8.1's
exactness therefore applies to Panel B, which already carries per-side statuses.

**Predicted magnitude, and a check worth running.** The collapsed status is the *worst* of the two
sides, so "collapsed bounded" is exactly "bounded on both sides". The two-sided **common pool is
therefore unchanged** by threading — `n_common` should stay 9909 / 9812 / 9341-9360 at
`tau = 0.05 / 0.10 / 0.20`. Only the per-side scales move, each gaining the draws bounded on that
side alone (at most 272 / 557 / 1923 draws across all seven coefficients). So Panel A's Target-P
numbers should land close to the prototype table above, and a large move in `n_common` after
stream B is evidence of a threading bug, not of the intended change.

### Stream C — `tau=0`, Panel A

**C1.** In `set_id_boot_draw_from_est`, add a length-7 `point_status`: `bounded` where the
`tau=0` point is available and the coefficient finite, `unreliable` where `Q` is rank deficient
or the point is unavailable or nonfinite. `unbounded` is impossible for a point evaluation and is
an implementation error.

**C2.** In `set_id_boot_collect`, stack an authoritative `B x 7` `point_status` alongside
`point_draws`, with `failed` for wholesale failures.

**C3.** Delete `point_inference` from `identified_set_inference.R`; Panel A calls the shared
`point_t_statistic` from stream A2 instead. Stars from the standard normal, per §A.10.

**C4.** Wire it in `mean_equation/inference/boot_results.R`, replacing `point_ci` with the
t-statistic frame while keeping `point_se` (which the diagnostics now render).

### Stream D — `tau=0`, Panel B

**D0. The seam is `logvar_run_estimator`, not the draw file.** The `tau` loop that calls
`logvar_engine_set_at_tau` for **every** slot including `tau=0` is
`log_variance/inference/set_bootstrap_core.R:6-25`; `set_bootstrap_draw.R:36-41` only invokes that
loop. Editing the draw file alone would leave the forbidden `tau=0` endpoint search running. Note
that `b_point` is already threaded into the loop as the search seed
(`seed <- if (is.null(b_point)) logvar_box_seed(boxes[[index]]) else b_point`), so the branch has
everything it needs in scope.

**D1.** In `logvar_run_estimator`, branch on the `tau=0` slot: instead of
`logvar_engine_set_at_tau`, call the estimator context's existing `fit_at_b(b_point)`, take
`fit$coef` as the authoritative `B x 5` `point`, and set `point_status` from `logvar_fit_ok`. Status mapping exactly per §5.2: `bounded` when accepted and finite;
`unreliable` when the draw completed but `Q` is rank deficient (`point` is `NULL`), the point is
unavailable or nonfinite, or the direct fit is rejected; `failed` only for a wholesale draw
failure; `unbounded` forbidden.

**D2.** Create the compatibility mirrors **only after** the direct evaluation, as exact copies:
`lower <- upper <- point` and `lower_status <- upper_status <- point_status`. Do not compare the
retired endpoints, impose an equality tolerance, or pick a side or midpoint. The mirrors keep
`logvar_boot_failure_gate` (`set_bootstrap_gate.R`, which pools both status fields) working
unchanged, which is the reason to retain them.

**D3.** Recompute every `tau=0` count from `point_status`. The old side-specific `unreliable`
counts are baseline diagnostics only, never gate inputs.

**D4.** Confirm the cache/contract schema layer accepts the two new per-slot fields; extend its
assertions rather than loosening them.

### Stream E — call sites

**E1.** Panel B: `support/inference/bootstrap_stage_result_helpers.R::bootstrap_stage_envelopes`
calls the shared `endpoint_target_table` instead of `logvar_endpoint_envelope`.
`logvar_endpoint_envelope` and `logvar_side_stat` are deleted from
`log_variance/inference/set_envelope.R`; `logvar_root_critical` moves to
`endpoint_targets.R` as `root_critical`. Keep `logvar_simultaneous_critical`, repointed at the
shared primitives, since the diagnostics report simultaneous coverage.

**E2.** Panel A: `boot_results.R` calls the shared `endpoint_target_table` instead of
`endpoint_inference`. `endpoint_inference` is deleted.

**E2a. Where the normal-theory cross-check is computed.** `endpoint_target_table` does **not**
return `c_stoye`/`c_im`/`rho` — keeping them out of the shared builder is what guarantees a single
live path into a published cell. They are computed in the Panel A **diagnostics assembler** only
(`set_id_boot_diagnostics` in `identified_set_bootstrap.R`), which already receives the endpoint
draws and the full-sample table: call the retained `robust_endpoint_cor`, then `stoye_critical`
and `im_critical`, and bind the three columns onto the diagnostics frame. Nothing else may call
them. This is the one place the plan previously left an implementer without an address.

**E3.** The sensitivity draws (`volatility_sensitivity`) go through the same shared functions.

**E4. Panel B's `tau=0` cell** is computed by the shared `point_t_statistic` from the `point` /
`point_status` fields, per estimator, and rendered by stream G2. Note that
`bootstrap_stage_envelopes` iterates `layout$slots = seq_along(display_taus) + 1L`
(`bootstrap_stage_result_helpers.R:16-28,47-65`), so the `tau=0` slot never enters the interval
builder and cannot be mistaken for a degenerate-width Target-P cell. The compatibility mirrors
exist for `logvar_boot_failure_gate` only, never as an inference input.

### Stream F — diagnostics schema

**F1.** Panel A `set_id_inference_diagnostics.csv`: add the Target-P columns (`c_p_lower`,
`c_p_upper`, `c_p_gap`, `c_p_lambda`, `c_p_evals`, `c_p_interior`), keep `c_s` and the
normal-theory `c_stoye`/`c_im`/`rho` as the cross-check, and **add `tau=0` rows for all seven
coefficients** carrying the robust scale, the point, the t-statistic, the four `point_status`
counts, the valid-point count and both gate denominators. Today the file has no `tau=0` rows at
all, and the `b_N` scales (`0.0088524`, `0.0793743`, `0.3082586`) live only in the cache; closing
this makes acceptance check 8.3 self-contained.

**F2.** Panel B `log_var_eq_set_inference_diagnostics.csv`: add the same Target-P columns beside
the existing `c_value` (which becomes `c_s`), plus the `point_status` counts and gate
denominators. Retain `tau0_sd_boot` / `tau0_se_analytic` / `tau0_ratio` as the before-and-after
record of the conditioning change.

**F3.** Both files carry `inference_version = "2.0.0"` via the existing contract stamp.

### Stream G — tables, notes, captions, figures

**G1.** Panel A rendering: `mean_equation/tables/structural_table_parts.R` renders the `tau=0`
sub-row as a parenthesized t-statistic with stars on the point (mirroring the `OLS` column's
`ols_cells`/`ols_tstats` treatment at lines 49-60) instead of `point_ci` at lines 75-78 and
112-115, and the `tau>0` sub-rows from the shared table's Target-P `ci_lower`/`ci_upper`. The
blank-when-the-set-cell-is-blank rule stays.

**G2.** Panel B rendering through `log_variance/tables/table_formatting.R` and
`estimator_panel.R`, published by `render_combined_inference_table.R`.

**G3.** Notes and captions regenerated so every statement about how the parentheses are built is
true: `log_variance/tables/ppml_captions.R`, `harvey_caption.R`, `ppml_table_parts.R`,
`legacy_log_ols_caption.R`, `set_inference_caption.R`, and the structural table's own notes.
"Calibrated at the set endpoints" and any citation of Stoye or Imbens–Manski **as the published
method** become false and must go. Panel B's containment language is replaced by the pointwise
statement. `log_var_eq.tex`'s plug-in-conditioning caveat must be rewritten, not deleted — the
new `tau=0` statistics *do* propagate first-stage error.

**G4.** Affected LaTeX artifacts, enumerated from `config/artifact_manifest_data.R` and
`config/artifact_latex.R` rather than assumed from this list:

    structural_var_inference.tex        structural_var_inference_standalone.tex
    structural_eq_inference.tex         structural_eq_inference_standalone.tex
    log_var_eq.tex                      log_var_eq_standalone.tex
    log_var_eq_harvey.tex               log_var_eq_harvey_standalone.tex
    log_var_eq_panels_inference.tex     log_var_eq_panels_inference_standalone.tex
    log_var_eq_panels.tex               log_var_eq_panels_standalone.tex
    log_var_eq_lad_panel.tex            log_var_eq_lad_panel_standalone.tex

Every standalone has a registered `.pdf` sibling that regenerates too.
`structural_var_inference.tex` is the main exhibit. `log_var_eq_lad_panel*` is a **conditional**
artifact (manifest flag `l`), so the startup-cleanup trap applies to it.

**Enumeration verified against the manifest.** `config/artifact_manifest_data.R` registers exactly
24 `.tex` artifacts. The 14 above are the complete set carrying set-identification inference. The
other ten need nothing, for a stated reason each: `summary_stats.tex`, `correlations.tex` and
`descriptive_stats.tex` are descriptive; `ols_mean_eq.tex` is OLS only; `hetero_tests{,_standalone}.tex`
are heteroskedasticity tests, not set inference; `var_share{,_standalone}.tex` and
`variance_bounds_summary{,_standalone}.tex` carry no inference statistics.
`approximation_error_quoted_numbers.csv` was also checked and holds variance-bound approximation
errors (`max_bound_sdf_news`, `max_bound_expected_sdf`), not inference numbers.

**G5.** Figures. Classify each: an identified-set image stays an identified-set object and its
note must not call it a confidence band; a confidence band moves to the Target-P construction and
says so. No figure may silently mix targets with the tables. Sweep every figure note, caption,
axis label and legend for paper-facing "slack" and replace with "tolerance".

### Stream H — tests

**H0. How the suite is actually wired — read this before adding a test file.**
`tests/run_tests.R:7-61` is an **explicit manifest**, not a discovery walk: a new file that is not
registered, or not sourced by a registered file, silently never runs. Most of the files this task
touches are reached indirectly, which was verified file by file:

| check file | reached from |
|---|---|
| `inference/set_envelope_checks.R` | `inference/test_set_bootstrap.R` |
| `inference/set_bootstrap_{gate,draw,collection}_checks.R` | `inference/test_set_bootstrap.R` |
| `inference/set_bootstrap_core_checks.R` | `inference/set_bootstrap_cores_checks.R`, itself sourced at `test_set_bootstrap.R:99` |
| `inference/mean_boot_results_schema_checks.R` | `inference/mean_boot_results_checks.R` |
| `support/envelope_cell_checks.R` | `estimators/{ppml,harvey}/test_{ppml,harvey}.R` |
| `support/inference_control_checks.R` | `support/test_statistics.R` |
| `inference/standard_error_estimators_checks.R` | `estimators/ppml/test_ppml.R` |

So the new `tests/inference/endpoint_targets_checks.R` must be **either** registered in
`suite_manifest` with a fresh `id` and `path` **or** sourced from an already-registered file.
Register it — a shared construction deserves its own named suite. Note `run_tests.R` also runs two
structural checks first (`support/check_topology.R`, `support/check_contract_ownership.R`); the
topology gate is what enforces the 200-line cap and the source graph, so it will fail on an
oversized new file or an unregistered contract field.

**H0a.** `inference/set_bootstrap_cores_checks.R` asserts that `logvar_set_boot_draw` gives
**identical** results under `cores = 1` and `cores = 2`, on the real callback rather than a toy.
Stream D must preserve that determinism: the `tau=0` direct evaluation must not introduce any
order- or process-dependent behavior. `set_bootstrap_core_checks.R` builds the shared fixture
(`lbd_dat`, `lbd_spec`) that this check reuses, and it exercises the real draw path including the
`tau=0` slot, so it needs retargeting for the new `point` / `point_status` fields.

**H1.** Retarget rather than delete, using the table above to find each file's entry point:
`tests/inference/set_envelope_checks.R`, `tests/support/envelope_cell_checks.R`,
`tests/support/inference_control_checks.R` (it asserts the `logvar_endpoint$stability_share` field
that stream A1 moves to the contract), `tests/inference/mean_boot_results_checks.R`,
`tests/inference/mean_boot_results_schema_checks.R`, the
`tests/inference/set_bootstrap_{gate,draw,collection,core}_checks.R` family, and
`tests/inference/standard_error_estimators_checks.R`.

**H2. There are no fixture numbers to re-pin.** `tests/validation/test_table_acceptance.R` is a
19-line dispatcher over `table_projection_checks.R`, `table_comparison_checks.R`,
`mutation_matrix_checks.R`, `ssot_checks.R` and `cli_checks.R`, and those exercise the **comparator
machinery** on synthetic inline fixtures (values like `1.234`, `4.56`), not the real published
numbers. So "re-pinning" is not a fixture edit and looking for one wastes time. The reference for a
cross-run comparison is the **tracked `output/tables/*.tex` artifacts themselves**, which the
from-scratch run regenerates. The acceptance work is therefore: commit the regenerated `.tex`, then
confirm the comparator reports no differences on a rerun against them — which is exactly acceptance
check 4. If a comparator fixture happens to encode a superseded *construction* rather than a
number, retarget that; otherwise leave these five files alone.

### Stream I — final sequence

1. `/simplify` over every changed and new file. Prefer deletion. Machinery not needed for the
   four cells in the deliverable matrix comes out.
2. `R CMD check` on the package; `Rscript scripts-paper/tests/run_tests.R`.
3. Inspect outputs critically — units, signs, orders of magnitude, comparability with §6. Write
   down every changed cell and every cell whose zero-exclusion status changed.
4. Remove all caches, generated artifacts, outputs and recorded guard/table state, distinguishing
   generated artifacts from tracked inputs, and honoring the conditional-artifact trap.
5. Full pipeline from scratch including bootstrap and sensitivity. Roughly eight hours. No
   shortcuts, no reduced `B`.
6. Re-inspect, and confirm the from-scratch numbers are **identical** to the pre-wipe ones — the
   seed and index families are fixed. Any difference is a finding to investigate and report.
7. `R CMD check` and the paper suite again.

## Acceptance checks

Few and exact; resist adding more.

1. **Target S reproduces today's Panel B envelope** to full displayed precision on the same
   draws, gates and scales. Already verified at `2.22e-16` against a fresh implementation on
   unmodified `main`.
2a. **The ordering test must not be tautological.** Because `c_p_upper` is *computed* as
`min(c_s, max U)`, asserting `c_p_upper <= c_s` proves only that `min` works. Keep it (it is
mandated and free) but add the two checks that actually have content: `c_p_lower <= c_s`, and the
**uncapped** bound `max U <= c_s + tolerance`. Record the uncapped bound in the diagnostics as
`c_p_raw_upper` so the ordering identity is auditable rather than assumed. Also assert the
draw-by-draw domination `R_P(lambda) <= R_S` directly in the unit tests, which is the identity's
actual source.

2. **Ordering, cell by cell.** `c_p_upper <= c_s` for every coefficient and `tau` in both panels
   — a deterministic identity, not a statistical expectation. Equality expected at zero width;
   equality at positive width is a finding to explain, not an automatic failure. Every reported
   two-sided Target-P cell must certify `0 <= c_p_upper - c_p_lower <= 1e-4` in the diagnostics,
   and the reported critical value must equal `c_p_upper`. Plus the adversarial interior-optimum
   unit test.
3. **`tau=0` consistency.** Panel A's t-statistic equals the full-sample point over the robust
   scale of the `tau=0` point draws, and therefore exceeds `1.645` in absolute value exactly for
   the coefficients whose current `tau=0` Wald interval excludes zero. Panel B makes **no**
   endpoint-search call at `tau=0`: one authoritative point per accepted draw, any compatibility
   values and statuses identical copies, no `point_status == "unbounded"`. For every Panel A
   coefficient and every Panel B coefficient-estimator pair, the diagnostics reconcile the four
   status counts, the valid-point count and both gate denominators exactly.
4. **From-scratch reproducibility.** The full run reproduces the pre-wipe numbers identically.

## Deliverables

1. The implementation on `feature/unified-bootstrap-inference`, atomic commits.
2. Regenerated tables, standalones, figures, diagnostics, reports.
3. A single markdown handoff memo in the repository, actionable without reading code, stating:
   the construction for each of the four matrix cells in prose and formulas; the gate policy;
   every `tau=0` point-status count, valid-point count and gate denominator; the before-and-after
   value of every changed table cell; every cell whose zero-exclusion status changed; the exact
   wording the paper's table notes should now use for the `tau>0`, `tau=0` and `OLS` parentheses;
   what the `tau=0` change means for interpreting Panel B given that the old statistics
   conditioned on the plug-in news vector and the new ones do not; whether an interior `lambda`
   binds in any cell; and the limitations below. Stamped with date, time and timezone.

## Limitations to document

1. **Moving-block validity** needs strict stationarity, a mixing condition with enough moments,
   and `m -> infinity`, `m/T -> 0`. `m = ceiling(1.5 T^(1/3)) = 10` is the chosen rate; the
   doubled-block family is the sensitivity check on it.
2. **Bootstrap consistency for extremum statistics is not automatic.** `l_hat` and `u_hat` solve
   optimization programs; the bootstrap can fail when the argument is not unique or the active
   constraint changes discontinuously across resamples. The multistart and certification
   machinery detects some of this and not all of it.
3. **The width is plugged in.** `w_hat` replaces `w`, exactly as the normal-theory path did.
   `c_P*` is non-increasing in the width **holding the anchors and scales fixed**, so an
   over-estimated width reduces the calibrated padding. It does **not** follow that an
   over-estimated width makes the interval anti-conservative on net: since
   `w_hat - w = (u_hat - u) - (l_hat - l)`, the same endpoint movement that inflates the width also
   moves the anchors the padding is applied to, and those two effects work in opposite directions.
   The net finite-sample coverage distortion is sign-indeterminate. Coverage is in any case not
   uniform in a neighborhood of `w = 0`. The width's own bootstrap dispersion is already in the
   diagnostics and belongs *next to* the cells, not folded into them.
4. **Endpoints come from a finite search**, so a reported range can understate the exact
   projection, and that carries into both intervals.
5. **Proxy-construction uncertainty is not propagated.** The principal components are held at
   their full-sample values in every draw, so all reported uncertainty is estimation uncertainty
   in the two equations, not uncertainty in building the SDF panels.
6. **Panel B's `tau=0` t-statistics now propagate first-stage error**, where the previous
   analytic statistics conditioned on the plug-in news vector. They are not comparable to the
   old ones and will generally be smaller in magnitude. That is the intended improvement.
7. **The `tau=0` stars read a MAD-denominated statistic against standard-normal quantiles, and for
   the `b_N` block that is materially anti-conservative.** This is the most consequential caveat in
   the change and it must be stated plainly in the memo, because it bears on a significance claim
   in the main table. Measured from the stored draws, with the centered studentized bootstrap roots
   `t* = (b_hat* - b_hat) / MAD(b_hat*)`:

   | coef | `t = b_hat/MAD` | `p` normal | empirical 90% of `abs(t*)` | `p` bootstrap |
   |---|---|---|---|---|
   | `b_0` | 14.324 | 0.0000 | 1.687 | 0.0001 |
   | `b_{1,E}` | 0.190 | 0.8493 | 1.844 | 0.8458 |
   | `b_{2,E}` | -3.595 | 0.0003 | 1.702 | 0.0015 |
   | `b_{3,E}` | 2.452 | 0.0142 | 1.698 | 0.0212 |
   | `b_{1,N}` | 1.807 | **0.0707** | 2.598 | **0.1682** |
   | `b_{2,N}` | 0.113 | 0.9097 | 3.295 | 0.9052 |
   | `b_{3,N}` | -1.470 | 0.1417 | 3.674 | 0.2548 |

   The normal reference is accurate exactly where the draws are near-normal — the point-identified
   block, whose empirical critical values sit at 1.69-1.84 against the normal 1.6449 — and badly
   off exactly where sd/MAD runs 10 to 20. Under the mandated normal reference `b_{1,N}` carries a
   single star at `p = 0.071`; its bootstrap p-value is 0.168. **Implement the specified normal
   reference** (§A.10 mandates it and acceptance check 8.3 pins the 1.645 correspondence to it),
   and record the empirical `abs(t*)` critical value and bootstrap p-value as diagnostics
   cross-check columns — recording further functions of the same draws is explicitly in scope. Do
   not silently substitute a different denominator or reference, and do not bury the discrepancy.

8. **Both targets' quantiles are conditional on endpoint certification, not unconditional.** The
   two-sided root pool is the both-sides-bounded draws (9909 of 10000 at `tau = 0.05`, 9341 at
   `tau = 0.20`), so `1 - alpha` is a quantile of the root distribution *given* that both endpoints
   certified, and worst-case unconditional coverage is lower by at most the non-certified share.
   The `minimum_valid_draw_share` and `stability_share` gates bound that share but do not remove
   the conditioning. This pool is mandated (§A.6) and required for acceptance check 8.1 to
   reproduce the published envelope, so the construction stands; the honest treatment is to report
   the per-cell pool size and the four status counts, which the diagnostics now do, and to state
   the conditioning in the memo rather than claiming unconditional coverage.

## Economics worth stating in the memo

A wide identified set *helps* you cover a point, because wherever the truth sits inside it the
far endpoint has room to spare. It does nothing to help you cover the whole set, because both
endpoints must be cleared however far apart they are. That asymmetry is the entire content of the
ordering `c_P* <= c_S`, and it is why the two panels were incomparable while one fitted a normal
to cover a parameter and the other quantiled a bootstrap root to contain a set.
