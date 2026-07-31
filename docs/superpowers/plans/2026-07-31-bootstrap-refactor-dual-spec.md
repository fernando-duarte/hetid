# Bootstrap cache/manifest split, dual specification, and unified inference references

**Created** 2026-07-31 00:42 EDT
**Branch** `feature/unified-bootstrap-inference` (worktree `/Users/fduarte/hetid-worktrees/unified-inference`)
**Base commit** `0d92ec3`
**Status** approved, ready to implement

---

## 1. What this changes and why

Five changes to the paper pipeline (`scripts-paper/`), all landing in one run:

1. **Delete the per-draw `tau*` search.** It costs 94% of every mean draw and reaches no
   published artifact.
2. **Split the bootstrap cache into primitives and derivations**, and split the code manifest
   into a draw hash (invalidates the cache) and a presentation hash (recorded, does not).
3. **Compute both specifications.** Spec B (sample `beta2R`) end-to-end and published; spec A
   (`beta2R = 0`) on the mean panel only, as a diagnostic.
4. **Make the tau = 0 column bootstrap-calibrated**, matching the tau > 0 columns. Retain the
   normal p-value for comparison.
5. **Unify the full-sample set solve with the per-draw solve**, including the `beta1` block,
   which is only reachable once spec B makes `beta1` set-valued.

The whole thing ends in one clean top-to-bottom run.

---

## 2. Background an implementer needs

### 2.1 The two specifications

The mean equation is

```
Y1 = X' beta1 + theta' Y2 + eps1        X = (1, lagged expected-SDF PCs), Y2 = SDF-news PCs
Y2 = beta2R X + eps2
```

`beta2R` is the population projection of the news PCs on `X`. Under exact SDF news it is zero.
`scripts-paper/config/analysis.R:33` currently forces that with `impose_beta2r_null <- TRUE`.

| | spec A (today) | spec B (new default) |
|---|---|---|
| `beta2R` | forced to 0 | estimated from the sample |
| `W2` | raw `Y2` | `Y2` residualised on `X` |
| `b_0`, `b_E` | point-identified at every tau | set-valued via `beta1(w) = beta1R - t(beta2R) w` |

**The evidence against the restriction is real but narrower than an iid F suggests.** An
initial `F(3,252)` gave PC3 `p = 5.4e-04`, but that reference assumes iid errors on quarterly
macro data. Re-tested with a Newey-West Wald (lag 4) and two null-imposed block bootstraps
(block 10, B = 20,000):

| news PC | HAC Wald | analytic p | **wild block p** |
|---|---|---|---|
| `sdf_news_pc1` | 0.65 | 0.886 | 0.836 |
| `sdf_news_pc2` | 1.30 | 0.730 | 0.794 |
| `sdf_news_pc3` | 19.76 | 1.9e-04 | **0.0014** |
| **joint (matrix)** | 21.70 | 9.9e-03 | **0.0425** |

Null-imposed wild block bootstrap, B = 20,000, block 10: residual signs are flipped by block with
each residual kept paired to its own `X_t`, which imposes `E[u | X] = 0` while leaving the
design alignment and conditional heteroskedasticity intact. PC3 rejects decisively and the joint
matrix restriction rejects at 5%.

Note on what the DGP must impose, since it is easy to garble. The pretest asks whether spec A's
restriction holds, so the resampling has to satisfy `beta2R = 0` by construction — that is what
makes the p-value a p-value, and it is unrelated to whether estimation residualizes. But spec A
asserts only that the *linear projection* vanishes. An earlier attempt resampled the restricted
residuals independently of `X`, which imposes full independence and therefore destroys nonlinear
and conditional-variance dependence that spec A permits. That reference was too wide and put the
joint at 0.092 instead of 0.043. Do not reintroduce it.

Remaining caveat: at T = 256 and block 10 the wild scheme draws only 26 independent signs per
replicate, which can narrow its reference. The PCs are treated as observed data throughout, by
decision.

Spec B therefore does **not** rest on the pretest. Its primary justification is
`docs/lewbel_multivariate_set_identification.tex`, §"News-equation reduced-form coefficients":
in proxy or finite-sample implementations, "estimating the projection on the full `X_t` is still
the correct residualization step" *even when* `beta2R = 0` holds in population. The pretest
points the same way for PC3, which is the component carrying `b_{3,N}`, but is not load-bearing.

This ambiguity is also why the plan computes **both** specifications rather than choosing one.

**Measured consequences of switching** (mean panel, B = 10,000, real index family):

- Sets narrow. `b_{3,N}` at tau = 0.20 goes from width 0.753 to 0.636.
- Confidence intervals mostly narrow. `b_{3,N}` at tau = 0.05: `[-1.699, 0.580]` to
  `[-1.226, 0.309]`, a 33% narrowing.
- **No significance verdict changes.** 21 cells, identical conclusions under both specs.
- Variance shares barely move for the news block (5.217% to 5.174% at tau = 0) but `PC_E`'s
  share becomes a range instead of a constant.

### 2.2 Why the tau = 0 reference must change

`scripts-paper/support/identification/endpoint_point_statistic.R` currently ends with

```r
p_value = 2 * stats::pnorm(-abs(statistic))
```

After change 4 the tau > 0 columns are bootstrap-calibrated (Target P) while tau = 0 would stay
normal — the same two-reference defect the previous unification removed, relocated from
Panel A vs Panel B to tau = 0 vs tau > 0. It lands on the weaker side.

Be careful about *why*. The raw excess kurtosis of the draws (9375 at tau = 0.05 for `b_3N`) is
**not** a distributional property: it is eight bad solves. One draw's lower endpoint sits at
-1550 against a full-sample bound of -0.544, and dropping that single draw takes kurtosis to 783.
Those draws do not reach the published critical value — `c_s` moves 3.364 to 3.343 when all eight
are removed, and the PRW rank sits at 8919 of 9909, far below them.

The real reason is that the root distribution is heavy through its **body**:

```
root quantiles   0.57   1.35   3.36   5.39   12.44   142.19
                 50%    75%    90%    95%    99%     max
```

The 90th percentile is 3.36 against the normal's 1.645, independently of the outliers. That is
what a normal reference discards, and it is why the machinery uses MAD and the conservative
Politis-Romano-Wolf order statistic. (4.4% of roots are exactly zero — draws wider than the
full sample on both sides. That atom grows as sets widen under spec B, which is why `c_S` falls
there.)

**The fix is the tau -> 0 limit of the existing machinery, not a new construction.** At a
point-identified cell `L = U = theta_hat`, so `z_U = -z_L` and the Target S/P root collapses:

```
R_S = max{0, z_L, z_U} = |z_L|
```

which is why `c_p_upper == c_s` to the digit on every degenerate row in the current diagnostics
(1.689/1.689, 1.844/1.844, 1.704/1.704, 1.698/1.698). So the tau = 0 p-value is the empirical
tail of `|z*|` with `z*_b = (point*_b - point_hat) / se`, reusing the `se` that
`point_t_statistic` already computes.

Two precision points. This is an **exact specialization at tau = 0**, not a proved `tau -> 0`
limit — active sets and status maps can be discontinuous, so do not claim continuity. And the
scale **cancels**: comparing `T*_b >= T_obs` is equivalent to `|point*_b - point_hat| >= |point_hat|`,
so the p-value is scale-free and should not be described as studentised. Use the finite-B rule
`p = (1 + sum(T* >= T_obs)) / (|valid| + 1)`, and report both directional tails alongside it,
since an absolute-deviation test is tail-unbalanced under skew.

**Measured consequence:** 5 of 17 tau = 0 stars change — `b_{1,N}` (`*` to none), `theta_0` PPML
(`***` to `*`), `theta_{4,R}` PPML (`**` to none), `theta_0` Harvey (`**` to `*`),
`theta_{4,R}` Harvey (`**` to none).

**The OLS column keeps classical stars** by decision — it is the benchmark showing what standard
tools conclude. That means one table carries two reference distributions on purpose and **must
carry a note saying so**.

### 2.3 The full-sample/per-draw solve mismatch

`scripts-paper/mean_equation/estimate_identified_set.R` solves the set twice:

```r
# coef_interval_tables starts every profile solve at the origin and can settle
# on a local vertex short of the true extreme
refined_theta <- set_id_display_tau_refinement(...)
for (j in seq_along(set_tables)) set_tables[[j]]$theta <- refined_theta[[j]]
```

The refinement re-solves with a box multistart and only ever widens. But each bootstrap draw goes
through `set_id_boot_geometry` -> `coef_interval_tables_from_quadratic`, the *unrefined*
origin-start path. So the studentised root

```
z_L = (L*_unrefined - L_refined) / s_L
```

mixes two solve recipes. Today this is inert — the two agree to better than 1e-8. Note what that
does and does not establish: the refinement certifies feasible points, so agreement means it
found no additional certified-feasible point outside the origin-start interval. It does **not**
certify a global extremum. Two multistart procedures can enter the same basin and agree exactly
while missing another component; this is a non-convex QCQP and the profile solver is local.
Report these as algorithmic bounds, never as proved identified-set extrema.

**Two reasons to fix now.** The draw path and full-sample path are both being touched anyway.
And the refinement replaces only `$theta`, never `$beta1` — harmless while `b_E` is a point under
spec A, but under spec B `b_E`'s bounds come from `solve_linear_functional_bound` over the
*unrefined* solve while `theta`'s cell is refined, so the two blocks in one table would report
over different sets. That is a spec-B correctness bug, not tidiness.

### 2.4 tau grid — unchanged

The bootstrap grid stays `c(0.05, 0.10, 0.20)`. Margins against the 0.85 stability gate:

| tau | share bounded | margin |
|---|---|---|
| 0.20 | 0.9340 | +0.0840 |
| 0.25 | 0.8588 | +0.0088 |
| 0.254 | 0.8504 | +0.0004 |

`q_0.15(tau*) = 0.2543` is the exact ceiling. 0.25 was considered and rejected as too marginal.

The bounds-by-tau **figures already span 0 to tau\* (~0.415)** because
`config/tau_grid.R` builds their grid from `tau_star`, not from
`PAPER_ANALYSIS_CONTRACT$tau$display`. So no display-grid change is needed for the plots to cover
0.3 and 0.4, and no set-only cell shape has to be designed. An appendix table at those taus is
deferred.

### 2.5 What per-draw tau* feeds

Only `tau_star_band`, `tau_star_share_bounded` and `n_capped`, which reach `cat()` output and
nothing else. Verified: no file under `scripts-paper/output/` contains `tau_star`. The
**full-sample** `tau*` at `estimate_identified_set.R:62` stays — it builds the figure grid
(`paper_bounds_tau_grid`) and `render_region_3d.R:21` asserts against it.

---

## 3. Invariants that must hold throughout

- **Both panels share one implementation.** `endpoint_target_table` and `point_t_statistic` are
  called by the mean side (`boot_results.R:28,40`) and the volatility side
  (`bootstrap_stage_result_helpers.R:64`, `set_bootstrap_builders.R:78`). Neither panel may grow
  its own variant. New construction families go in the shared module.
- **Spec A and spec B share one index family.** Same resamples, different estimator, so the
  comparison is paired. A and B cannot share *draws* — residualising `W2` changes the moments and
  hence the sets — but they must share indices.
- **Sensitivity scope:** primary + doubled-block sensitivity for spec B; primary only for spec A.
- **No new bootstrap randomness.** `circular_mbb`, B = 10,000, block 10, seed 20260708 unchanged.
- **Deliberate redundancy stays.** `n_failed`, `n_point_deficient` and similar derivable
  aggregates are integrity cross-checks, not waste. Keep them and keep their invariants.
- **Simplicity is a deliverable.** No speculative extensibility, no elaborate gates beyond what
  is specified here.
- **Never bypass pre-commit hooks.** No `--no-verify`, no loosening linter config.
- **Files under 200 lines, lines under 100 characters.**
- Paper-facing text says "tolerance"/"validity tolerance", never "slack". Existing identifiers
  containing "slack" may stay.

---

## 4. Tasks

Ordered. The mechanical refactor is verified inert *before* any semantic change, so the final
run's diff is attributable.

### Phase I — mechanical, must be numerically inert

**T1. Delete the per-draw tau\* search.**

Remove the `sweep_fixed_gamma` + `tau_star_fixed` block from
`support/identification/identified_set_bootstrap.R` (~lines 91-102) and the `tau_star` / `capped`
return fields. Then follow the chain: `identified_set_bootstrap_collect.R:46-47`,
`bootstrap_stage_mean_cache.R:23,98-102,104,150-159`, `mean_equation/inference/boot_results.R:58-60`,
`inference/bootstrap_stage_results.R:34-36,43,45`. Update the test fixtures and schema lists
(`mean_boot_results_schema_checks.R:9,11`, `mean_boot_results_checks.R:45-46,105-106`,
`bootstrap_stage_cache_payload_checks.R:67-68,83,101-102`, `mean_set_bootstrap_checks.R:68`,
`bootstrap_stage_draw_contract_checks.R:26`).

Watch the cross-field invariant `identical(is.na(tau_star), failed_mask)` — it couples the tau*
NA pattern to the failure mask, so the failure bookkeeping must keep working without it.

*Verify:* full suite green; `AuditCacheSchema` list fully consumed.

**T2. Split the cache into primitives and move the derivations out.**

Cache keeps only: per-tau `lower`, `upper`, `lower_status`, `upper_status`; `point_draws`,
`point_status`; and the failure-count fields as cross-checks. Everything else moves to a
post-bootstrap layer.

**Partition, verified against the expanded 101-file manifest.**

Move to a new directory outside the draw manifest (proposed
`scripts-paper/support/inference_post/`) — six of the twenty `support/identification` members,
all consumed only by post-draw code:

```
endpoint_targets.R          endpoint_point_statistic.R   inference_calibration.R
endpoint_target_cells.R     identified_set_inference.R   set_id_diagnostics_rows.R
```

The other fourteen stay: the profile solvers, quadratic system, status contract, moments,
`tau_star.R` (still needed full-sample) and the bootstrap draw itself.

**One file must be SPLIT, not moved.** `log_variance/inference/set_bootstrap_builders.R` is
genuinely mixed:

| function | called from | side |
|---|---|---|
| `logvar_set_boot_builders` | `inference/bootstrap_stage_draw.R:4` | DRAW |
| `logvar_boot_point_t` | `inference/bootstrap_stage_results.R:79` | PRESENTATION |

Moving the file wholesale would pull a draw-time builder out of the invalidation set, which is
exactly the error the two-tier hash is meant to prevent. Split it and move only
`logvar_boot_point_t`.

Audit every remaining manifest entry the same way before assuming its side — the file name is
not evidence, as this one shows.

*Verify:* full suite green.

**T3. Tier the code hash.**

`bootstrap_stage_code_manifest.R` gains a draw manifest and a presentation manifest.
`bootstrap_stage_provenance.R` (~lines 30, 79) records `draw_code_sha` and
`presentation_code_sha`; only the former participates in cache invalidation. Update
`BOOTSTRAP_STAGE_PROVENANCE_FIELDS`, `BOOTSTRAP_STAGE_SHA_FIELDS`, the sha pattern, the cache
schema version, and the provenance fixtures.

Take `AuditManifest`'s partition as the specification and its risk assessment as the guide for
what the presentation hash must still guarantee.

*Verify:* full suite green.

**T4. NEUTRALITY GATE — blocking.**

Feed the **existing** cache (`scripts-paper/output/state/bootstrap_stage_draws.rds`, produced by
pre-refactor code) through the **new** post-draw layer and confirm it reproduces every currently
published number to machine precision. Pattern already proven — see the earlier
`replay_postboot.R` approach. It must bypass schema validation, since the tau* fields are gone.

Compare against the committed `.tex` artifacts and `set_id_inference_diagnostics.csv`.

**If this gate fails, stop and diagnose before any Phase II work.** Its whole purpose is to
separate "the refactor was inert" from "a semantic change moved a number".

### Phase II — semantic changes

**T5. Unify the set solve, including `beta1`.**

Make the full-sample and per-draw paths use the same recipe. Extend
`set_id_display_tau_refinement` (or its call site) to refresh `$beta1` as well as `$theta`, so
under spec B both blocks are reported over the same set.

*Note:* this is genuinely new behaviour with unmeasurable-in-advance magnitude, because it is
masked under spec A. Record the movement in the final report.

**T6. Bootstrap-calibrated tau = 0.**

In `endpoint_point_statistic.R`, replace `2 * stats::pnorm(-abs(statistic))` with the empirical
tail of `|z*|`, `z*_b = (point*_b - point_hat) / se`, using the already-computed `se`. Retain
the normal p-value as a comparison column. Both panels inherit this automatically.

*Verify:* the 5 expected star changes from §2.2, and no others.

**T7. Dual specification.**

Replace the single global `impose_beta2r_null` with a configuration supporting A-only, B-only, or
both, independently for the mean and volatility panels. Default: B end-to-end, A mean-only.
Both specs draw from one shared index family. Sensitivity family for B only.

Take `AuditGridSpec`'s single-spec and point-identified-`beta1` enumerations as the specification
for what must be de-singletonised (artifact registry, state files, globals, figures, tables,
diagnostics).

**T8. Draw-independent interval families.**

Compute and store, for comparison: Target P (published), Target S, percentile, basic, normal.
All are pure functions of the stored endpoint arrays. Studentised/bootstrap-t is **out of scope**
— it needs a per-draw scale and hence a nested bootstrap.

**T9. Feasibility assertions.**

For each requested bootstrap tau, check realised `frac_lower`/`frac_upper` against
`stability_share` and fail with an informative error naming the tau, the share and the threshold
— rather than silently blanking a cell.

**T10. Table note on mixed references.**

The main table's tau = 0 stars are bootstrap-calibrated while the OLS column's are classical.
Note this wherever the table's notes live.

### Phase III — run and verify

**T11. Real-settings guard.** Assert `HETID_BOOT_REPS` is unset or 10000 and that quick mode is
off before the run, so a quick-mode run cannot silently overwrite tracked `.tex`.

**T12. Clean artifacts and run top to bottom.** Expect ~5-6h: spec B end-to-end plus spec A
mean-only.

**T13. Verify and report.** Full suite, `R CMD check`, and a written account of every number that
moved with its attributed cause. Push the branch. **Do not merge to main** — that is the user's
call.

---

## 5. Verification gates

| Gate | When | Criterion |
|---|---|---|
| Suite | after each task | all suites pass, exit 0 |
| Neutrality | after T3 | old cache through new layer reproduces published numbers to machine precision |
| Star delta | after T6 | exactly the 5 changes in §2.2 |
| Spec delta | after T7 | mean-panel A-vs-B matches the measured table in §2.1 |
| Package | before run | `R CMD check` 0 errors |
| Run | T12 | clean exit at real settings |

## 6. Explicitly out of scope

- Nested bootstrap / bootstrap-t (needs per-draw scale; deferred indefinitely).
- Appendix table at tau = 0.3 / 0.4 (plots already cover the range).
- Merging to main.
- New bootstrap draws, resampling schemes, index families, seeds, or B.
