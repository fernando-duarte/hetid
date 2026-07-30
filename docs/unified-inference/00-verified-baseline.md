# Verified baseline for the unified-inference task

Stamped 2026-07-30 00:25 EDT. Every number below was recomputed read-only from the
working-tree artifacts on unmodified `main` (`b79e035`) with throwaway code that calls no
pipeline function, so it is an independent check rather than a restatement.

Sources (all gitignored working-tree artifacts, present but in no commit):
`scripts-paper/output/state/bootstrap_stage_draws.rds` (15.8 MB, 2026-07-30 00:01),
`scripts-paper/output/diagnostics/set_id_inference_diagnostics.csv`,
`scripts-paper/output/diagnostics/log_var_eq_set_inference_diagnostics.csv`.

## Draw-cache shape

`readRDS` gives seven top-level fields: `anchor`, `mean`, `volatility_primary`,
`volatility_primary_n_failed`, `volatility_sensitivity`, `volatility_sensitivity_n_failed`,
`provenance`. Provenance confirms the frozen resampling: `circular_mbb`, `sample_size = 256`,
`b_reps = 10000`, `block = 10`, `seed = 20260708`, `sens_block = 20`, `sens_reps = 10000`,
`index_sha256 = 5701196733f917...`.

- `mean$point_draws` is `10000 x 7`; `mean$endpoint_draws` has three slots (tau = 0.05, 0.10,
  0.20), each with `lower`, `upper`, `status` at `10000 x 7`. The status is **collapsed to one
  per coefficient**, which is the per-side threading gap.
- `volatility_primary$<estimator>` has **four** slots (tau = 0, 0.05, 0.10, 0.20), each with
  `lower`, `upper`, `lower_status`, `upper_status` at `10000 x 5`. `anchor$<estimator>` mirrors
  the four slots at the full sample. Per-side statuses already exist here.
- Slot 1 is the tau = 0 slot. Its `tau0_slot = 1L` invariant is set in
  `scripts-paper/inference/bootstrap_stage_draw.R:60`.

## Acceptance check 8.1 passes on unmodified main

An independent Target-S implementation (conservative order statistic
`ceiling((n+1)(1-alpha))` of `max{0, z_l, z_u}` over the both-sides-bounded pool, per-side MAD
scales over each side's own bounded pool) reproduces **all 30** published Panel B `c_value`s:

    max |c_s - c_value| = 2.22e-16      n compared = 30 of 30

So the construction in `log_variance/inference/set_envelope.R` is exactly §A.6-§A.7, and the
task is a relocation plus a second target, not a change to Target S.

## The tau = 0 slot is a point search pretending to be an interval

Confirms §5.2's diagnosis quantitatively, over all 50,000 draw-coefficient cells per estimator:

| estimator | max abs(upper-lower) | median | exactly equal | non-bounded share, both sides |
|---|---|---|---|---|
| ppml | 4.408e-03 | 1.625e-05 | 0 of 50,000 | 1.428% |
| harvey | 5.185e-03 | 2.110e-05 | 0 of 50,000 | 1.477% |

No cell is exactly degenerate, and the non-bounded share matches the brief's "about 1.4
percent". Every non-bounded tau = 0 side is `unreliable`; none is `unbounded`.

## Mean branch draw health

`n_failed = 0`, `n_point_deficient = 0`, and all 10,000 point draws are finite for all seven
coefficients. Collapsed endpoint statuses across `10000 x 7` cells:

| tau | bounded | unbounded | unreliable |
|---|---|---|---|
| 0.05 | 69,727 | 272 | 1 |
| 0.10 | 69,411 | 557 | 32 |
| 0.20 | 67,927 | 1,923 | 150 |

So the brief is right that the "all 10,000 draws deliver finite values" claim must not be
repeated for tau > 0, while it does hold for the tau = 0 point.

## Reference numbers in section 6 all reconcile

Recomputed Panel A widths, per-side scales, paired-finite counts and `c_stoye` match the
brief's table to displayed precision (`w/sigma` 0.93/0.79/0.52 at tau = 0.05, 1.71/1.45/0.93 at
0.10, 2.52/2.29/1.48 at 0.20; `c_stoye` 1.349/1.373/1.439, 1.289/1.299/1.349,
1.282/1.283/1.297; pooled counts 9909/9812/9341-9360).

The tau = 0 robust scales and implied t-statistics also match:

| coef | MAD scale | full-sample point | t |
|---|---|---|---|
| `b_0` | 0.055569527 | 0.795742496 | 14.3198 |
| `b_{1,E}` | 0.005263653 | 0.000925889 | 0.1759 |
| `b_{2,E}` | 0.131002357 | -0.471483954 | -3.5990 |
| `b_{3,E}` | 0.340106203 | 0.834091782 | 2.4524 |

The `b_N` block's scales are `0.008852422`, `0.079374323`, `0.308258602`, matching the brief's
`0.0088524`, `0.0793743`, `0.3082586` — these live only in the cache today, which is the
diagnostics schema gap §5.2 closes. Their sd/MAD ratios are 11.28, 19.52 and 10.26 against
1.02-1.14 for the point-identified block, which is the heavy-tail fact that makes the MAD
mandatory.

## Interior lambda genuinely binds — the certified optimizer is load-bearing

Target P was prototyped by certified Lipschitz branch-and-bound over `lambda` in `[0,1]` at
`target_p_lambda_tolerance = 1e-4`. Ordering `c_p_upper <= c_s` held in **every** cell of both
panels, with zero violations, and every gap came in under tolerance.

An interior `lambda` attains the supremum, by more than the tolerance above both endpoint
values, in **9 of 30** Panel B cells and **3 of 9** Panel A cells. An endpoint-only search
would therefore be wrong on a third of the table. The brief's adversarial case reproduces this
in miniature: with 84% zero roots, 8% `max(0, 1/4 - lambda)` and 8% `lambda` (realized by
`d_l = d_u = 1`, zero draws `z_l = z_u = 0`, kink draws `z_l = 1/4, z_u = 0`, ramp draws
`z_l = 0, z_u = 1`), the former `{0, 1/4, 1/2, 3/4, 1}` grid returns 0 at all five nodes while
the true supremum is `1/8` at `lambda = 1/8`. The branch-and-bound finds it in 6 evaluations
with a zero gap.

## Provisional magnitudes (will shift once per-side statuses are threaded)

Panel B moves very little, as predicted from `w/sigma` in 0.01-0.77 — the largest change is
`theta_{2,R}` at tau = 0.20, `c` from 1.231 to 1.168.

Panel A moves a lot, and mostly **wider**, because the bootstrap root distribution has far
heavier tails than the fitted bivariate normal it replaces:

| tau | coef | c_stoye (published) | c_s | c_p_upper |
|---|---|---|---|---|
| 0.05 | `b_{1,N}` | 1.349 | 2.118 | 1.853 |
| 0.05 | `b_{2,N}` | 1.373 | 2.622 | 2.315 |
| 0.05 | `b_{3,N}` | 1.439 | 3.381 | 3.376 |
| 0.10 | `b_{1,N}` | 1.289 | 1.753 | 1.413 |
| 0.10 | `b_{2,N}` | 1.299 | 2.073 | 1.685 |
| 0.10 | `b_{3,N}` | 1.349 | 3.188 | 3.179 |
| 0.20 | `b_{1,N}` | 1.282 | 1.184 | 0.936 |
| 0.20 | `b_{2,N}` | 1.283 | 1.321 | 0.963 |
| 0.20 | `b_{3,N}` | 1.297 | 2.283 | 2.281 |

Note the sign flip across tau: at tau = 0.05 the bootstrap demands a much larger critical value
than the normal fit, while at tau = 0.20 it demands a smaller one. That is the width credit
doing its job — at tau = 0.20 the set is wide relative to sampling noise (`w/sigma` up to 2.5),
so only one side can fail at a time and `c_P` falls toward the one-sided quantile, exactly the
§A.8 limit. These are provisional: threading per-side statuses changes Panel A's stored draws,
so the shipped numbers will differ slightly from this table.

## Consequence for sequencing

`support/identification` is a hashed **directory** in
`support/inference/bootstrap_stage_code_manifest.R:1-7`, and `config/analysis_contract.R` plus
`config/inference_search_control.R` are hashed **files**. Every edit this task requires
invalidates the 10,000-draw cache, so there is no cheap partial regeneration: development runs
read-only against the existing cache, and the single full run at the end regenerates everything.
