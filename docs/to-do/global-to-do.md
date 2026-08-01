## 2026-06-11 — Over-limit scripts files: dedicated split refactor (post-merge-train)

DONE 2026-06-12 on branch `feature/overlimit-scripts-split` (worktree
../hetid-split; plan docs/superpowers/plans/2026-06-12-overlimit-scripts-split.md).
All six files split by pure byte-proven code motion; capture gate (extended to
the copied identification_optimized RDS payloads) ALL IDENTICAL at tolerance 0;
extras diffs for the three blind-spot writers empty; suite 1219/0; sweep 21/21;
hooks 17/17 with no hook-induced rewrites; quality suite all-pass.

Ruled during the arbitrary-width-pipeline plan review (bounded growth now,
split later as its own gated change). Files (lines at 0faffc8 -> after that plan):
scripts/05_identification_with_optimization/spec_comparison.R (269 -> ~283),
scripts/02_identification_diagnostics/heteroskedasticity_tests.R (203 -> ~213),
scripts/05_identification_with_optimization/output_results.R (206 -> ~212),
scripts/06_results_production/create_tables_and_figures.R (225 -> ~230),
scripts/utils/profile_bounds.R (265, untouched),
scripts/run_all_scripts.R (286, untouched).
Recipe: extract helpers to scripts/utils/ (pure code motion, no behavior
change); run AFTER the support-mask/whitening/arbitrary-width/post-selection
merge train lands; verify with its own full pipeline run plus the
docs/baselines capture gate (tolerance 0).

## 2026-06-12 — Remaining over-limit scripts files (pre-existing; outside the six-file ruling)

Ten scripts files >= 200 lines at ce3c3e7, every one byte-unchanged since
0faffc8 (pre merge train) — pre-existing overage excluded from the 2026-06-12
six-file split (stages 01/03 emit dozens of figure/table artifacts wholly
outside the capture gate; folding them in would have multiplied the extras
surface of a tolerance-0 effort). A future dedicated split needs its own gate
design (per-writer extras snapshots for stage 01/02/03/06 artifacts):
scripts/01_data_analysis/time_series_properties.R (529),
scripts-paper/quality-check.R (343, tooling),
scripts/03_variance_bounds/output_results.R (334),
scripts/01_data_analysis/visualize_raw_data.R (292),
scripts/03_variance_bounds/compute_variance_bounds.R (262),
scripts/03_variance_bounds/analyze_bounds.R (258),
scripts/06_results_production/output_results.R (254),
scripts/02_identification_diagnostics/n_hat_episodes.R (252),
scripts/05_identification_with_optimization/analyze_optimization.R (208),
scripts/06_results_production/assemble_results.R (203).
Also noted: scripts/run_all_stage_list.R lands at 181 lines (~19 lines of
headroom for future stages).

## 2026-06-12 — Post-selection split study: RESOLVED (two failed rounds; machinery REMOVED)

The K<=4 re-validation round ran 2026-06-12 and FAILED at Stage D: premise
healthy at K {2,4}, degradation direction confirmed (optimized weights lose
0.32-0.48 coverage vs fixed at K=4), but the split REPAIR direction is absent
at both Stage-P passers (split coverage sits beside self-selection, 0.28-0.40
below fixed_e, 3-4 SE). Full sim never run per the registered stopping rule.
Two failed pre-registered rounds on record (K=8 premise; K<=4 directional) —
the split machinery was then REMOVED from the codebase (branch
feature/remove-split-study): scripts/post_selection/, postsel utils + tests,
README/NEWS/demo/header mentions all gone. The honest record lives ONLY in
docs/: removal note docs/postsel-split-study-removal-2026-06-12.md (start
here), the two pre-registrations, both failure reports, the pilot log, and
the adversarial reviews. Conceptual discussion + limitations go to the paper
body/appendix from those docs. Any third attempt needs a fundamentally
different design (pre-specified weight rule / more data per block /
multi-split aggregation) — never another grid rescope; the iso-rho rescale
is a proven dead end.
