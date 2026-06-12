# Post-Selection Split Study

Selection-honest handling of data-selected weights: select the
width-minimizing weights on one contiguous temporal block, evaluate
the identified set on the other, so the evaluated bound uses weights
fixed relative to the evaluation sample (the spec's fixed-Lambda
inclusion applies without uniformity over the admissibility class).
None of these scripts is part of the default pipeline; nothing here
is registered in `run_all_scripts.R`, and the default pipeline's
artifacts are unaffected.

Run order (from the package root, after one full pipeline run has
created `scripts/output/temp/data.rds`):

```
Rscript scripts/post_selection/run_split_study.R
Rscript scripts/post_selection/split_simulation.R   # heavy; see below
Rscript scripts/post_selection/postsel_report.R
```

Environment knobs:

- `HETID_SPLIT_GAP` (default 4): gap quarters between the blocks.
- `HETID_SPLIT_TAU` (default `BASELINE_TAU`): the slack, applied
  identically to both blocks; never data-selected.
- `HETID_SIM_QUICK=1`: smoke-grid simulation (minutes). Quick
  artifacts carry a `_quick` suffix and never clobber full-run ones.
- `HETID_SIM_REPS`, `HETID_SIM_CORES`: replication count and worker
  override for the simulation.
- `HETID_POSTSEL_SIM_SOURCE`: point the report at a saved simulation.

Honesty contract: the report prints the solver's three-state
vocabulary per profile side (bounded / unbounded /
no-certified-bound) and never pools widths across evaluation windows.
The affirmative selection-honest wording is emitted only alongside a
passing FULL simulation verdict (mechanically gated in the report
text layer); without one the summary says NOT validated, and a
failing full-simulation verdict makes the report process itself exit
nonzero after writing its artifacts. Residual caveats (serial
dependence across the block boundary; the maintained bound at the
selected weights) are stated in every summary.
