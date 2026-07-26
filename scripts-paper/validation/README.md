# Cross-run table acceptance

Updated: 2026-07-26 10:06 EDT

Cross-run acceptance is decided only by numeric results printed in final TeX
tables and the significance stars attached to those results. Table paths,
numeric coordinates, and token counts must match, and every table must contain
at least one numeric result.

All non-table output is ignored. Table content that is non-numeric in both
records is also ignored. Acceptance fails when a numeric result becomes
non-numeric, appears, disappears, moves to another coordinate, changes beyond
its displayed rounding interval, or has different attached stars.

The reference record is always explicit. A candidate run is staged with an
empty `scripts-paper/output` tree and executes the pipeline once. Historical
schema-2 records are obsolete for current acceptance because they do not store
significance stars. Recapture a schema-3 reference from the retained TeX tables
before a new acceptance run; do not modify the historical schema-2 RDS.

When capturing from the retained legacy output through the compatibility
helper, provide a distinct schema-3 destination explicitly:

```sh
bash docs/bootstrap-single-stage-refactor/validation-tools/capture_legacy_reference.sh \
  path/to/reference-schema3.rds
```

The helper rejects the historical schema-2 path before reading table output.

Run these commands from the repository root.

Capture a schema-3 reference from retained output:

```sh
Rscript --vanilla scripts-paper/validation/capture_table_record.R \
  path/to/retained/scripts-paper/output \
  path/to/reference-schema3.rds
```

Compare two existing schema-3 records:

```sh
Rscript --vanilla scripts-paper/validation/compare_table_records.R \
  path/to/reference-schema3.rds \
  path/to/candidate-schema3.rds
```

Run one clean candidate pipeline and compare it with the explicit reference:

```sh
bash scripts-paper/validation/run_clean_validation.sh \
  path/to/reference-schema3.rds
```

Set `HETID_VALIDATION_RUN_ROOT` to retain the staged source, pipeline log,
candidate record, and `comparison-passed` marker at a chosen absolute path.
The compatibility Mac entrypoint delegates to the same clean runner:

```sh
bash docs/bootstrap-single-stage-refactor/validation-tools/run_mac_candidate.sh \
  path/to/reference-schema3.rds
```
