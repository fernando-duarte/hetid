# Cross-run table acceptance

Updated: 2026-07-26 11:20 EDT

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

Run the direct comparison from the repository root:

```sh
Rscript --vanilla scripts-paper/validation/compare_output_tables.R \
  path/to/retained/scripts-paper/output \
  path/to/candidate/scripts-paper/output
```

Set `HETID_VALIDATION_RUN_ROOT` to retain the staged source, pipeline log,
candidate record, and `comparison-passed` marker at a chosen absolute path.
The chosen path must be new, empty, or carry the runner's private ownership
marker from an earlier validation. The runner rejects nonempty paths without
its ownership marker without changing their contents. It also rejects paths
equal to, above, or inside the repository. When no explicit run root is
supplied, `TMPDIR` must be an existing absolute directory with the same
repository-separation property; the runner checks it before creating a
temporary directory.

The runner validates and snapshots the explicit reference into a protected,
unique path before staging. The snapshot is used for the final comparison, so a
supported reference stored under the reusable run root cannot alias the
generated candidate or be changed by source staging. Supported locations
include `candidate.rds` and ordinary paths under `source` but outside
`source/scripts-paper/output`. For `candidate.rds`, or a hard-link alias with
the same file identity, the new candidate is retained beside the protected
snapshot.

References that collide with runner-managed state are rejected without changing
the original reference. Reserved locations are `pipeline.log`,
`comparison-passed`, the ownership marker, the private snapshot tree,
`preexisting-output`, and `source/scripts-paper/output`. Checks use file and
ancestor identity as well as path spelling, so hard links and case aliases do
not bypass them. Final-component symbolic-link references are always rejected.

The stale success marker is cleared for an authorized failed run unless the
supplied reference has the same file identity as `comparison-passed`. That sole
exception preserves the reference bytes; the collision is then rejected.
The compatibility Mac entrypoint delegates to the same clean runner:

```sh
bash docs/bootstrap-single-stage-refactor/validation-tools/run_mac_candidate.sh \
  path/to/reference-schema3.rds
```
