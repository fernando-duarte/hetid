# Bootstrap validation compatibility tools

Updated: 2026-07-26 10:06 EDT

These tools are compatibility wrappers around the canonical schema-3 owners in
`scripts-paper/validation/`. They do not define numeric-token parsing,
precision quanta, record validation, or rounding-overlap comparison.

Current acceptance uses only numeric results in final TeX tables and their
attached significance stars. It requires identical table paths, numeric
coordinates, and token counts. A numeric result that appears, disappears,
becomes non-numeric, or moves fails. Non-numeric content on both sides and all
non-table output are ignored.

The retained schema-2 RDS and the earlier rerun/reuse comparisons are
historical evidence. Schema 2 is not accepted because it does not preserve
stars. Do not overwrite the historical RDS. Recapture a schema-3 reference
from the retained TeX tables with the canonical command documented in
`scripts-paper/validation/README.md`.

Capture retained legacy output only to an explicit, distinct schema-3 path:

```sh
bash capture_legacy_reference.sh path/to/reference-schema3.rds
```

Calling the helper without a destination fails. Passing
`baseline-artifacts/fresh-legacy-scientific-record.rds` also fails before
capture so the historical schema-2 evidence cannot be overwritten.

Run a Mac candidate only with an explicit schema-3 reference:

```sh
bash run_mac_candidate.sh path/to/reference-schema3.rds
```

The wrapper delegates to the clean runner. It stages an empty output tree,
runs the pipeline once, and compares the candidate with the explicit
reference.

Run compatibility regressions from the repository root:

```sh
Rscript docs/bootstrap-single-stage-refactor/validation-tools/test_pipeline_expression.R
Rscript docs/bootstrap-single-stage-refactor/validation-tools/test_scientific_record.R
bash docs/bootstrap-single-stage-refactor/validation-tools/test_capture_legacy_reference.sh
```
