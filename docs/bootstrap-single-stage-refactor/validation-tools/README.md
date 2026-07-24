# Universal validation tools

These tools implement the single cross-platform acceptance rule. They
do not launch a bootstrap until explicitly invoked.

The scientific comparator recursively removes only enumerated operational
metadata and then calls:

```r
all.equal(
  reference,
  candidate,
  tolerance = 1e-4,
  check.attributes = TRUE
)
```

The tracked owner is
`scripts-paper/tests/support/scientific_comparison.R`. Statuses, names,
classes, dimensions, missingness, infinities, draw counts, schedules, and
scientific provenance remain comparable.

Capture the completed legacy reference without drawing again:

```sh
bash capture_legacy_reference.sh
```

Run the deferred Mac candidate after user authorization:

```sh
bash run_mac_candidate.sh
```

The candidate script launches exactly one 10,000-draw unified bootstrap. Its
second pipeline pass is strict cache reuse: an invalid cache stops immediately
instead of falling back to another bootstrap. It leaves two scientific records
and logs in an isolated temporary directory. The Mac core count is not
overridden, so production reserves two logical CPUs.

The capture driver optionally receives a legacy gate record through
`HETID_GATE_REFERENCE_RDS`. It permits a runtime-only rebind of the existing
non-rejection decision after the old and fresh gate records match under the
universal scientific rule with only their serialized sample ID and commit
removed. This accommodates the documented R-version-dependent sample hash
without changing tracked scientific configuration.

Run the cross-version pipeline-expression regression directly:

```sh
Rscript test_pipeline_expression.R
```
