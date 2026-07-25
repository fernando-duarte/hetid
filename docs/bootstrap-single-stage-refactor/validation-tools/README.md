# Published-table validation tools

These tools implement the single cross-platform acceptance rule. They
do not launch a bootstrap until explicitly invoked.

The comparator reads every TeX file below `scripts-paper/output/tables` and
checks only numeric result cells present in both runs. Each printed token
defines a rounding interval from its displayed precision. Two tokens agree
when those intervals overlap; adjacent tokens printed at the same precision
do not agree.

Table paths must match. Table structure, prose, significance stars, missing
markers, and statuses such as `unreliable`, `unbounded`, and `--` are ignored.
A cell is also ignored when it is nonnumeric on either side or exposes a
different count of numeric tokens.

The tracked owner is
`scripts-paper/tests/support/published_table_comparison.R`. Raw bootstrap
draws, diagnostics, caches, public R objects, statuses, and provenance are not
cross-platform acceptance inputs.

Capture the completed legacy reference without drawing again:

```sh
bash capture_legacy_reference.sh
```

This regenerates the schema-2 table record. The older schema-1 record is not
accepted by the current runner.

Run a Mac candidate after user authorization:

```sh
bash run_mac_candidate.sh
```

The candidate script launches exactly one 10,000-draw unified bootstrap. Its
second pipeline pass is strict cache reuse: an invalid cache stops immediately
instead of falling back to another bootstrap. It leaves two table records
and logs in an isolated temporary directory. The Mac core count is not
overridden, so production reserves two logical CPUs. Before launching, it
requires a schema-2 legacy table record so an obsolete baseline cannot waste a
long run.

The capture driver optionally receives a legacy gate record through
`HETID_GATE_REFERENCE_RDS`. It permits a runtime-only rebind of the existing
non-rejection decision after the old and fresh gate records match under the
internal scientific rule with only their serialized sample ID and commit
removed. This accommodates the documented R-version-dependent sample hash
without changing tracked scientific configuration. That runtime guard is
separate from the final cross-platform table acceptance gate.

Run the cross-version pipeline-expression regression directly:

```sh
Rscript test_pipeline_expression.R
```
