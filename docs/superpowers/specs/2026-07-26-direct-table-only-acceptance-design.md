# Direct Table-Only Acceptance Design

Recorded: 2026-07-26 13:28 EDT

Status: Approved

## Purpose

Make displayed numbers and attached significance stars in final TeX tables the
only cross-run acceptance inputs for `scripts-paper`.

The comparator reads two existing output directories. It does not run the
pipeline, serialize a reference, inspect intermediate artifacts, or enforce a
separate reproducibility protocol.

This design supersedes:

- `2026-07-25-pipeline-table-only-acceptance-design.md`;
- `../plans/2026-07-25-pipeline-table-only-acceptance.md`.

## Scope boundary

Remove the external acceptance and reproducibility layer:

- schema-versioned table records;
- RDS capture, round-trip, and record-validation checks;
- clean-run staging and forced bootstrap settings;
- reference snapshots and success markers;
- path-ownership and alias checks created for the clean runner;
- legacy Mac, capture, and scientific-record acceptance commands;
- tests and documentation that enforce those mechanisms.

Keep producer-internal scientific and runtime safeguards. This change does not
alter `run_pipeline.R`, estimator reliability checks, dependency decisions,
cache validation, scientific assertions, or ordinary artifact serialization
used by the computation itself.

## Public interface

The acceptance layer exposes one command:

```sh
Rscript --vanilla scripts-paper/validation/compare_output_tables.R \
  path/to/reference/scripts-paper/output \
  path/to/candidate/scripts-paper/output
```

Both arguments name existing output roots. The command reads `.tex` files
recursively under each root's `tables/` directory.

The command exits zero when all projected numeric results match. It exits one
and prints targeted differences when they do not match. Invalid command
arguments or unreadable input paths produce ordinary command errors.

No acceptance command runs the pipeline. A caller that needs fresh output runs
the pipeline separately.

## Architecture

Retain three focused modules under `scripts-paper/validation/`:

- `table_tokens.R` parses displayed numeric tokens, precision quanta, and
  attached stars.
- `table_projection.R` maps final TeX tables to in-memory numeric projections.
- `table_comparison.R` compares two projections and reports differences.

Add `compare_output_tables.R` as the sole command-line entrypoint.

Remove:

- `table_record.R`;
- `capture_table_record.R`;
- `compare_table_records.R`;
- `run_clean_validation.sh`;
- acceptance-only compatibility and scientific-record tools under
  `docs/bootstrap-single-stage-refactor/validation-tools/`;
- acceptance-only wrappers under `scripts-paper/tests/support/`.

Historical reports, archives, and retained evidence remain unchanged. Active
code must not read them or treat them as acceptance inputs.

## Data flow

The command performs one in-memory flow:

```text
reference output root -> TeX discovery -> numeric projection
candidate output root -> TeX discovery -> numeric projection
numeric projections   -> displayed-value and star comparison -> exit status
```

The projection identifies each result by relative TeX path, tabular block, row,
column, and token position. These coordinates pair corresponding numbers; no
separate structural or reproducibility gate examines them.

The comparator writes no records, caches, snapshots, logs, manifests, or
markers.

## Acceptance semantics

Acceptance depends only on projected numeric tokens and their attached stars.

A candidate fails when:

- a reference numeric token is missing;
- the candidate adds a numeric token;
- a numeric token moves to a different coordinate;
- corresponding cells contain different token counts;
- displayed values do not overlap at their printed precision;
- attached significance stars differ.

A candidate passes through differences in:

- table labels, headers, captions, notes, and prose;
- nonnumeric statuses and missing-value markers when neither side contains a
  number at that coordinate;
- TeX files that contain no projected numeric results;
- CSV, RDS, Markdown, PDF, SVG, logs, diagnostics, caches, provenance, and all
  other non-table output.

Relative table paths and cell coordinates serve only to pair numeric tokens. An
extra or missing nonnumeric-only table has no effect. An extra or missing table
with numeric tokens changes the numeric projection and fails.

The comparator imposes no minimum number of tables or numeric tokens. Two empty
numeric projections compare equal when both output roots and their `tables/`
directories are readable.

## Precision and stars

Preserve the established PPML-style numerical rule:

- parse each displayed token as a numeric value;
- infer its precision quantum from the printed form;
- accept two values when their rounding intervals overlap, subject to
  floating-point slack;
- compare `""`, `*`, `**`, and `***` exactly.

Continue to support signed decimals, leading-decimal notation, scientific
notation, and TeX `\times 10^{...}` notation.

## Error handling

Command errors are limited to conditions that prevent comparison:

- wrong argument count;
- missing or unreadable output roots;
- missing or unreadable `tables/` directories;
- unreadable TeX files.

These errors do not inspect pipeline provenance or intermediate state. The
comparison has no schema, serialization, clean-run, cache, dependency, or
process-success prerequisites.

## Tests

Retain focused tests for:

- numeric token parsing;
- precision quanta;
- significance stars;
- TeX scientific notation;
- projection coordinates;
- displayed-precision overlap;
- missing, added, and moved numeric tokens;
- token-count and star differences;
- ignored nonnumeric TeX content;
- ignored nonnumeric-only tables;
- ignored non-table artifacts;
- the command's zero and nonzero exit behavior.

Delete tests for:

- schema versions and record fields;
- RDS capture and round trips;
- exact serialization ownership in the acceptance layer;
- clean-run staging and forced bootstrap settings;
- reference snapshots, markers, ownership, path collisions, and aliases;
- legacy acceptance wrappers.

The mutation matrix must demonstrate that only displayed numbers and stars
change the acceptance result.

## Documentation

`scripts-paper/README.md` and `scripts-paper/validation/README.md` document the
single direct-comparison command. They must not instruct users to capture
records, stage clean runs, set acceptance-specific environment variables, or
invoke legacy wrappers.

Historical bootstrap-refactor reports may describe earlier methods as
historical. Their current-status sections must point to the direct comparator
and must not present serialized records or clean-run tooling as active
acceptance requirements.

## Completion criteria

The simplification is complete when:

- one active acceptance CLI compares two output roots directly;
- no active acceptance path reads or writes RDS;
- no active acceptance path runs the pipeline or inspects intermediate output;
- no schema, record, capture, clean-run, marker, or compatibility gate remains;
- producer-internal scientific and runtime safeguards remain unchanged;
- focused tests and the paper test harness pass;
- the mutation matrix proves that only final table numbers and stars decide
  acceptance.
