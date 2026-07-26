# Pipeline Table-Only Acceptance Design

Recorded: 2026-07-25 23:05 EDT
Updated: 2026-07-26 08:42 EDT

## Purpose

Replace every cross-run acceptance rule for `scripts-paper` with one rule:
accept a completed candidate when every displayed numeric result and its
significance stars in the final TeX tables agree with the reference.

The acceptance decision ignores all other output. Figures, reports,
diagnostics, caches, R objects, provenance, hashes, runtime metadata, console
text, table prose, statuses, and missing-value markers do not affect it unless
they replace a displayed numeric result.

## Scope

This change covers the entire paper pipeline. It generalizes the table-number
comparison developed for the single-stage bootstrap workflow and makes
`scripts-paper/validation/` its sole production owner.

Estimator checks, dependency decisions, routing, cache validation, and
scientific assertions may still execute while the pipeline builds its output.
The acceptance layer does not inspect or compare their internal records.

## Acceptance contract

A candidate run must satisfy these prerequisites:

- The runner stages source in an isolated temporary directory.
- The staged copy contains no prior `scripts-paper/output` content.
- The pipeline process exits successfully.
- The completed output contains a `tables` directory with TeX tables.
- The reference and candidate records are syntactically valid.

After these prerequisites, acceptance depends only on table results:

- Reference and candidate contain the same relative `.tex` table paths.
- Reference and candidate contain the same numeric cell coordinates and token
  counts.
- Every table has at least one numeric result token.
- Corresponding numeric tokens agree at their displayed precision.
- Significance stars attached to corresponding numeric tokens match exactly.

The comparison ignores:

- table labels, headers, ordering prose, notes, and captions;
- table structure outside corresponding numeric cells;
- nonnumeric statuses such as `unreliable`, `unbounded`, and `--`;
- missing markers and cells that are nonnumeric on both sides;
- every file outside `output/tables`, including table PDFs.

A numeric cell may not disappear into a missing marker or status. A numeric
cell that appears, disappears, moves, or changes its token count fails the
comparison. These coverage checks ensure that the gate compares every final
table number rather than a surviving subset.

## Displayed-precision rule

Each numeric token defines a rounding interval from its displayed precision.
The parser supports ordinary decimal notation, scientific `e` notation, and
LaTeX `\times 10^{}` notation.

Two tokens agree when their rounding intervals overlap. The comparison uses a
small machine-precision boundary allowance so adjacent values at the same
displayed precision fail instead of passing through binary rounding noise.

A significance marker belongs to the numeric token immediately before it.
The canonical forms are no stars, `*`, `**`, and `***`, including the current
TeX spelling `$^{***}$`. Numeric equality cannot compensate for a star
difference.

## Canonical modules

Create focused files under `scripts-paper/validation/`:

- `table_tokens.R` parses numeric tokens, precision quanta, and attached stars.
- `table_projection.R` projects final TeX tables into comparable cells.
- `table_record.R` builds and validates serialized table records.
- `table_comparison.R` compares validated records and reports differences.
- `capture_table_record.R` provides the record-capture command.
- `compare_table_records.R` provides the record-comparison command.
- `run_clean_validation.sh` runs and compares one clean candidate.
- `README.md` documents the contract and commands.

Each R file remains below 200 lines, and each line remains below 100
characters.

The public internal interfaces are:

```r
paper_table_record(output_root)
paper_validate_table_record(record)
paper_compare_table_records(reference, candidate)
```

Schema version 3 contains only a schema version and the projected published
tables. Each numeric token stores its value, displayed quantum, and
significance-star string. Schema-2 records lack stars and are rejected; callers
must recapture them from their retained TeX tables.

## Clean-run workflow

`run_clean_validation.sh` receives an explicit reference record. It:

- validates the reference before staging or computation;
- creates a temporary run root;
- copies repository source without `.git` or `scripts-paper/output`;
- creates no candidate output before the pipeline starts;
- forces one fresh bootstrap run;
- runs `Rscript scripts-paper/run_pipeline.R`;
- captures and validates the candidate table record;
- compares it with the reference;
- retains the candidate source, output, record, and log for inspection.

The wrapper never clears or writes the caller's main output tree. The staged
copy gives the candidate an empty output tree without risking retained results.
It launches no cache-reuse pass and no second bootstrap.

The caller must supply the reference. The validation layer never selects a
baseline implicitly.

## Compatibility and retirement

The current files under `scripts-paper/tests/support/published_table_*` become
thin wrappers around the canonical validation modules. Existing focused tests
and the historical bootstrap workflow therefore use the same implementation.

`scripts-paper/tests/support/compare_pipeline_artifacts.R` keeps its two-output
command-line interface but delegates to the canonical table projection and
comparison. Its old exact CSV, RDS, Markdown, path-normalization, and
PDF-presence logic is removed.

The active bootstrap validation tools under
`docs/bootstrap-single-stage-refactor/validation-tools/` also become thin
wrappers around the canonical modules. Their schema-2 reference remains
historical evidence, but a future bootstrap comparison must capture a
schema-3 record from the retained reference tables.

Internal scientific-comparison helpers may remain for unit and estimator
tests. They are not pipeline acceptance gates.

## Failure behavior

The commands fail with a nonzero status for:

- an unreadable or malformed reference or candidate record;
- an absent or empty final table directory;
- a missing or extra table path;
- a table with no numeric result;
- an added, removed, moved, or replaced numeric cell;
- a numeric token-count mismatch;
- a displayed numeric difference;
- a significance-star difference;
- a staging failure or nonzero pipeline exit.

On comparison failure, the report names the table, cell coordinate, token, and
whether the value or stars differ. No later artifact or internal-state check
can change the decision.

## Test strategy

Implementation follows red-green-refactor.

Synthetic fixtures cover:

- decimal, negative, interval, and scientific-notation values;
- displayed-precision overlap and adjacent-token rejection;
- no-star, one-star, two-star, and three-star equality;
- star differences with equal numbers;
- ignored labels, prose, statuses, and markers that remain nonnumeric in both
  records;
- ignored non-table artifacts and arbitrary non-table differences;
- missing and extra table paths;
- added, removed, moved, and number-to-status cells;
- numeric token-count mismatches;
- malformed record fields and token columns;
- tables with no numeric results.

A compatibility test first proves that the current general artifact comparator
rejects non-table differences. The implementation then changes that test to
pass while preserving failures for numeric and star differences.

A clean-run fixture proves that staging excludes prior output and that a failed
pipeline cannot accept stale tables. It uses a small fake producer and never
launches the production bootstrap.

Final validation includes:

- the focused validation suites;
- `Rscript scripts-paper/tests/run_tests.R`;
- current-output capture and self-comparison;
- a candidate with ignored non-table and prose changes;
- candidates with deliberate numeric and star changes;
- package tests, lint, package check, and pre-commit.

No 10,000-draw bootstrap is required to validate this refactor.

## Non-goals

This change does not alter estimators, reported precision, scientific results,
pipeline ordering, table rendering, dependencies, or bootstrap computation.
It adds no acceptance criterion outside final displayed table numbers and
their significance stars.
