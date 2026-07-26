# Pipeline Table-Only Acceptance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> subagent-driven-development (recommended) or executing-plans to implement
> this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make displayed table numbers and significance stars the only
cross-run acceptance inputs for the complete `scripts-paper` pipeline.

**Architecture:** Canonical modules under `scripts-paper/validation/` parse,
project, validate, and compare final TeX tables. A clean runner stages an
isolated source copy without prior output, runs the pipeline once, captures a
schema-3 table record, and compares it with an explicit reference.

**Tech Stack:** Base R, TeX text parsing, POSIX shell, `rsync`, Git, and the
existing `scripts-paper` source and test harnesses.

## Global Constraints

- Execute implementation in a new Git worktree and branch created from commit
  `9152c69` or its exact descendant containing only this plan.
- Do not modify, switch, merge, reset, or clean the main checkout during
  implementation.
- Compare only displayed numeric table results and their attached significance
  stars.
- Ignore all non-table outputs and all nonnumeric table content.
- Require matching relative TeX table paths and at least one comparable numeric
  result per table.
- Start every candidate with no prior `scripts-paper/output` content.
- Require successful pipeline process completion before comparison.
- Run exactly one fresh bootstrap in the production clean runner.
- Never select a reference implicitly; require an explicit reference record.
- Use schema version `3L`; reject schema-2 records because they omit stars.
- Preserve current numeric parsing and displayed-precision boundary behavior.
- Add no estimator, dependency, reported result, or output-format change.
- Add no package or paper-pipeline dependency.
- Keep every R file below 200 lines and every line below 100 columns.
- Follow red-green-refactor and commit after each green task.
- Do not run a 10,000-draw bootstrap while implementing or validating this
  refactor.

---

## File map

Create:

- `scripts-paper/validation/table_tokens.R`: numeric, precision, and star parser.
- `scripts-paper/validation/table_projection.R`: TeX table projection.
- `scripts-paper/validation/table_record.R`: schema-3 construction and validation.
- `scripts-paper/validation/table_comparison.R`: canonical comparison and messages.
- `scripts-paper/validation/capture_table_record.R`: atomic record-capture CLI.
- `scripts-paper/validation/compare_table_records.R`: record-comparison CLI.
- `scripts-paper/validation/run_clean_validation.sh`: isolated clean-run CLI.
- `scripts-paper/validation/README.md`: acceptance contract and commands.
- `scripts-paper/tests/validation/test_table_acceptance.R`: validation suite driver.
- `scripts-paper/tests/validation/table_projection_checks.R`: parsing and projection tests.
- `scripts-paper/tests/validation/table_record_checks.R`: schema tests.
- `scripts-paper/tests/validation/table_comparison_checks.R`: comparison tests.
- `scripts-paper/tests/validation/cli_checks.R`: capture and compatibility CLI tests.
- `scripts-paper/tests/validation/fixture_pipeline.R`: small clean-run producer.
- `scripts-paper/tests/validation/clean_runner_checks.R`: clean staging tests.

Modify:

- `scripts-paper/tests/run_tests.R`: register the validation suite.
- `scripts-paper/tests/support/published_table_tokens.R`: compatibility loader.
- `scripts-paper/tests/support/published_table_comparison.R`: compatibility loader.
- `scripts-paper/tests/support/published_table_comparison_checks.R`: add star coverage.
- `scripts-paper/tests/support/compare_pipeline_artifacts.R`: table-only compatibility CLI.
- `scripts-paper/README.md`: document whole-pipeline table-only acceptance.
- `docs/bootstrap-single-stage-refactor/validation-tools/scientific_record.R`:
  delegate to schema 3.
- `docs/bootstrap-single-stage-refactor/validation-tools/compare_scientific_objects.R`:
  delegate to the canonical comparator.
- `docs/bootstrap-single-stage-refactor/validation-tools/capture_table_record.R`:
  delegate to the canonical capture command.
- `docs/bootstrap-single-stage-refactor/validation-tools/README.md`: mark schema 2
  historical and document schema-3 recapture.
- `docs/bootstrap-single-stage-refactor/validation-tools/test_scientific_record.R`:
  use schema-3 fixtures.
- `docs/bootstrap-single-stage-refactor/validation-tools/test_capture_legacy_reference.sh`:
  expect schema 3 and stars.

Do not modify archived records under
`docs/bootstrap-single-stage-refactor/archives/` or the historical schema-2 RDS.

---

### Task 1: Canonical token parsing and table projection

**Files:**

- Create: `scripts-paper/validation/table_tokens.R`
- Create: `scripts-paper/validation/table_projection.R`
- Create: `scripts-paper/tests/validation/test_table_acceptance.R`
- Create: `scripts-paper/tests/validation/table_projection_checks.R`
- Modify: `scripts-paper/tests/run_tests.R`

**Interfaces:**

- Consumes: a TeX cell string or an output root containing `tables/**/*.tex`.
- Produces:
  `paper_table_cell_results(cell) -> data.frame(value, quantum, stars)`,
  `paper_table_numeric_projection(path) -> named list of token frames`, and
  `paper_published_tables_projection(output_root) -> named list of tables`.

- [ ] **Step 1: Register a validation suite that sources focused checks**

Add `table_acceptance` and `validation/test_table_acceptance.R` to the same
positions in `suite_manifest$id` and `suite_manifest$path`.

Create the suite driver:

```r
#!/usr/bin/env Rscript

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path(
  "tests", "validation", "table_projection_checks.R"
))
cat("test_table_acceptance: PASS\n")
```

- [ ] **Step 2: Write the failing token and projection checks**

Create fixtures through a local `write_table()` helper. Require the current
decimal, negative, interval, `e`, and `\times 10^{}` behavior plus stars:

```r
paper_source_once(paper_path(
  "validation", "table_projection.R"
))

starred <- paper_table_cell_results("1.23$^{***}$")
stopifnot(
  identical(names(starred), c("value", "quantum", "stars")),
  identical(starred$value, 1.23),
  identical(starred$quantum, 0.01),
  identical(starred$stars, "***")
)

scientific <- paper_table_cell_results(
  "$2.31 \\times 10^{-9}$"
)
stopifnot(
  identical(scientific$value, 2.31e-9),
  identical(scientific$quantum, 1e-11),
  identical(scientific$stars, "")
)
```

Build a temporary `tables/nested/results.tex` with a tabular, `\midrule`,
numeric rows, statuses, and notes. Assert that projection includes numeric
cells after `\midrule`, ignores note numbers outside the tabular, and uses the
relative path `nested/results.tex`.

- [ ] **Step 3: Run the suite and verify red**

Run:

```sh
Rscript scripts-paper/tests/validation/test_table_acceptance.R
```

Expected: failure because
`scripts-paper/validation/table_projection.R` does not exist.

- [ ] **Step 4: Implement token parsing**

In `table_tokens.R`, move the existing number and scientific-notation patterns
from test support. Add immediate trailing-star parsing:

```r
PAPER_TABLE_STAR_PATTERN <- paste0(
  "^[[:space:]]*[$]?[[:space:]]*",
  "\\^\\{([*]{1,3})\\}",
  "[[:space:]]*[$]?"
)

paper_table_token_stars <- function(cell, starts, lengths) {
  vapply(seq_along(starts), function(index) {
    tail <- substring(cell, starts[[index]] + lengths[[index]])
    match <- regexec(PAPER_TABLE_STAR_PATTERN, tail, perl = TRUE)
    pieces <- regmatches(tail, match)[[1L]]
    if (length(pieces) == 2L) pieces[[2L]] else ""
  }, character(1))
}
```

Implement `paper_table_cell_results()` by reusing
`paper_table_normalize_token()` and `paper_table_number_quantum()`. Return a
zero-row data frame with typed `double`, `double`, and `character` columns when
the cell has no numeric token.

- [ ] **Step 5: Implement table projection**

Move the current tabular scan into `table_projection.R`. Source
`table_tokens.R` with `paper_source_once()`. Preserve these rules exactly:

```r
if (grepl("\\begin{tabular}", line, fixed = TRUE)) {
  in_tabular <- TRUE
  data_started <- FALSE
}
if (grepl("\\midrule", line, fixed = TRUE)) {
  data_started <- TRUE
}
```

Store result cells by
`tabular_%d/row_%d/column_%d`. Require at least one numeric token per file.
Sort relative table paths before constructing the named result.

- [ ] **Step 6: Run focused and topology checks**

Run:

```sh
Rscript scripts-paper/tests/validation/test_table_acceptance.R
Rscript scripts-paper/tests/support/check_topology.R
```

Expected: both pass.

- [ ] **Step 7: Commit**

```sh
git add scripts-paper/validation/table_tokens.R \
  scripts-paper/validation/table_projection.R \
  scripts-paper/tests/validation/test_table_acceptance.R \
  scripts-paper/tests/validation/table_projection_checks.R \
  scripts-paper/tests/run_tests.R
git commit -m "Add canonical published-table projection"
```

### Task 2: Schema-3 records and table-only comparison

**Files:**

- Create: `scripts-paper/validation/table_record.R`
- Create: `scripts-paper/validation/table_comparison.R`
- Create: `scripts-paper/tests/validation/table_record_checks.R`
- Create: `scripts-paper/tests/validation/table_comparison_checks.R`
- Modify: `scripts-paper/tests/validation/test_table_acceptance.R`

**Interfaces:**

- Consumes: a projected output root or two schema-3 records.
- Produces:
  `paper_table_record(output_root)`,
  `paper_validate_table_record(record)`,
  `paper_table_tokens_equal(reference, candidate)`, and
  `paper_compare_table_records(reference, candidate)`.
- `paper_compare_table_records()` returns `TRUE` or a character vector of
  precise problems.

- [ ] **Step 1: Write failing record-validation checks**

Use this valid fixture:

```r
valid_cell <- data.frame(
  value = 1.23,
  quantum = 0.01,
  stars = "***",
  stringsAsFactors = FALSE
)
valid_record <- list(
  schema_version = 3L,
  published_tables = list(
    "table.tex" = list(
      "tabular_1/row_1/column_1" = valid_cell
    )
  )
)
stopifnot(isTRUE(paper_validate_table_record(valid_record)))
```

Require errors for schema 2, empty tables, traversal and drive paths, duplicate
paths, malformed coordinates, non-data-frame cells, wrong columns, matrices,
nonfinite values, nonpositive quanta, and stars outside
`c("", "*", "**", "***")`.

- [ ] **Step 2: Write failing comparison checks**

Build reference and candidate records that prove:

```r
stopifnot(isTRUE(paper_compare_table_records(reference, candidate)))

candidate$published_tables$table.tex[[coordinate]]$stars <- "**"
problems <- paper_compare_table_records(reference, candidate)
stopifnot(
  !isTRUE(problems),
  any(grepl("stars differ", problems, fixed = TRUE))
)
```

Also require:

- `1.234` versus `1.23` passes;
- `1.23` versus `1.24` fails;
- `2.31e-9` versus `2.32e-9` fails;
- changed labels and statuses pass;
- different numeric token counts in one cell are ignored;
- missing or extra table paths fail;
- a table with zero comparable tokens fails.

- [ ] **Step 3: Run the suite and verify red**

Run:

```sh
Rscript scripts-paper/tests/validation/test_table_acceptance.R
```

Expected: failure because the schema and comparison functions do not exist.

- [ ] **Step 4: Implement schema-3 construction and validation**

In `table_record.R`, source `table_projection.R` and implement:

```r
paper_table_record <- function(output_root) {
  record <- list(
    schema_version = 3L,
    published_tables = paper_published_tables_projection(output_root)
  )
  paper_validate_table_record(record)
  record
}
```

Validate exact top-level names, schema `3L`, safe relative `.tex` paths,
coordinate syntax, exact cell columns, atomic column vectors, finite values,
positive quanta, allowed stars, and at least one numeric token per table.
Prefix all failures with `invalid published-table record:`.

- [ ] **Step 5: Implement displayed-value and star comparison**

In `table_comparison.R`, source `table_record.R`. Preserve the current strict
rounding-overlap calculation:

```r
difference <- abs(reference$value - candidate$value)
rounding_overlap <- (reference$quantum + candidate$quantum) / 2
scale <- pmax(
  abs(reference$value),
  abs(candidate$value),
  rounding_overlap
)
slack <- 8 * .Machine$double.eps * scale
value_equal <- difference == 0 |
  difference < rounding_overlap - slack
```

Compare stars only for token frames with equal nonzero row counts. Report value
and star differences separately. Intersect coordinates and require at least one
compared token per table.

- [ ] **Step 6: Run focused and topology checks**

Run:

```sh
Rscript scripts-paper/tests/validation/test_table_acceptance.R
Rscript scripts-paper/tests/support/check_topology.R
```

Expected: both pass.

- [ ] **Step 7: Commit**

```sh
git add scripts-paper/validation/table_record.R \
  scripts-paper/validation/table_comparison.R \
  scripts-paper/tests/validation/test_table_acceptance.R \
  scripts-paper/tests/validation/table_record_checks.R \
  scripts-paper/tests/validation/table_comparison_checks.R
git commit -m "Compare published table results and stars"
```

### Task 3: Capture, compare, and compatibility commands

**Files:**

- Create: `scripts-paper/validation/capture_table_record.R`
- Create: `scripts-paper/validation/compare_table_records.R`
- Create: `scripts-paper/tests/validation/cli_checks.R`
- Modify: `scripts-paper/tests/validation/test_table_acceptance.R`
- Modify: `scripts-paper/tests/support/published_table_tokens.R`
- Modify: `scripts-paper/tests/support/published_table_comparison.R`
- Modify: `scripts-paper/tests/support/published_table_comparison_checks.R`
- Modify: `scripts-paper/tests/support/compare_pipeline_artifacts.R`

**Interfaces:**

- Capture CLI:
  `Rscript capture_table_record.R output_root record.rds`.
- Record comparison CLI:
  `Rscript compare_table_records.R reference.rds candidate.rds`.
- Output-root compatibility CLI:
  `Rscript compare_pipeline_artifacts.R reference_root candidate_root`.

- [ ] **Step 1: Write failing CLI tests**

Create temporary reference and candidate output roots containing the same table.
Add arbitrary, different CSV, RDS, Markdown, SVG, PDF, and diagnostic files.
Assert:

```r
status <- system2(
  rscript,
  paper_path("tests", "support", "compare_pipeline_artifacts.R"),
  c(reference_root, candidate_root)
)
stopifnot(identical(status, 0L))
```

The current comparator must fail this assertion. Add further subprocess checks:

- equal numbers and stars pass despite arbitrary non-table differences;
- a displayed numeric difference exits nonzero;
- a star difference exits nonzero;
- capture writes a schema-3 record with stars;
- comparing the record with itself exits zero.

- [ ] **Step 2: Run the suite and verify red**

Run:

```sh
Rscript scripts-paper/tests/validation/test_table_acceptance.R
```

Expected: failure because the current compatibility comparator examines
non-table artifacts and the canonical CLIs do not exist.

- [ ] **Step 3: Implement atomic capture**

The capture CLI must derive the repository root from its own `--file` path,
change to that root, source `config/paths.R`, and source
`validation/table_comparison.R`. It must:

```r
record <- paper_table_record(output_root)
temporary <- tempfile(
  pattern = paste0(basename(record_path), "."),
  tmpdir = dirname(record_path)
)
saveRDS(record, temporary, version = 3L)
roundtrip <- readRDS(temporary)
paper_validate_table_record(roundtrip)
stopifnot(identical(record, roundtrip))
if (!file.rename(temporary, record_path)) {
  stop("could not promote validated table record", call. = FALSE)
}
```

Keep the temporary file on the destination filesystem so promotion is atomic.

- [ ] **Step 4: Implement record comparison**

Read both RDS files, validate them, call `paper_compare_table_records()`, print
all problems, and exit 1 unless the result is `TRUE`. A passing command prints:

```text
published table-result comparison passed
```

- [ ] **Step 5: Replace the general artifact comparator**

Delete its manifest, path normalization, timestamp normalization, and
CSV/RDS/Markdown/PDF logic. Preserve its two-root argument contract:

```r
reference <- paper_table_record(reference_root)
candidate <- paper_table_record(candidate_root)
comparison <- paper_compare_table_records(reference, candidate)
if (!isTRUE(comparison)) {
  cat("Table-result comparison failed:\n")
  cat(paste0("- ", comparison), sep = "\n")
  quit(status = 1L)
}
cat("Published table-result comparison passed.\n")
```

- [ ] **Step 6: Convert test-support owners into thin loaders**

`published_table_tokens.R` sources
`validation/table_tokens.R`. `published_table_comparison.R` sources
`validation/table_comparison.R` and defines only compatibility aliases where
an existing caller needs an old name:

```r
paper_table_cell_numbers <- paper_table_cell_results
paper_published_tables_compare <- function(reference, candidate) {
  paper_compare_table_records(
    list(schema_version = 3L, published_tables = reference),
    list(schema_version = 3L, published_tables = candidate)
  )
}
```

Update existing checks to expect the `stars` column and to reject an equal
number whose stars changed.

- [ ] **Step 7: Run focused and existing compatibility checks**

Run:

```sh
Rscript scripts-paper/tests/validation/test_table_acceptance.R
Rscript scripts-paper/tests/support/scientific_comparison_checks.R
Rscript scripts-paper/tests/support/check_topology.R
```

Expected: all pass.

- [ ] **Step 8: Commit**

```sh
git add scripts-paper/validation/capture_table_record.R \
  scripts-paper/validation/compare_table_records.R \
  scripts-paper/tests/validation/cli_checks.R \
  scripts-paper/tests/validation/test_table_acceptance.R \
  scripts-paper/tests/support/published_table_tokens.R \
  scripts-paper/tests/support/published_table_comparison.R \
  scripts-paper/tests/support/published_table_comparison_checks.R \
  scripts-paper/tests/support/compare_pipeline_artifacts.R
git commit -m "Make pipeline artifact comparison table-only"
```

### Task 4: Isolated clean-run acceptance

**Files:**

- Create: `scripts-paper/validation/run_clean_validation.sh`
- Create: `scripts-paper/tests/validation/fixture_pipeline.R`
- Create: `scripts-paper/tests/validation/clean_runner_checks.R`
- Modify: `scripts-paper/tests/validation/test_table_acceptance.R`

**Interfaces:**

- Command:
  `bash scripts-paper/validation/run_clean_validation.sh reference.rds`.
- Optional run location:
  `HETID_VALIDATION_RUN_ROOT=/absolute/path`.
- Test-only producer seam:
  `HETID_VALIDATION_PIPELINE_SCRIPT=scripts-paper/tests/validation/fixture_pipeline.R`.

- [ ] **Step 1: Write the fake producer**

The fixture must fail if stale output survives staging:

```r
output_root <- file.path("scripts-paper", "output")
if (file.exists(file.path(output_root, "stale-sentinel"))) {
  stop("stale output reached fixture pipeline", call. = FALSE)
}
if (identical(Sys.getenv("HETID_FIXTURE_PIPELINE_FAIL"), "1")) {
  stop("requested fixture failure", call. = FALSE)
}
dir.create(file.path(output_root, "tables"), recursive = TRUE)
writeLines(
  c(
    "\\begin{tabular}{lc}",
    "\\midrule",
    "Estimate & 1.23$^{**}$ \\\\",
    "\\end{tabular}"
  ),
  file.path(output_root, "tables", "fixture.tex")
)
```

- [ ] **Step 2: Write failing clean-run checks**

Create a schema-3 reference from the same fixture table. Preseed
`$HETID_VALIDATION_RUN_ROOT/source/scripts-paper/output/stale-sentinel`, run
the wrapper with the fixture seam, and require:

- exit zero;
- stale output moved outside the staged output tree;
- candidate record exists and compares equal;
- the log records the fixture producer;
- the original checkout output is unchanged.

Run again with `HETID_FIXTURE_PIPELINE_FAIL=1`; require a nonzero exit and no
`comparison-passed` marker.

- [ ] **Step 3: Run the suite and verify red**

Run:

```sh
Rscript scripts-paper/tests/validation/test_table_acceptance.R
```

Expected: failure because `run_clean_validation.sh` does not exist.

- [ ] **Step 4: Implement safe staging**

Use `set -euo pipefail`. Derive `repo_root` from the script. Validate the
reference before staging by comparing it with itself. Create `run_root` through
`mktemp -d` unless the environment supplies one.

Stage with:

```sh
rsync -a --delete \
  --exclude .git \
  --exclude scripts-paper/output/ \
  "$repo_root/" "$source_root/"
```

Resolve the exact staged output path. If it exists, move it to
`$run_root/preexisting-output`; never recursively delete it. Create an empty
output directory afterward.

- [ ] **Step 5: Run once, capture, and compare**

Default the producer to `scripts-paper/run_pipeline.R`, require a relative path
inside the staged source, and set:

```sh
export HETID_BOOT_REPS=10000
export HETID_BOOT_MODE=rerun
unset HETID_VALIDATION_STRICT_REUSE
```

Do not override `HETID_BOOT_CORES`; the production Mac default remains
authoritative. Run `Rscript --vanilla` once through `tee` with `pipefail`, then
invoke the canonical capture and compare CLIs. Write
`comparison-passed` only after a successful comparison. Print and retain the
run root on both success and failure.

- [ ] **Step 6: Run focused checks**

Run:

```sh
Rscript scripts-paper/tests/validation/test_table_acceptance.R
shellcheck scripts-paper/validation/run_clean_validation.sh
```

If `shellcheck` is unavailable, record that fact and rely on
`bash -n scripts-paper/validation/run_clean_validation.sh`. Expected: available
checks pass.

- [ ] **Step 7: Commit**

```sh
git add scripts-paper/validation/run_clean_validation.sh \
  scripts-paper/tests/validation/fixture_pipeline.R \
  scripts-paper/tests/validation/clean_runner_checks.R \
  scripts-paper/tests/validation/test_table_acceptance.R
git commit -m "Add clean whole-pipeline acceptance runner"
```

### Task 5: Canonical documentation and historical-tool migration

**Files:**

- Create: `scripts-paper/validation/README.md`
- Modify: `scripts-paper/README.md`
- Modify: `docs/bootstrap-single-stage-refactor/validation-tools/scientific_record.R`
- Modify: `docs/bootstrap-single-stage-refactor/validation-tools/compare_scientific_objects.R`
- Modify: `docs/bootstrap-single-stage-refactor/validation-tools/capture_table_record.R`
- Modify: `docs/bootstrap-single-stage-refactor/validation-tools/README.md`
- Modify: `docs/bootstrap-single-stage-refactor/validation-tools/test_scientific_record.R`
- Modify: `docs/bootstrap-single-stage-refactor/validation-tools/test_capture_legacy_reference.sh`

**Interfaces:**

- All active acceptance tools delegate to
  `scripts-paper/validation/table_comparison.R`.
- Historical schema-2 RDS files remain immutable evidence and are not accepted.

- [ ] **Step 1: Write a failing SSOT check**

Add to `table_comparison_checks.R` a source scan that permits numeric token,
quantum, schema, and rounding-overlap definitions only under
`scripts-paper/validation/`. Reject active duplicates under test support or the
bootstrap validation tools. The check must inspect only active files, excluding
`docs/bootstrap-single-stage-refactor/archives/`.

- [ ] **Step 2: Run the suite and verify red**

Run:

```sh
Rscript scripts-paper/tests/validation/test_table_acceptance.R
```

Expected: failure because the active bootstrap tools still own schema-2
validation and table-record construction.

- [ ] **Step 3: Convert bootstrap tools into wrappers**

Make `scientific_record.R` load the canonical module from the repository root:

```r
source(file.path(
  repo_root,
  "scripts-paper",
  "config",
  "paths.R"
))
paper_source_once(paper_path(
  "validation", "table_comparison.R"
))
bootstrap_validation_record <- paper_table_record
```

Make comparison and capture call the canonical functions or CLIs. Change tests
to schema `3L` and `data.frame(value, quantum, stars)`. Update messages from
“schema-2” to “schema-3.” Do not replace or rewrite the historical schema-2
baseline RDS.

- [ ] **Step 4: Document the canonical workflow**

The validation README must state:

- only table numbers and attached stars decide acceptance;
- table path equality and one comparable result per table remain required;
- all non-table output and nonnumeric content are ignored;
- the candidate uses an empty staged output tree;
- the pipeline runs once;
- the reference is explicit;
- schema 2 is obsolete because it lacks stars;
- exact capture, compare, and clean-run commands.

Add a concise “Cross-run acceptance” section to `scripts-paper/README.md` with
links to the canonical README and commands.

- [ ] **Step 5: Run active historical-tool tests and topology**

Run:

```sh
Rscript docs/bootstrap-single-stage-refactor/validation-tools/test_scientific_record.R
bash docs/bootstrap-single-stage-refactor/validation-tools/test_capture_legacy_reference.sh
Rscript scripts-paper/tests/validation/test_table_acceptance.R
Rscript scripts-paper/tests/support/check_topology.R
```

Expected: all pass.

- [ ] **Step 6: Commit**

```sh
git add scripts-paper/validation/README.md \
  scripts-paper/README.md \
  docs/bootstrap-single-stage-refactor/validation-tools/scientific_record.R \
  docs/bootstrap-single-stage-refactor/validation-tools/compare_scientific_objects.R \
  docs/bootstrap-single-stage-refactor/validation-tools/capture_table_record.R \
  docs/bootstrap-single-stage-refactor/validation-tools/README.md \
  docs/bootstrap-single-stage-refactor/validation-tools/test_scientific_record.R \
  docs/bootstrap-single-stage-refactor/validation-tools/test_capture_legacy_reference.sh \
  scripts-paper/tests/validation/table_comparison_checks.R
git commit -m "Centralize whole-pipeline acceptance documentation"
```

### Task 6: End-to-end validation without a production bootstrap

**Files:**

- Validate: all files committed by Tasks 1 through 5.
- Do not regenerate `scripts-paper/output`.

**Interfaces:**

- Validates the completed worktree and its current retained output.

- [ ] **Step 1: Run the complete paper test harness**

Run:

```sh
Rscript scripts-paper/tests/run_tests.R
```

Expected: every suite and structural check passes.

- [ ] **Step 2: Capture and self-compare current tables**

Use a temporary record:

```sh
record=$(mktemp "${TMPDIR:-/tmp}/hetid-table-record.XXXXXX.rds")
Rscript scripts-paper/validation/capture_table_record.R \
  scripts-paper/output "$record"
Rscript scripts-paper/validation/compare_table_records.R \
  "$record" "$record"
```

Expected: schema-3 capture and comparison pass. Record the number of table paths
and numeric tokens in the validation log.

- [ ] **Step 3: Prove ignored differences and enforced differences**

Copy `scripts-paper/output` into two temporary roots. In the candidate:

- alter or add CSV, RDS, Markdown, SVG, PDF, diagnostic, state, and report files;
- alter a table label, note, status, and missing marker without changing a
  comparable number or star.

Run the compatibility comparator and require exit zero.

Then change one displayed number and require exit nonzero. Restore it, change
one attached star, and require exit nonzero.

- [ ] **Step 4: Run package and repository gates**

Run:

```sh
Rscript -e 'devtools::test()'
Rscript -e 'lintr::lint_package()'
Rscript -e 'devtools::check()'
pre-commit run --all-files
git diff --check
```

Expected: all commands pass. Fix causes rather than weakening checks.

- [ ] **Step 5: Audit scope and repository state**

Run:

```sh
git status --short --branch
git diff --stat 9152c69..HEAD
git diff --name-only 9152c69..HEAD
git log --oneline 9152c69..HEAD
```

Confirm:

- no tracked generated output changed;
- no estimator or scientific code changed;
- every active acceptance implementation routes through
  `scripts-paper/validation/`;
- the worktree is clean.

- [ ] **Step 6: Commit any validation-only fixes**

When a validation command exposes a defect, return to the last green commit,
add a focused failing check to the owning Task 1 through 5 test file, fix its
canonical owner, rerun the focused and failed commands, and stage only those
explicit paths. Commit the correction with:

```sh
git commit -m "Harden table-only pipeline acceptance"
```

Skip this commit when no tracked change remains.

---

## Completion criteria

- One canonical table acceptance implementation exists under
  `scripts-paper/validation/`.
- Every cross-run acceptance CLI compares only table numbers and stars.
- Exact non-table artifact comparison is retired.
- Schema 3 records and validates stars.
- The clean runner starts without prior output and runs the pipeline once.
- Missing or extra tables, numeric differences, and star differences fail.
- Nonnumeric table content and every non-table output are ignored.
- No production 10,000-draw bootstrap ran during implementation.
- All focused, paper, package, lint, check, and pre-commit gates pass.
- The implementation branch remains unmerged for user review.
