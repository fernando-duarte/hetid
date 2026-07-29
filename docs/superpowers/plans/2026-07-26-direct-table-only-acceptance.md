# Direct Table-Only Acceptance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use subagent-driven-development
> (recommended) or executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

Recorded: 2026-07-26 13:38 EDT

**Goal:** Replace the serialized and staged acceptance system with one command
that directly compares displayed numbers and significance stars in final TeX
tables under two existing `scripts-paper/output` roots.

**Architecture:** Keep `table_tokens.R` as the numeric parser. Refactor
`table_projection.R` and `table_comparison.R` into a pure in-memory path from
two output roots to numeric projections and comparison results, then expose
that path through `compare_output_tables.R`. Remove every record, schema,
capture, clean-run, marker, compatibility, and intermediate scientific
acceptance component while leaving pipeline-owned safeguards unchanged.

**Tech Stack:** Base R, TeX table fragments, the existing `paper_path()` and
`paper_source_once()` source conventions, shell/static checks, Git.

**Approved design:**
`docs/superpowers/specs/2026-07-26-direct-table-only-acceptance-design.md`

## Global Constraints

- The sole public acceptance command is
  `Rscript --vanilla scripts-paper/validation/compare_output_tables.R
  <reference-output-root> <candidate-output-root>`.
- Each output root must already exist and contain a readable `tables/`
  directory. The comparator never runs the pipeline.
- Acceptance reads `.tex` files recursively under each `tables/` directory and
  writes no records, caches, snapshots, logs, manifests, or markers.
- Only projected numeric tokens and attached significance stars decide
  acceptance.
- Displayed values pass when their printed-precision intervals overlap,
  including the existing floating-point slack.
- Stars `""`, `*`, `**`, and `***` compare exactly.
- Signed decimals, leading decimals, scientific notation, and TeX
  `\times 10^{...}` notation remain supported.
- Missing, added, or moved numeric tokens; different token counts; displayed
  values outside the overlap rule; and different stars fail.
- Labels, headers, captions, notes, prose, paired nonnumeric statuses,
  nonnumeric-only TeX files, and every non-table artifact are ignored.
- Two empty numeric projections pass when both roots and `tables/` directories
  are readable.
- `scripts-paper/run_pipeline.R`, estimator reliability checks, cache
  validation, scientific assertions, and producer-owned serialization remain
  unchanged.
- Historical reports and retained evidence remain in place. Their
  current-status text must point to the direct comparator.
- Keep changed R files below 200 lines and every changed line below 100
  characters.
- Use `apply_patch` for edits and deletions. Preserve unrelated user changes.

---

## File Structure

### Retained acceptance implementation

- `scripts-paper/validation/table_tokens.R`: sole parser for displayed numeric
  tokens, precision quanta, and stars; no functional change.
- `scripts-paper/validation/table_projection.R`: validates input directories,
  reads TeX, and returns only in-memory numeric cells and numeric table paths.
- `scripts-paper/validation/table_comparison.R`: compares two projections and
  exposes the root-to-root comparison function.
- `scripts-paper/validation/compare_output_tables.R`: sole acceptance CLI.
- `scripts-paper/validation/README.md`: direct-command usage and exact
  acceptance semantics.

### Retained acceptance tests

- `scripts-paper/tests/validation/table_projection_checks.R`: token and
  projection behavior, including empty projections.
- `scripts-paper/tests/validation/table_comparison_checks.R`: pure projection
  comparison behavior.
- `scripts-paper/tests/validation/mutation_matrix_checks.R`: proof that only
  numeric results, their placement, token counts, and stars affect acceptance.
- `scripts-paper/tests/validation/cli_checks.R`: subprocess status, output, and
  input-error behavior for the one CLI.
- `scripts-paper/tests/validation/ssot_checks.R`: parser, quantum, and rounding
  definitions remain owned by `scripts-paper/validation/`.
- `scripts-paper/tests/validation/test_table_acceptance.R`: focused acceptance
  test entrypoint.

### Removed acceptance implementation

- `scripts-paper/validation/table_record.R`
- `scripts-paper/validation/capture_table_record.R`
- `scripts-paper/validation/compare_table_records.R`
- `scripts-paper/validation/run_clean_validation.sh`
- `docs/bootstrap-single-stage-refactor/validation-tools/`
- acceptance-only wrappers and comparators under
  `scripts-paper/tests/support/`
- record, clean-run, compatibility, and fixture-pipeline tests under
  `scripts-paper/tests/validation/`

### Updated current documentation

- `scripts-paper/README.md`
- `scripts-paper/validation/README.md`
- `docs/bootstrap-single-stage-refactor/baseline-artifacts/README.md`
- `docs/bootstrap-single-stage-refactor/validation.md`
- `docs/bootstrap-single-stage-refactor/final-report.md`

---

### Task 1: Make TeX projection numeric-only and empty-safe

**Files:**

- Modify: `scripts-paper/validation/table_projection.R:5-74`
- Modify:
  `scripts-paper/tests/validation/table_projection_checks.R:53-95`

**Interfaces:**

- Consumes:
  `paper_table_cell_results(cell) -> data.frame(value, quantum, stars)` from
  `table_tokens.R`.
- Produces:
  `paper_table_numeric_projection(path) -> named list<data.frame>`, containing
  only cells with at least one numeric token.
- Produces:
  `paper_published_tables_projection(output_root) -> named
  list<named list<data.frame>>`, containing only TeX files with numeric cells.

- [ ] **Step 1: Add failing projection and input-path checks**

Keep the existing token-parser assertions. After the current numeric projection
assertions, add:

```r
nonnumeric_path <- write_table(output_root, "nonnumeric.tex", c(
  "\\begin{tabular}{lr}",
  "Measure & Estimate \\\\",
  "\\midrule",
  "Status & not estimated \\\\",
  "\\end{tabular}"
))
nonnumeric_projection <- paper_table_numeric_projection(nonnumeric_path)
stopifnot(identical(nonnumeric_projection, list()))

tables <- paper_published_tables_projection(output_root)
stopifnot(
  identical(names(tables), "nested/results.tex"),
  identical(tables[["nested/results.tex"]], projection)
)

empty_root <- tempfile("paper-empty-table-root-")
dir.create(file.path(empty_root, "tables"), recursive = TRUE)
empty_tables <- paper_published_tables_projection(empty_root)
stopifnot(length(empty_tables) == 0L)

missing_root_error <- tryCatch(
  paper_published_tables_projection(file.path(empty_root, "missing")),
  error = function(error) conditionMessage(error)
)
missing_tables_root <- tempfile("paper-no-tables-")
dir.create(missing_tables_root)
missing_tables_error <- tryCatch(
  paper_published_tables_projection(missing_tables_root),
  error = function(error) conditionMessage(error)
)
stopifnot(
  grepl("output root does not exist", missing_root_error, fixed = TRUE),
  grepl("tables directory does not exist", missing_tables_error, fixed = TRUE)
)

unlink(output_root, recursive = TRUE)
unlink(empty_root, recursive = TRUE)
unlink(missing_tables_root, recursive = TRUE)
rm(
  write_table,
  output_root,
  table_path,
  projection,
  nonnumeric_path,
  nonnumeric_projection,
  tables,
  empty_root,
  empty_tables,
  missing_root_error,
  missing_tables_root,
  missing_tables_error
)
```

- [ ] **Step 2: Run the focused test and verify the old minimum gate fails**

Run:

```sh
Rscript --vanilla scripts-paper/tests/validation/table_projection_checks.R
```

Expected: nonzero exit with
`published table has no numeric result cells`, proving that the old minimum
numeric-result gate is still active.

- [ ] **Step 3: Replace projection construction with numeric-only filtering**

Replace the two functions in `table_projection.R` with:

```r
paper_require_readable_directory <- function(path, label) {
  if (!dir.exists(path)) {
    stop(label, " does not exist: ", path, call. = FALSE)
  }
  if (file.access(path, mode = 5L) != 0L) {
    stop(label, " is not readable: ", path, call. = FALSE)
  }
  invisible(path)
}

paper_table_numeric_projection <- function(path) {
  if (!file.exists(path) || dir.exists(path)) {
    stop("published table does not exist: ", path, call. = FALSE)
  }
  if (file.access(path, mode = 4L) != 0L) {
    stop("published table is not readable: ", path, call. = FALSE)
  }
  lines <- readLines(path, warn = FALSE)
  projection <- list()
  in_tabular <- FALSE
  data_started <- FALSE
  tabular_id <- 0L
  row_id <- 0L
  for (line in lines) {
    if (grepl("\\begin{tabular}", line, fixed = TRUE)) {
      in_tabular <- TRUE
      data_started <- FALSE
      tabular_id <- tabular_id + 1L
      row_id <- 0L
      next
    }
    if (grepl("\\end{tabular}", line, fixed = TRUE)) {
      in_tabular <- FALSE
      next
    }
    if (!in_tabular) {
      next
    }
    if (grepl("\\midrule", line, fixed = TRUE)) {
      data_started <- TRUE
      next
    }
    if (!data_started || !grepl("&", line, fixed = TRUE)) {
      next
    }
    row_id <- row_id + 1L
    cells <- strsplit(line, "&", fixed = TRUE)[[1L]]
    if (length(cells) < 2L) {
      next
    }
    for (column_id in seq_along(cells[-1L])) {
      key <- sprintf(
        "tabular_%d/row_%d/column_%d",
        tabular_id,
        row_id,
        column_id
      )
      projection[[key]] <- paper_table_cell_results(cells[[column_id + 1L]])
    }
  }
  has_values <- vapply(projection, nrow, integer(1)) > 0L
  projection[has_values]
}

paper_published_tables_projection <- function(output_root) {
  paper_require_readable_directory(output_root, "output root")
  table_root <- file.path(output_root, "tables")
  paper_require_readable_directory(table_root, "tables directory")
  paths <- list.files(
    table_root,
    pattern = "[.]tex$",
    recursive = TRUE,
    full.names = TRUE
  )
  relative <- substring(paths, nchar(table_root) + 2L)
  ordering <- order(relative)
  projections <- stats::setNames(
    lapply(paths[ordering], paper_table_numeric_projection),
    relative[ordering]
  )
  has_values <- vapply(projections, length, integer(1)) > 0L
  projections[has_values]
}
```

Do not add a minimum-table or minimum-token assertion.

- [ ] **Step 4: Run projection checks**

Run:

```sh
Rscript --vanilla scripts-paper/tests/validation/table_projection_checks.R
```

Expected: exit 0 with no output.

- [ ] **Step 5: Commit the projection change**

```sh
git add scripts-paper/validation/table_projection.R \
  scripts-paper/tests/validation/table_projection_checks.R
git commit -m "Make table projections numeric only"
```

Expected: commit succeeds with hooks enabled.

---

### Task 2: Compare in-memory projections directly

**Files:**

- Modify: `scripts-paper/validation/table_comparison.R:1-130`
- Modify:
  `scripts-paper/tests/validation/table_comparison_checks.R:1-130`

**Interfaces:**

- Consumes:
  `paper_published_tables_projection(output_root)`.
- Produces:
  `paper_compare_table_projections(reference, candidate) -> TRUE | character`.
- Produces:
  `paper_compare_output_tables(reference_output_root,
  candidate_output_root) -> TRUE | character`.

- [ ] **Step 1: Replace record-based tests with direct-projection tests**

Replace `table_comparison_checks.R` with:

```r
# Focused checks for direct published-table comparisons.

paper_source_once(paper_path("validation", "table_comparison.R"))

comparison_cell <- function(value, quantum = 0.01, stars = "") {
  data.frame(
    value = value,
    quantum = rep(quantum, length(value)),
    stars = rep(stars, length(value)),
    stringsAsFactors = FALSE
  )
}

coordinate <- "tabular_1/row_1/column_1"
second_coordinate <- "tabular_1/row_1/column_2"
reference <- list("table.tex" = list(
  "tabular_1/row_1/column_1" = comparison_cell(1.234),
  "tabular_1/row_1/column_2" = comparison_cell(4.56)
))
candidate <- list("table.tex" = list(
  "tabular_1/row_1/column_1" = comparison_cell(1.23),
  "tabular_1/row_1/column_2" = comparison_cell(4.560)
))
stopifnot(isTRUE(paper_compare_table_projections(reference, candidate)))

candidate$table.tex[[coordinate]]$stars <- "**"
problems <- paper_compare_table_projections(reference, candidate)
stopifnot(any(grepl("stars differ", problems, fixed = TRUE)))
candidate$table.tex[[coordinate]]$stars <- ""

reference$table.tex[[coordinate]]$value <- 1.23
candidate$table.tex[[coordinate]]$value <- 1.24
problems <- paper_compare_table_projections(reference, candidate)
stopifnot(any(grepl("displayed values differ", problems, fixed = TRUE)))
reference$table.tex[[coordinate]]$value <- 1.234
candidate$table.tex[[coordinate]]$value <- 1.23

candidate$table.tex[[coordinate]] <- comparison_cell(c(1.23, 2.34))
problems <- paper_compare_table_projections(reference, candidate)
stopifnot(any(grepl("token counts differ", problems, fixed = TRUE)))
candidate$table.tex[[coordinate]] <- comparison_cell(1.23)

candidate$table.tex[[coordinate]] <- NULL
problems <- paper_compare_table_projections(reference, candidate)
stopifnot(any(grepl("numeric coordinates differ", problems, fixed = TRUE)))
candidate$table.tex[[coordinate]] <- comparison_cell(1.23)

moved_candidate <- list("table.tex" = list(
  "tabular_1/row_1/column_2" = comparison_cell(1.23),
  "tabular_1/row_1/column_3" = comparison_cell(4.560)
))
problems <- paper_compare_table_projections(reference, moved_candidate)
stopifnot(any(grepl("numeric coordinates differ", problems, fixed = TRUE)))

missing_table_reference <- reference
missing_table_reference$extra.tex <- list(
  "tabular_1/row_1/column_1" = comparison_cell(1.23)
)
problems <- paper_compare_table_projections(
  missing_table_reference,
  candidate
)
stopifnot(any(grepl("missing candidate numeric tables", problems, fixed = TRUE)))

extra_table <- candidate
extra_table$extra.tex <- list(
  "tabular_1/row_1/column_1" = comparison_cell(1.23)
)
problems <- paper_compare_table_projections(reference, extra_table)
stopifnot(any(grepl("extra candidate numeric tables", problems, fixed = TRUE)))

empty_projection <- stats::setNames(list(), character())
stopifnot(isTRUE(paper_compare_table_projections(
  empty_projection,
  empty_projection
)))

scientific_reference <- list("table.tex" = list(
  "tabular_1/row_1/column_1" = comparison_cell(2.31e-9, 1e-11)
))
scientific_candidate <- list("table.tex" = list(
  "tabular_1/row_1/column_1" = comparison_cell(2.32e-9, 1e-11)
))
stopifnot(!isTRUE(paper_compare_table_projections(
  scientific_reference,
  scientific_candidate
)))

rm(
  comparison_cell,
  coordinate,
  second_coordinate,
  reference,
  candidate,
  problems,
  moved_candidate,
  missing_table_reference,
  extra_table,
  empty_projection,
  scientific_reference,
  scientific_candidate
)
```

- [ ] **Step 2: Run the comparison test and verify the record API mismatch**

Run:

```sh
Rscript --vanilla scripts-paper/tests/validation/table_comparison_checks.R
```

Expected: nonzero exit because the existing comparator interprets direct
projections as schema-versioned records.

- [ ] **Step 3: Replace record comparison with projection comparison**

Replace `table_comparison.R` with:

```r
# Direct comparisons of published-table numeric projections.

paper_source_once(paper_path("validation", "table_projection.R"))

paper_table_tokens_equal <- function(reference, candidate) {
  stopifnot(nrow(reference) == nrow(candidate))
  difference <- abs(reference$value - candidate$value)
  rounding_overlap <- (reference$quantum + candidate$quantum) / 2
  scale <- pmax(
    abs(reference$value),
    abs(candidate$value),
    rounding_overlap
  )
  slack <- 8 * .Machine$double.eps * scale
  difference == 0 |
    difference < rounding_overlap - slack
}

paper_projection_names <- function(projection) {
  projection_names <- names(projection)
  if (is.null(projection_names)) character() else sort(projection_names)
}

paper_coordinate_difference <- function(path, reference, candidate) {
  missing <- setdiff(reference, candidate)
  extra <- setdiff(candidate, reference)
  if (!length(missing) && !length(extra)) {
    return(character())
  }
  details <- c(
    if (length(missing)) {
      paste("missing candidate:", paste(missing, collapse = ", "))
    },
    if (length(extra)) {
      paste("extra candidate:", paste(extra, collapse = ", "))
    }
  )
  paste0(
    "numeric coordinates differ: ",
    path,
    " (",
    paste(details, collapse = "; "),
    ")"
  )
}

paper_table_path_difference <- function(reference, candidate) {
  missing <- setdiff(reference, candidate)
  extra <- setdiff(candidate, reference)
  c(
    if (length(missing)) {
      paste(
        "missing candidate numeric tables:",
        paste(missing, collapse = ", ")
      )
    },
    if (length(extra)) {
      paste(
        "extra candidate numeric tables:",
        paste(extra, collapse = ", ")
      )
    }
  )
}

paper_compare_table_projections <- function(reference, candidate) {
  reference_paths <- paper_projection_names(reference)
  candidate_paths <- paper_projection_names(candidate)
  problems <- paper_table_path_difference(reference_paths, candidate_paths)
  for (path in intersect(reference_paths, candidate_paths)) {
    reference_table <- reference[[path]]
    candidate_table <- candidate[[path]]
    reference_coordinates <- paper_projection_names(reference_table)
    candidate_coordinates <- paper_projection_names(candidate_table)
    problems <- c(
      problems,
      paper_coordinate_difference(
        path,
        reference_coordinates,
        candidate_coordinates
      )
    )
    for (coordinate in intersect(
      reference_coordinates,
      candidate_coordinates
    )) {
      reference_cell <- reference_table[[coordinate]]
      candidate_cell <- candidate_table[[coordinate]]
      if (nrow(reference_cell) != nrow(candidate_cell)) {
        problems <- c(
          problems,
          sprintf(
            "token counts differ: %s/%s (reference: %d, candidate: %d)",
            path,
            coordinate,
            nrow(reference_cell),
            nrow(candidate_cell)
          )
        )
        next
      }
      unequal_values <- which(!paper_table_tokens_equal(
        reference_cell,
        candidate_cell
      ))
      if (length(unequal_values)) {
        problems <- c(
          problems,
          sprintf(
            "displayed values differ: %s/%s/token_%d",
            path,
            coordinate,
            unequal_values
          )
        )
      }
      unequal_stars <- which(reference_cell$stars != candidate_cell$stars)
      if (length(unequal_stars)) {
        problems <- c(
          problems,
          sprintf(
            "stars differ: %s/%s/token_%d",
            path,
            coordinate,
            unequal_stars
          )
        )
      }
    }
  }
  if (length(problems)) problems else TRUE
}

paper_compare_output_tables <- function(
  reference_output_root,
  candidate_output_root
) {
  paper_compare_table_projections(
    paper_published_tables_projection(reference_output_root),
    paper_published_tables_projection(candidate_output_root)
  )
}
```

The file must not source `table_record.R` or construct a schema wrapper.

- [ ] **Step 4: Run comparison and projection checks**

Run:

```sh
Rscript --vanilla scripts-paper/tests/validation/table_projection_checks.R
Rscript --vanilla scripts-paper/tests/validation/table_comparison_checks.R
```

Expected: both commands exit 0.

- [ ] **Step 5: Commit the direct comparator**

```sh
git add scripts-paper/validation/table_comparison.R \
  scripts-paper/tests/validation/table_comparison_checks.R
git commit -m "Compare table projections directly"
```

Expected: commit succeeds with hooks enabled.

---

### Task 3: Add the sole CLI and the acceptance mutation matrix

**Files:**

- Create: `scripts-paper/validation/compare_output_tables.R`
- Create:
  `scripts-paper/tests/validation/mutation_matrix_checks.R`
- Modify: `scripts-paper/tests/validation/cli_checks.R:1-135`
- Modify:
  `scripts-paper/tests/validation/test_table_acceptance.R:1-31`

**Interfaces:**

- Consumes:
  `paper_compare_output_tables(reference_output_root,
  candidate_output_root) -> TRUE | character`.
- Produces: one CLI with exit 0 on equality and exit 1 on comparison
  differences or unusable inputs.
- Produces: a mutation matrix that separates accepted nonnumeric changes from
  rejected numeric and star changes.

- [ ] **Step 1: Add failing subprocess tests for the new CLI**

Rewrite `cli_checks.R` so it:

```r
# Subprocess checks for direct output-table comparison.

cli_write_table <- function(root, body, relative = "table.tex") {
  table_path <- file.path(root, "tables", relative)
  dir.create(dirname(table_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "\\begin{tabular}{lr}",
    "\\toprule",
    " & Estimate \\\\",
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}"
  ), table_path)
}

cli_table_body <- function(value, stars) {
  paste0("Estimate & ", value, "$^{", stars, "}$ \\\\")
}

cli_write_non_table_artifacts <- function(root, suffix) {
  writeLines(suffix, file.path(root, "other.csv"))
  writeLines(suffix, file.path(root, "other.rds"))
  writeLines(suffix, file.path(root, "other.md"))
  writeLines(suffix, file.path(root, "other.svg"))
  writeLines(suffix, file.path(root, "other.pdf"))
  dir.create(file.path(root, "diagnostics"), showWarnings = FALSE)
  writeLines(suffix, file.path(root, "diagnostics", "details.txt"))
}

cli_output <- function(arguments) {
  system2(
    file.path(R.home("bin"), "Rscript"),
    args = c(
      "--vanilla",
      paper_path("validation", "compare_output_tables.R"),
      arguments
    ),
    stdout = TRUE,
    stderr = TRUE
  )
}

cli_status <- function(output) {
  status <- attr(output, "status")
  if (is.null(status)) 0L else as.integer(status)
}

cli_reference_root <- tempfile("table-cli-reference-")
cli_candidate_root <- tempfile("table-cli-candidate-")
dir.create(cli_reference_root)
dir.create(cli_candidate_root)
cli_write_table(
  cli_reference_root,
  cli_table_body("1.23", "***")
)
cli_write_table(
  cli_candidate_root,
  cli_table_body("1.23", "***")
)
cli_write_table(
  cli_candidate_root,
  "Status & not estimated \\\\",
  "nonnumeric-only.tex"
)
cli_write_non_table_artifacts(cli_reference_root, "reference")
cli_write_non_table_artifacts(cli_candidate_root, "candidate")

output <- cli_output(c(cli_reference_root, cli_candidate_root))
stopifnot(
  identical(cli_status(output), 0L),
  any(grepl(
    "Published table-result comparison passed.",
    output,
    fixed = TRUE
  ))
)

cli_write_table(cli_candidate_root, cli_table_body("1.24", "***"))
output <- cli_output(c(cli_reference_root, cli_candidate_root))
stopifnot(
  identical(cli_status(output), 1L),
  any(grepl("displayed values differ", output, fixed = TRUE))
)

cli_write_table(cli_candidate_root, cli_table_body("1.23", "**"))
output <- cli_output(c(cli_reference_root, cli_candidate_root))
stopifnot(
  identical(cli_status(output), 1L),
  any(grepl("stars differ", output, fixed = TRUE))
)

empty_reference <- tempfile("table-cli-empty-reference-")
empty_candidate <- tempfile("table-cli-empty-candidate-")
dir.create(file.path(empty_reference, "tables"), recursive = TRUE)
dir.create(file.path(empty_candidate, "tables"), recursive = TRUE)
output <- cli_output(c(empty_reference, empty_candidate))
stopifnot(identical(cli_status(output), 0L))

output <- cli_output(cli_reference_root)
stopifnot(
  identical(cli_status(output), 1L),
  any(grepl("Usage: compare_output_tables.R", output, fixed = TRUE))
)

unlink(cli_reference_root, recursive = TRUE)
unlink(cli_candidate_root, recursive = TRUE)
unlink(empty_reference, recursive = TRUE)
unlink(empty_candidate, recursive = TRUE)
rm(
  cli_write_table,
  cli_table_body,
  cli_write_non_table_artifacts,
  cli_output,
  cli_status,
  cli_reference_root,
  cli_candidate_root,
  empty_reference,
  empty_candidate,
  output
)
```

The `.rds` fixture is plain ignored text. The acceptance tests must not call
`saveRDS()` or `readRDS()`.

- [ ] **Step 2: Run the CLI test and verify the command is absent**

Run:

```sh
Rscript --vanilla scripts-paper/tests/validation/cli_checks.R
```

Expected: nonzero exit because
`scripts-paper/validation/compare_output_tables.R` does not exist.

- [ ] **Step 3: Create the direct comparison CLI**

Create `compare_output_tables.R` with:

```r
#!/usr/bin/env Rscript
# Compare final published table numbers across two output roots.

script_argument <- commandArgs(FALSE)
script_argument <- script_argument[grepl("^--file=", script_argument)]
if (length(script_argument) != 1L) {
  stop("could not determine compare_output_tables.R location", call. = FALSE)
}
script_path <- normalizePath(
  sub("^--file=", "", script_argument),
  mustWork = TRUE
)
setwd(normalizePath(
  file.path(dirname(script_path), "..", ".."),
  mustWork = TRUE
))

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("validation", "table_comparison.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    paste(
      "Usage: compare_output_tables.R",
      "<reference-output-root> <candidate-output-root>"
    ),
    call. = FALSE
  )
}
comparison <- paper_compare_output_tables(args[[1L]], args[[2L]])
if (!isTRUE(comparison)) {
  cat("Table-result comparison failed:\n")
  cat(paste0("- ", comparison), sep = "\n")
  quit(status = 1L)
}
cat("Published table-result comparison passed.\n")
```

- [ ] **Step 4: Add the mutation matrix**

Create `mutation_matrix_checks.R` with:

```r
# Mutation proof for direct final-table acceptance.

paper_source_once(paper_path("validation", "table_comparison.R"))

mutation_write_table <- function(root, lines, relative = "table.tex") {
  path <- file.path(root, "tables", relative)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
}

mutation_replace <- function(lines, old, new) {
  sub(old, new, lines, fixed = TRUE)
}

baseline_lines <- c(
  "\\caption{Baseline caption}",
  "\\begin{tabular}{lrr}",
  "Measure & Estimate & Interval \\\\",
  "\\midrule",
  "Main & 1.23$^{***}$ & [-0.50, 2.0] \\\\",
  "Status & not estimated & unavailable \\\\",
  "\\end{tabular}",
  "\\emph{Notes:} Baseline note."
)

mutation_cases <- data.frame(
  id = c(
    "caption_text",
    "header_text",
    "note_text",
    "paired_status_text",
    "extra_nonnumeric_table",
    "non_table_artifact",
    "displayed_value",
    "significance_stars",
    "missing_numeric_token",
    "added_numeric_token",
    "moved_numeric_token",
    "numeric_token_count"
  ),
  accepted = c(
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE
  ),
  stringsAsFactors = FALSE
)

reference_root <- tempfile("table-mutation-reference-")
candidate_root <- tempfile("table-mutation-candidate-")
dir.create(reference_root)
mutation_write_table(reference_root, baseline_lines)

mutation_failures <- character()
mutation_seen <- character()
check_mutation <- function(
  id,
  lines = baseline_lines,
  setup = function(root) invisible(root),
  expected_message = NULL
) {
  unlink(candidate_root, recursive = TRUE)
  dir.create(candidate_root)
  mutation_write_table(candidate_root, lines)
  setup(candidate_root)
  comparison <- paper_compare_output_tables(
    reference_root,
    candidate_root
  )
  expected <- mutation_cases$accepted[mutation_cases$id == id]
  passed <- isTRUE(comparison)
  message_ok <- is.null(expected_message) ||
    any(grepl(expected_message, comparison, fixed = TRUE))
  if (length(expected) != 1L || passed != expected || !message_ok) {
    mutation_failures <<- c(
      mutation_failures,
      paste(id, paste(comparison, collapse = " | "), sep = ": ")
    )
  }
  mutation_seen <<- c(mutation_seen, id)
  invisible(comparison)
}

check_mutation(
  "caption_text",
  mutation_replace(
    baseline_lines,
    "Baseline caption",
    "Changed caption"
  )
)
check_mutation(
  "header_text",
  mutation_replace(baseline_lines, "Measure", "Statistic")
)
check_mutation(
  "note_text",
  mutation_replace(baseline_lines, "Baseline note.", "Changed note.")
)
check_mutation(
  "paired_status_text",
  mutation_replace(baseline_lines, "not estimated", "not available")
)
check_mutation(
  "extra_nonnumeric_table",
  setup = function(root) {
    mutation_write_table(
      root,
      c(
        "\\begin{tabular}{lr}",
        "Measure & Status \\\\",
        "\\midrule",
        "Model & not estimated \\\\",
        "\\end{tabular}"
      ),
      "nonnumeric-only.tex"
    )
  }
)
check_mutation(
  "non_table_artifact",
  setup = function(root) {
    writeLines("ignored", file.path(root, "different.rds"))
  }
)
check_mutation(
  "displayed_value",
  mutation_replace(baseline_lines, "1.23", "1.24"),
  expected_message = "displayed values differ"
)
check_mutation(
  "significance_stars",
  mutation_replace(baseline_lines, "***", "**"),
  expected_message = "stars differ"
)
check_mutation(
  "missing_numeric_token",
  mutation_replace(baseline_lines, "1.23$^{***}$", "not reported"),
  expected_message = "numeric coordinates differ"
)
check_mutation(
  "added_numeric_token",
  mutation_replace(baseline_lines, "not estimated", "7.89"),
  expected_message = "numeric coordinates differ"
)
check_mutation(
  "moved_numeric_token",
  mutation_replace(
    baseline_lines,
    "Main & 1.23$^{***}$ & [-0.50, 2.0]",
    "Main & not reported & [1.23$^{***}$, -0.50, 2.0]"
  ),
  expected_message = "numeric coordinates differ"
)
check_mutation(
  "numeric_token_count",
  mutation_replace(
    baseline_lines,
    "1.23$^{***}$",
    "[1.23$^{***}$, 1.24]"
  ),
  expected_message = "token counts differ"
)

if (!identical(sort(mutation_seen), sort(mutation_cases$id))) {
  mutation_failures <- c(
    mutation_failures,
    "mutation case coverage differs from the declared matrix"
  )
}
if (length(mutation_failures)) {
  stop(
    "table acceptance mutation failures: ",
    paste(mutation_failures, collapse = "; "),
    call. = FALSE
  )
}

unlink(reference_root, recursive = TRUE)
unlink(candidate_root, recursive = TRUE)
rm(
  mutation_write_table,
  mutation_replace,
  baseline_lines,
  mutation_cases,
  reference_root,
  candidate_root,
  mutation_failures,
  mutation_seen,
  check_mutation
)
```

- [ ] **Step 5: Wire the focused entrypoint to active checks**

Replace `test_table_acceptance.R` with:

```r
#!/usr/bin/env Rscript

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path(
  "tests", "validation", "table_projection_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "table_comparison_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "mutation_matrix_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "ssot_checks.R"
))
paper_source_once(paper_path(
  "tests", "validation", "cli_checks.R"
))
cat("test_table_acceptance: PASS\n")
```

- [ ] **Step 6: Run the new command and focused tests**

Run:

```sh
Rscript --vanilla scripts-paper/tests/validation/cli_checks.R
Rscript --vanilla scripts-paper/tests/validation/mutation_matrix_checks.R
Rscript --vanilla scripts-paper/tests/validation/test_table_acceptance.R
Rscript --vanilla scripts-paper/validation/compare_output_tables.R \
  scripts-paper/output scripts-paper/output
```

Expected:

- the first two commands exit 0;
- the focused entrypoint prints `test_table_acceptance: PASS`;
- the self-comparison prints
  `Published table-result comparison passed.`

- [ ] **Step 7: Commit the CLI and mutation coverage**

```sh
git add scripts-paper/validation/compare_output_tables.R \
  scripts-paper/tests/validation/cli_checks.R \
  scripts-paper/tests/validation/mutation_matrix_checks.R \
  scripts-paper/tests/validation/test_table_acceptance.R
git commit -m "Add direct table comparison command"
```

Expected: commit succeeds with hooks enabled.

---

### Task 4: Remove serialized, staged, and compatibility acceptance layers

**Files:**

- Delete:
  `scripts-paper/validation/table_record.R`
- Delete:
  `scripts-paper/validation/capture_table_record.R`
- Delete:
  `scripts-paper/validation/compare_table_records.R`
- Delete:
  `scripts-paper/validation/run_clean_validation.sh`
- Delete:
  `scripts-paper/tests/validation/table_record_checks.R`
- Delete:
  `scripts-paper/tests/validation/compatibility_wrapper_checks.R`
- Delete:
  `scripts-paper/tests/validation/clean_runner_checks.R`
- Delete:
  `scripts-paper/tests/validation/clean_runner_reference_checks.R`
- Delete:
  `scripts-paper/tests/validation/clean_runner_safety_checks.R`
- Delete:
  `scripts-paper/tests/validation/fixture_pipeline.R`
- Delete:
  `scripts-paper/tests/support/compare_pipeline_artifacts.R`
- Delete:
  `scripts-paper/tests/support/published_table_comparison.R`
- Delete:
  `scripts-paper/tests/support/published_table_comparison_checks.R`
- Delete:
  `scripts-paper/tests/support/published_table_tokens.R`
- Delete:
  `scripts-paper/tests/support/scientific_comparison.R`
- Delete:
  `scripts-paper/tests/support/scientific_comparison_checks.R`
- Delete every tracked file under
  `docs/bootstrap-single-stage-refactor/validation-tools/`
- Modify:
  `scripts-paper/tests/validation/fixtures/renamed_acceptance_duplicate.R`
- Modify:
  `scripts-paper/tests/validation/ssot_checks.R:38-67,97-104`
- Modify: `scripts-paper/tests/run_tests.R:18-19,50-53`
- Modify: `inst/WORDLIST:247`

**Interfaces:**

- Consumes: the active tests and CLI from Tasks 1-3.
- Produces: no acceptance record API, no acceptance serialization, no
  acceptance runner, no acceptance compatibility wrapper, and no separate
  intermediate scientific comparison suite.

- [ ] **Step 1: Add a static test proving forbidden mechanisms are absent**

At the end of `ssot_checks.R`, before cleanup, add:

```r
acceptance_files <- list.files(
  paper_path("validation"),
  recursive = TRUE,
  full.names = TRUE
)
acceptance_files <- acceptance_files[grepl("[.](R|sh)$", acceptance_files)]
acceptance_code <- paste(
  unlist(lapply(acceptance_files, readLines, warn = FALSE)),
  collapse = "\n"
)
forbidden_acceptance_terms <- c(
  "saveRDS",
  "readRDS",
  "schema_version",
  "table_record",
  "capture_table",
  "run_clean_validation",
  "HETID_VALIDATION",
  "comparison-passed"
)
stopifnot(!any(vapply(
  forbidden_acceptance_terms,
  grepl,
  logical(1),
  x = acceptance_code,
  fixed = TRUE
)))
```

Add `acceptance_files`, `acceptance_code`, and
`forbidden_acceptance_terms` to the final `rm()` call.

- [ ] **Step 2: Run the SSOT check and verify obsolete files are detected**

Run:

```sh
Rscript --vanilla scripts-paper/tests/validation/ssot_checks.R
```

Expected: nonzero exit because the active validation directory still contains
`readRDS`, `schema_version`, record files, and the clean runner.

- [ ] **Step 3: Delete obsolete acceptance implementation and tests**

Use one `apply_patch` deletion containing exactly the files listed in this
task. Include all eleven tracked files under
`docs/bootstrap-single-stage-refactor/validation-tools/`:

```text
README.md
capture_legacy_reference.sh
capture_pipeline_record.R
capture_table_record.R
compare_scientific_objects.R
pipeline_expression.R
run_mac_candidate.sh
scientific_record.R
test_capture_legacy_reference.sh
test_pipeline_expression.R
test_scientific_record.R
```

Do not delete historical `.rds` evidence, reports, archives, or output.

- [ ] **Step 4: Remove schema semantics from the SSOT regression**

Delete `renamed_record_constructor()` and `renamed_record_validator()` from
`renamed_acceptance_duplicate.R`. Keep only the renamed token parser, precision
quantum, and rounding comparator.

In `ssot_checks.R`, set:

```r
definition_patterns <- list(
  numeric_token = c(
    "gregexpr\\s*\\(",
    "regmatches\\s*\\(",
    "as.numeric\\s*\\("
  ),
  quantum = c(
    "sub\\s*\\(",
    "regexpr\\s*\\(",
    "nchar\\s*\\(",
    "10\\s*\\^"
  ),
  rounding_overlap = c(
    "\\$quantum",
    "abs\\s*\\(",
    "/\\s*2",
    "[.]Machine\\$double[.]eps"
  )
)
```

Set the fixture assertions to:

```r
stopifnot(
  any(grepl("numeric_token", fixture_hits, fixed = TRUE)),
  any(grepl("quantum", fixture_hits, fixed = TRUE)),
  any(grepl("rounding_overlap", fixture_hits, fixed = TRUE))
)
```

- [ ] **Step 5: Remove the intermediate scientific suite from the harness**

Delete the paired manifest entries:

```r
"scientific_comparison"
"support/scientific_comparison_checks.R"
```

Keep `table_acceptance` paired with
`validation/test_table_acceptance.R`. The resulting manifest has 34 suites.

- [ ] **Step 6: Remove the clean-run-only spelling entry**

Delete the exact line:

```text
unowned
```

from `inst/WORDLIST`. No surviving scoped source uses it.

- [ ] **Step 7: Run focused, topology, ownership, and full paper tests**

Run:

```sh
Rscript --vanilla scripts-paper/tests/validation/ssot_checks.R
Rscript --vanilla scripts-paper/tests/validation/test_table_acceptance.R
Rscript --vanilla scripts-paper/tests/support/check_topology.R
Rscript --vanilla scripts-paper/tests/support/check_contract_ownership.R
Rscript --vanilla scripts-paper/tests/run_tests.R
```

Expected:

- SSOT exits 0;
- focused acceptance prints `test_table_acceptance: PASS`;
- topology and ownership exit 0;
- the full harness ends with
  `All 34 suites and structural checks passed.`

- [ ] **Step 8: Verify only the one CLI remains**

Run:

```sh
rg -l '^#!/usr/bin/env Rscript' scripts-paper/validation
find scripts-paper/validation -maxdepth 1 -type f -name '*.sh' -print
test ! -d docs/bootstrap-single-stage-refactor/validation-tools
```

Expected:

- `rg` prints only
  `scripts-paper/validation/compare_output_tables.R`;
- `find` prints nothing;
- `test` exits 0.

- [ ] **Step 9: Commit the removals**

```sh
git add -A scripts-paper/validation \
  scripts-paper/tests/validation \
  scripts-paper/tests/support \
  scripts-paper/tests/run_tests.R \
  docs/bootstrap-single-stage-refactor/validation-tools \
  inst/WORDLIST
git commit -m "Remove intermediate acceptance layers"
```

Expected: commit succeeds with hooks enabled.

---

### Task 5: Document the direct comparator as the current workflow

**Files:**

- Modify: `scripts-paper/README.md:191-204`
- Modify: `scripts-paper/validation/README.md:1-89`
- Modify:
  `docs/bootstrap-single-stage-refactor/baseline-artifacts/README.md:3,26-40`
- Modify:
  `docs/bootstrap-single-stage-refactor/validation.md:3-25`
- Modify:
  `docs/bootstrap-single-stage-refactor/final-report.md:3,14-18,50-55,86-97,172-174`

**Interfaces:**

- Consumes: the sole command created in Task 3.
- Produces: current documentation with no active capture, schema, clean-run,
  environment-variable, or compatibility instructions.

- [ ] **Step 1: Obtain the report timestamp**

Run:

```sh
date "+%Y-%m-%d %H:%M %Z"
```

Expected: one timestamp in the repository timezone, for example
`2026-07-26 13:38 EDT`. Use the exact returned value in every changed
`Updated:` line. Do not guess it.

- [ ] **Step 2: Replace the paper README acceptance section**

Replace lines 191-204 with:

````markdown
## Cross-run acceptance

Cross-run acceptance uses only final TeX table numbers and their attached
significance stars. Compare two existing output roots directly:

```sh
Rscript --vanilla scripts-paper/validation/compare_output_tables.R \
  path/to/reference/scripts-paper/output \
  path/to/candidate/scripts-paper/output
```

The command reads `.tex` files below each root's `tables/` directory. It does
not run the pipeline or inspect intermediate artifacts. See the
[canonical validation workflow](validation/README.md) for the precise
displayed-precision rule.
````

- [ ] **Step 3: Replace the validation README**

Replace `scripts-paper/validation/README.md` with:

````markdown
# Cross-run table acceptance

Updated: 2026-07-26 13:38 EDT

Cross-run acceptance is decided only by numeric results printed in final TeX
tables and the significance stars attached to those results.

Run this command from the repository root:

```sh
Rscript --vanilla scripts-paper/validation/compare_output_tables.R \
  path/to/reference/scripts-paper/output \
  path/to/candidate/scripts-paper/output
```

Both arguments are existing output roots. The command reads `.tex` files
recursively below each root's `tables/` directory. It does not run the
pipeline, capture a reference, or write validation artifacts.

## Comparison rule

Each numeric result is paired by relative TeX path, tabular block, row,
column, and token position. A candidate fails when a numeric token is missing,
added, or moved; a cell has a different numeric token count; displayed
rounding intervals do not overlap; or attached stars differ.

Displayed precision is inferred from each printed token. The parser supports
signed decimals, leading decimals, scientific notation, and TeX
`\times 10^{...}` notation. Significance stars compare exactly.

Labels, headers, captions, notes, prose, and paired nonnumeric statuses are
ignored. TeX files without numeric result cells and all non-table artifacts
are ignored. Two empty numeric projections pass when both output roots and
their `tables/` directories are readable.

## Inputs and exit status

The command exits zero and prints
`Published table-result comparison passed.` when the projections match. It
exits one and prints targeted numeric or star differences when they do not.
Wrong arguments, missing or unreadable roots, missing or unreadable `tables/`
directories, and unreadable TeX files are ordinary input errors.

Run the pipeline separately when fresh output is required:

```sh
Rscript scripts-paper/run_pipeline.R
```
````

If Step 1 prints a later timestamp, replace the recorded timestamp above with
that exact output before saving.

- [ ] **Step 4: Update bootstrap-report current-status sections**

In `baseline-artifacts/README.md`, retain the historical schema-1 and schema-2
descriptions and replace the current-acceptance paragraphs with:

````markdown
The RDS checksum is immutable historical evidence for the local R build that
wrote it. It is not a current acceptance input. Do not overwrite this RDS.

For current acceptance, compare the retained replay's existing output root
directly with a candidate output root:

```sh
Rscript --vanilla scripts-paper/validation/compare_output_tables.R \
  path/to/retained/scripts-paper/output \
  path/to/candidate/scripts-paper/output
```
````

In `validation.md`, replace `Current cross-run acceptance` and
`Current Task 5 validation` with:

```markdown
## Current cross-run acceptance

Current acceptance directly compares numeric tokens and attached stars in the
final TeX tables under two existing output roots. It does not serialize a
reference, stage a clean run, execute the pipeline, or inspect intermediate
artifacts. The retained schema records and earlier rerun/reuse comparisons are
immutable historical evidence, not current acceptance inputs.

## Current direct-comparison validation

- Direct table acceptance suite: passed.
- Mutation matrix: accepted prose, nonnumeric-only table, and non-table changes;
  rejected displayed-value, star, numeric-coordinate, and token-count changes.
- Semantic SSOT scan: passed across active R functions outside
  `scripts-paper/validation/`; renamed parser, quantum, and rounding duplicate
  fixtures were detected.
- Topology and contract ownership: passed.
- Paper test runner: all 34 suites and structural checks passed.
```

In `final-report.md`:

- replace current status lines 14-18 with the direct-comparison paragraph above;
- remove the schema-3 validator bullet from resulting architecture;
- replace `Current Task 5 validation` with the five current-validation bullets
  above;
- replace lines 172-174 with:

```markdown
This byte checksum is scoped to the local R serializer. It is historical
evidence only. Current cross-platform acceptance reads displayed numeric tokens
and attached stars directly from the two existing TeX output trees.
```

Keep the remaining schema and long-run discussion explicitly historical.

- [ ] **Step 5: Verify active documentation exposes one workflow**

Run:

```sh
rg -n \
  'capture_table_record|compare_table_records|run_clean_validation|HETID_VALIDATION|run_mac_candidate' \
  scripts-paper/README.md scripts-paper/validation/README.md
rg -n 'compare_output_tables[.]R' \
  scripts-paper/README.md scripts-paper/validation/README.md \
  docs/bootstrap-single-stage-refactor/baseline-artifacts/README.md \
  docs/bootstrap-single-stage-refactor/validation.md \
  docs/bootstrap-single-stage-refactor/final-report.md
```

Expected:

- the first command has no matches;
- the second command finds the current direct command in both paper READMEs and
  in current-status bootstrap documentation where a command is shown.

- [ ] **Step 6: Commit documentation**

```sh
git add scripts-paper/README.md scripts-paper/validation/README.md \
  docs/bootstrap-single-stage-refactor/baseline-artifacts/README.md \
  docs/bootstrap-single-stage-refactor/validation.md \
  docs/bootstrap-single-stage-refactor/final-report.md
git commit -m "Document direct table-only acceptance"
```

Expected: commit succeeds with hooks enabled.

---

### Task 6: Prove the final boundary and repository invariants

**Files:**

- Verify only; no planned source changes.

**Interfaces:**

- Consumes all prior tasks.
- Produces final evidence that the acceptance layer is direct and table-only,
  while producer code and retained output remain unchanged.

- [ ] **Step 1: Parse active validation R code**

Run:

```sh
Rscript --vanilla -e '
files <- list.files(
  "scripts-paper/validation",
  pattern = "[.]R$",
  full.names = TRUE
)
invisible(lapply(files, parse))
cat(sprintf("parsed %d validation R files\n", length(files)))
'
```

Expected: `parsed 4 validation R files`.

- [ ] **Step 2: Check line and file limits**

Run:

```sh
Rscript --vanilla -e '
files <- c(
  list.files(
    "scripts-paper/validation",
    pattern = "[.]R$",
    full.names = TRUE
  ),
  list.files(
    "scripts-paper/tests/validation",
    pattern = "[.]R$",
    recursive = TRUE,
    full.names = TRUE
  )
)
lines <- lapply(files, readLines, warn = FALSE)
long <- unlist(Map(
  function(path, text) {
    index <- which(nchar(text, type = "width") > 99L)
    if (length(index)) paste0(path, ":", index) else character()
  },
  files,
  lines
))
oversize <- files[vapply(lines, length, integer(1)) > 199L]
stopifnot(!length(long), !length(oversize))
cat("validation line and file limits passed\n")
'
```

Expected: `validation line and file limits passed`.

- [ ] **Step 3: Run focused and full tests**

Run:

```sh
Rscript --vanilla scripts-paper/tests/validation/test_table_acceptance.R
Rscript --vanilla scripts-paper/tests/run_tests.R
```

Expected:

- `test_table_acceptance: PASS`;
- `All 34 suites and structural checks passed.`

- [ ] **Step 4: Exercise the public command against retained output**

Run:

```sh
Rscript --vanilla scripts-paper/validation/compare_output_tables.R \
  scripts-paper/output scripts-paper/output
```

Expected:
`Published table-result comparison passed.`

- [ ] **Step 5: Scan active acceptance code for removed mechanisms**

Run:

```sh
rg -n \
  'saveRDS|readRDS|schema_version|table_record|capture_table|run_clean_validation|HETID_VALIDATION|comparison-passed|run_mac_candidate|compare_scientific_objects' \
  scripts-paper/validation scripts-paper/tests/validation
```

Expected: no matches.

Then run:

```sh
rg -n \
  'capture_table_record|compare_table_records|run_clean_validation|run_mac_candidate|compare_scientific_objects|scientific_comparison' \
  scripts-paper --glob '*.R' --glob '*.sh' --glob 'README.md'
```

Expected: no active code or README references to removed commands.

- [ ] **Step 6: Prove producer code and retained output are unchanged**

Use the approved-spec commit as the boundary:

```sh
git diff --exit-code 7df91fd..HEAD -- \
  scripts-paper/run_pipeline.R \
  scripts-paper/config \
  scripts-paper/log_variance \
  scripts-paper/mean_equation \
  scripts-paper/inference \
  scripts-paper/support \
  scripts-paper/output
```

Expected: no output and exit 0.

This check intentionally excludes `scripts-paper/tests/support`, where obsolete
acceptance wrappers were removed.

- [ ] **Step 7: Run changed-file pre-commit hooks**

Run:

```sh
git diff --name-only --diff-filter=ACMR -z 7df91fd..HEAD |
  xargs -0 pre-commit run --files
```

Expected: all hooks pass. If a hook rewrites a task file, inspect the diff,
rerun the focused and full tests, stage the intended rewrite, and commit it
with hooks enabled. Do not use `--no-verify`.

- [ ] **Step 8: Review the final diff and commit any hook-owned repair**

Run:

```sh
git status --short
git diff --stat 7df91fd..HEAD
git diff --check 7df91fd..HEAD
```

Expected:

- no uncommitted changes;
- changes are confined to acceptance implementation, acceptance tests,
  acceptance documentation, and the obsolete `unowned` word-list entry;
- `git diff --check` exits 0.

If a hook required an intended repair, commit only that repair:

```sh
git diff --name-only --diff-filter=M -z -- \
  scripts-paper/validation \
  scripts-paper/tests/validation \
  scripts-paper/README.md \
  docs/bootstrap-single-stage-refactor |
  xargs -0 git add --
git commit -m "Fix direct acceptance validation"
```

The worktree must be clean before Step 7, so this stages only hook rewrites
inside the task-owned paths. Inspect `git diff --cached` before committing and
unstage any unrelated file if the clean-worktree precondition was violated.

---

## Completion Evidence

The implementation is complete only when all of the following are true:

- `scripts-paper/validation/` contains four R files:
  `table_tokens.R`, `table_projection.R`, `table_comparison.R`, and
  `compare_output_tables.R`.
- Only `compare_output_tables.R` has an Rscript shebang.
- No acceptance `.sh` file, schema record, RDS operation, capture command,
  clean runner, marker, environment gate, Mac wrapper, or intermediate
  scientific comparator remains.
- The mutation matrix passes every accepted and rejected case.
- Both empty numeric projections compare equal.
- The retained output self-comparison passes.
- The focused acceptance suite and all 34 paper suites pass.
- Producer code and `scripts-paper/output` are byte-unchanged from `7df91fd`.
- Current documentation presents only the direct output-root comparator.
