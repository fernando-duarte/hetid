# Cross-run table acceptance

Updated: 2026-10-01 05:02 EDT

This comparator checks numeric results printed in final TeX tables and the
significance stars attached to those results. A pass establishes agreement
within that comparison scope; it does not certify a complete or fresh pipeline
run, figure agreement, or scientific correctness.

Run this command from the repository root:

```sh
Rscript --vanilla scripts-paper/validation/compare_output_tables.R \
  path/to/reference/scripts-paper/output \
  path/to/candidate/scripts-paper/output
```

Both arguments are existing output roots, each containing a `tables/`
directory. Relative arguments are resolved from the repository root. The
command reads `.tex` files recursively below each root's `tables/` directory.
It does not run the pipeline, capture a reference, or write validation
artifacts.

## Comparison rule

Each numeric result is paired by relative TeX path below `tables/`, tabular
block, data row, result column, and token position within the cell. A candidate
fails when numeric table paths or cell coordinates differ, a cell has a
different numeric token count, displayed rounding intervals have no interior
overlap, or attached stars differ. Equal numeric values pass even when their
printed precision differs. Adjacent rounded values such as `1.23` and `1.24`
fail; intervals that only touch at a boundary do not count as overlap.

Displayed precision is inferred from each printed token. The parser supports
signed decimals, leading decimals, scientific notation, and TeX
`\times 10^{...}` notation. Significance stars compare exactly, whether
written bare (`0.796***`) or as a superscript (`0.796$^{***}$`).

## Coverage and limits

The parser reads the pipeline's `tabular` layout: one row per source line,
with `&` separating cells and the first column serving as the row label. It
recognizes both `\midrule` and `\cmidrule`. After the first such rule, the
first row with a nonempty label starts the data region; later rows with empty
labels, such as standard-error rows, are included. This covers both ordinary
booktabs tables and the pipeline's tables with `\kern` spacing. It is not a general
TeX parser.

Labels, headers, captions, notes, prose, and paired nonnumeric statuses are
ignored, as are numbers in the row-label column. TeX files without numeric
result cells and all non-table artifacts are ignored. Row and column positions
still matter: inserting a nonnumeric data row can shift later numeric
coordinates and fail the comparison.

Two empty numeric projections pass when both output roots and their `tables/`
directories are readable. Before using a pass for acceptance, verify that the
expected numeric tables were actually parsed. Check artifact completeness,
input identity, freshness, labels, notes, nonnumeric statuses, and figures
separately.

## Inputs and exit status

The command exits zero and prints
`Published table-result comparison passed.` when the projections match. It
exits one and prints targeted numeric or star differences when they do not.
Wrong arguments, missing or unreadable roots, missing or unreadable `tables/`
directories, and unreadable TeX files also exit nonzero with an input error.

Run the pipeline separately when fresh output is required, following the
prerequisites and publishing safeguards in the [pipeline README](../README.md):

```sh
Rscript scripts-paper/run_pipeline.R
```

## Files and checks

| File | Role |
| --- | --- |
| `compare_output_tables.R` | Two-root command-line interface and exit status. |
| `table_projection.R` | Table discovery and numeric cell coordinates. |
| `table_tokens.R` | Number, displayed precision, and star parsing. |
| `table_comparison.R` | Coordinate, rounding-interval, and star comparisons. |

Run the focused checks from the repository root:

```sh
Rscript --vanilla scripts-paper/tests/validation/test_table_acceptance.R
```

These checks cover parsing, comparisons, intended mutation acceptance or
rejection, command-line behavior, and duplicate acceptance definitions. They
use temporary fixtures and scan source files; they do not run the paper
pipeline.
