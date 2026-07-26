# Cross-run table acceptance

Updated: 2026-07-26 19:37 EDT

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
