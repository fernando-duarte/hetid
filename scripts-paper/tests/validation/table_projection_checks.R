# Focused checks for canonical published-table numeric projections.

source(file.path("scripts-paper", "config", "paths.R"))

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

negative <- paper_table_cell_results("-0.50")
stopifnot(
  identical(negative$value, -0.5),
  identical(negative$quantum, 0.01),
  identical(negative$stars, "")
)

interval <- paper_table_cell_results("[-1.2, 3.45]")
stopifnot(
  identical(interval$value, c(-1.2, 3.45)),
  identical(interval$quantum, c(0.1, 0.01)),
  identical(interval$stars, c("", ""))
)

exponent <- paper_table_cell_results("6.0e-3")
stopifnot(
  identical(exponent$value, 0.006),
  identical(exponent$quantum, 1e-4),
  identical(exponent$stars, "")
)

empty <- paper_table_cell_results("not estimated")
stopifnot(
  identical(names(empty), c("value", "quantum", "stars")),
  identical(empty$value, double()),
  identical(empty$quantum, double()),
  identical(empty$stars, character())
)

write_table <- function(root, relative, lines) {
  path <- file.path(root, "tables", relative)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
  path
}

output_root <- tempfile("paper-table-projection-")
dir.create(output_root)
table_path <- write_table(output_root, "nested/results.tex", c(
  "\\begin{tabular}{lrr}",
  "Measure & Estimate & Interval \\\\",
  "\\midrule",
  "Result & 1.23$^{**}$ & [-0.50, 2.0] \\\\",
  "Scientific & $2.31 \\times 10^{-9}$ & 6.0e-3 \\\\",
  "\\end{tabular}",
  "\\emph{Notes:} 99 observations."
))

projection <- paper_table_numeric_projection(table_path)
stopifnot(
  identical(names(projection), c(
    "tabular_1/row_1/column_1",
    "tabular_1/row_1/column_2",
    "tabular_1/row_2/column_1",
    "tabular_1/row_2/column_2"
  )),
  identical(projection[["tabular_1/row_1/column_1"]]$stars, "**"),
  identical(
    projection[["tabular_1/row_1/column_2"]]$value,
    c(-0.5, 2)
  ),
  identical(
    projection[["tabular_1/row_2/column_1"]]$value,
    2.31e-9
  )
)

tables <- paper_published_tables_projection(output_root)
stopifnot(
  identical(names(tables), "nested/results.tex"),
  identical(tables[["nested/results.tex"]], projection)
)

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
