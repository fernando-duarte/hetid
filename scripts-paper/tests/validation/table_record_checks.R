# Focused checks for schema-3 published-table records.

paper_source_once(paper_path("validation", "table_record.R"))

record_check_error <- function(record) {
  tryCatch(
    paper_validate_table_record(record),
    error = function(error) conditionMessage(error)
  )
}

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

projected_record <- paper_table_record(output_root)
stopifnot(
  identical(projected_record$schema_version, 3L),
  identical(projected_record$published_tables, tables)
)

invalid_schema <- valid_record
invalid_schema$schema_version <- 2L
stopifnot(grepl("invalid published-table record:", record_check_error(invalid_schema)))

empty_tables <- valid_record
empty_tables$published_tables <- list()
stopifnot(grepl("invalid published-table record:", record_check_error(empty_tables)))

forbidden_paths <- c("../table.tex", "/table.tex", "C:/table.tex")
for (path in forbidden_paths) {
  invalid_path <- valid_record
  names(invalid_path$published_tables) <- path
  stopifnot(grepl("invalid published-table record:", record_check_error(invalid_path)))
}

duplicate_paths <- valid_record
duplicate_paths$published_tables <- rep(
  valid_record$published_tables,
  2L
)
names(duplicate_paths$published_tables) <- c("table.tex", "table.tex")
stopifnot(grepl("invalid published-table record:", record_check_error(duplicate_paths)))

malformed_coordinate <- valid_record
names(malformed_coordinate$published_tables$table.tex) <- "row_1/column_1"
stopifnot(grepl("invalid published-table record:", record_check_error(malformed_coordinate)))

non_data_frame_cell <- valid_record
non_data_frame_cell$published_tables$table.tex[[1L]] <- 1.23
stopifnot(grepl("invalid published-table record:", record_check_error(non_data_frame_cell)))

wrong_columns <- valid_record
wrong_columns$published_tables$table.tex[[1L]] <- data.frame(
  value = 1.23,
  stars = "",
  stringsAsFactors = FALSE
)
stopifnot(grepl("invalid published-table record:", record_check_error(wrong_columns)))

matrix_cell <- valid_record
matrix_cell$published_tables$table.tex[[1L]] <- matrix(1.23, nrow = 1L)
stopifnot(grepl("invalid published-table record:", record_check_error(matrix_cell)))

matrix_column_cell <- valid_record
matrix_column_cell$published_tables$table.tex[[1L]] <- structure(
  list(
    value = matrix(c(1.23, 1.24), nrow = 1L),
    quantum = matrix(c(0.01, 0.01), nrow = 1L),
    stars = matrix(c("", "*"), nrow = 1L)
  ),
  class = "data.frame",
  row.names = 1L
)
stopifnot(grepl(
  "invalid published-table record:",
  record_check_error(matrix_column_cell)
))

non_atomic_column <- valid_record
non_atomic_column$published_tables$table.tex[[1L]] <- data.frame(
  value = I(list(1.23)),
  quantum = 0.01,
  stars = "",
  stringsAsFactors = FALSE
)
stopifnot(grepl("invalid published-table record:", record_check_error(non_atomic_column)))

nonfinite_value <- valid_record
nonfinite_value$published_tables$table.tex[[1L]]$value <- Inf
stopifnot(grepl("invalid published-table record:", record_check_error(nonfinite_value)))

nonfinite_quantum <- valid_record
nonfinite_quantum$published_tables$table.tex[[1L]]$quantum <- Inf
stopifnot(grepl("invalid published-table record:", record_check_error(nonfinite_quantum)))

nonpositive_quantum <- valid_record
nonpositive_quantum$published_tables$table.tex[[1L]]$quantum <- 0
stopifnot(grepl("invalid published-table record:", record_check_error(nonpositive_quantum)))

invalid_stars <- valid_record
invalid_stars$published_tables$table.tex[[1L]]$stars <- "****"
stopifnot(grepl("invalid published-table record:", record_check_error(invalid_stars)))

empty_numeric_table <- valid_record
empty_numeric_table$published_tables$table.tex[[1L]] <- data.frame(
  value = double(),
  quantum = double(),
  stars = character()
)
stopifnot(grepl("invalid published-table record:", record_check_error(empty_numeric_table)))

rm(
  record_check_error,
  valid_cell,
  valid_record,
  projected_record,
  invalid_schema,
  empty_tables,
  forbidden_paths,
  path,
  duplicate_paths,
  malformed_coordinate,
  non_data_frame_cell,
  wrong_columns,
  matrix_cell,
  matrix_column_cell,
  non_atomic_column,
  nonfinite_value,
  nonfinite_quantum,
  nonpositive_quantum,
  invalid_stars,
  empty_numeric_table
)
