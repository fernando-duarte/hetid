#!/usr/bin/env Rscript

tool_dir <- dirname(normalizePath(
  sub("^--file=", "", grep(
    "^--file=",
    commandArgs(trailingOnly = FALSE),
    value = TRUE
  )),
  mustWork = TRUE
))
source(file.path(tool_dir, "scientific_record.R"))

valid_cell <- data.frame(value = 1.23, quantum = 0.01)
valid_record <- list(
  schema_version = 2L,
  published_tables = list(
    "table.tex" = list(
      "tabular_1/row_1/column_1" = valid_cell
    )
  )
)

stopifnot(isTRUE(paper_validate_table_record(valid_record)))

empty_record <- valid_record
empty_record$published_tables <- list()
traversal_record <- valid_record
names(traversal_record$published_tables) <- "../table.tex"
drive_record <- valid_record
names(drive_record$published_tables) <- "C:/table.tex"
backslash_record <- valid_record
names(backslash_record$published_tables) <- "dir\\..\\table.tex"
coordinate_record <- valid_record
names(coordinate_record$published_tables[[1L]]) <- "bogus"
malformed_record <- valid_record
malformed_record$published_tables[[1L]][[1L]] <- "malformed"
infinite_record <- valid_record
infinite_record$published_tables[[1L]][[1L]]$value <- Inf
zero_quantum_record <- valid_record
zero_quantum_record$published_tables[[1L]][[1L]]$quantum <- 0
matrix_record <- valid_record
matrix_record$published_tables[[1L]][[1L]] <- data.frame(
  value = I(matrix(c(1.23, 2.34), nrow = 1L)),
  quantum = I(matrix(c(0.01, 0.01), nrow = 1L))
)

invalid_records <- list(
  list(schema_version = 2L),
  empty_record,
  traversal_record,
  drive_record,
  backslash_record,
  coordinate_record,
  malformed_record,
  infinite_record,
  zero_quantum_record,
  matrix_record
)

for (record in invalid_records) {
  stopifnot(inherits(
    try(paper_validate_table_record(record), silent = TRUE),
    "try-error"
  ))
}

cat("test_scientific_record: PASS\n")
