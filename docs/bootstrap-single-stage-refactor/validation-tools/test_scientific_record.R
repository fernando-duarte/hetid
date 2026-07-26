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
malformed_record <- valid_record
malformed_record$published_tables[[1L]][[1L]] <- "malformed"
infinite_record <- valid_record
infinite_record$published_tables[[1L]][[1L]]$value <- Inf
zero_quantum_record <- valid_record
zero_quantum_record$published_tables[[1L]][[1L]]$quantum <- 0

invalid_records <- list(
  list(schema_version = 2L),
  empty_record,
  traversal_record,
  malformed_record,
  infinite_record,
  zero_quantum_record
)

for (record in invalid_records) {
  stopifnot(inherits(
    try(paper_validate_table_record(record), silent = TRUE),
    "try-error"
  ))
}

cat("test_scientific_record: PASS\n")
