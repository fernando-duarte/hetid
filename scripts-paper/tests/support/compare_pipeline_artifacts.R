#!/usr/bin/env Rscript
# Compare published table results across two output roots.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("validation", "table_comparison.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "Usage: compare_pipeline_artifacts.R <reference-output-root> <candidate-output-root>",
    call. = FALSE
  )
}
reference <- paper_table_record(args[[1L]])
candidate <- paper_table_record(args[[2L]])
comparison <- paper_compare_table_records(reference, candidate)
if (!isTRUE(comparison)) {
  cat("Table-result comparison failed:\n")
  cat(paste0("- ", comparison), sep = "\n")
  quit(status = 1L)
}
cat("Published table-result comparison passed.\n")
