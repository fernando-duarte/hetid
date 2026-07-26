#!/usr/bin/env Rscript
# Compare two validated schema-3 published-table records.

script_argument <- commandArgs(FALSE)
script_argument <- script_argument[grepl("^--file=", script_argument)]
if (length(script_argument) != 1L) {
  stop("could not determine compare_table_records.R location", call. = FALSE)
}
script_path <- normalizePath(sub("^--file=", "", script_argument), mustWork = TRUE)
setwd(normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE))

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("validation", "table_comparison.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "Usage: compare_table_records.R <reference.rds> <candidate.rds>",
    call. = FALSE
  )
}
reference <- readRDS(args[[1L]])
candidate <- readRDS(args[[2L]])
invisible(paper_validate_table_record(reference))
invisible(paper_validate_table_record(candidate))
comparison <- paper_compare_table_records(reference, candidate)
if (!isTRUE(comparison)) {
  cat(paste0("- ", comparison), sep = "\n")
  quit(status = 1L)
}
cat("published table-result comparison passed\n")
