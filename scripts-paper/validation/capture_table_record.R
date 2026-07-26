#!/usr/bin/env Rscript
# Capture a validated schema-3 published-table record atomically.

script_argument <- commandArgs(FALSE)
script_argument <- script_argument[grepl("^--file=", script_argument)]
if (length(script_argument) != 1L) {
  stop("could not determine capture_table_record.R location", call. = FALSE)
}
script_path <- normalizePath(sub("^--file=", "", script_argument), mustWork = TRUE)
setwd(normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE))

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("validation", "table_comparison.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "Usage: capture_table_record.R <output-root> <record.rds>",
    call. = FALSE
  )
}
output_root <- args[[1L]]
record_path <- args[[2L]]
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
