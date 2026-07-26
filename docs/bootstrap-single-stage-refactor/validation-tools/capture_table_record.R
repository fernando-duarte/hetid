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

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "usage: Rscript capture_table_record.R output_root record.rds",
    call. = FALSE
  )
}

output_root <- normalizePath(args[[1L]], mustWork = TRUE)
record_path <- normalizePath(
  args[[2L]],
  mustWork = FALSE
)
dir.create(dirname(record_path), recursive = TRUE, showWarnings = FALSE)

record <- bootstrap_validation_record(output_root)
invisible(paper_validate_table_record(record))
temporary <- tempfile(
  pattern = paste0(basename(record_path), "."),
  tmpdir = dirname(record_path)
)
on.exit(unlink(temporary), add = TRUE)
saveRDS(record, temporary, version = 3L)
roundtrip <- readRDS(temporary)
invisible(paper_validate_table_record(roundtrip))
if (!identical(record, roundtrip)) {
  stop("table record changed during serialization", call. = FALSE)
}
if (!file.rename(temporary, record_path)) {
  stop("could not promote validated table record", call. = FALSE)
}

cat(
  "schema-2 table reference retained at ",
  record_path,
  "\n",
  sep = ""
)
