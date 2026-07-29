#!/usr/bin/env Rscript
# Compare final published table numbers across two output roots.

script_argument <- commandArgs(FALSE)
script_argument <- script_argument[grepl("^--file=", script_argument)]
if (length(script_argument) != 1L) {
  stop("could not determine compare_output_tables.R location", call. = FALSE)
}
script_path <- normalizePath(
  sub("^--file=", "", script_argument),
  mustWork = TRUE
)
setwd(normalizePath(
  file.path(dirname(script_path), "..", ".."),
  mustWork = TRUE
))

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("validation", "table_comparison.R"))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    paste(
      "Usage: compare_output_tables.R",
      "<reference-output-root> <candidate-output-root>"
    ),
    call. = FALSE
  )
}
comparison <- paper_compare_output_tables(args[[1L]], args[[2L]])
if (!isTRUE(comparison)) {
  cat("Table-result comparison failed:\n")
  cat(paste0("- ", comparison), sep = "\n")
  quit(status = 1L)
}
cat("Published table-result comparison passed.\n")
