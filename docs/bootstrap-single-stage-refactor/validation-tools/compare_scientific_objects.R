comparison_sources <- vapply(
  sys.frames(),
  function(frame) if (is.null(frame$ofile)) "" else frame$ofile,
  character(1)
)
comparison_source <- tail(
  comparison_sources[
    basename(comparison_sources) == "compare_scientific_objects.R"
  ],
  1L
)
if (!length(comparison_source)) {
  script_argument <- commandArgs(FALSE)
  script_argument <- script_argument[grepl("^--file=", script_argument)]
  if (length(script_argument) != 1L) {
    stop("could not determine comparator location", call. = FALSE)
  }
  comparison_source <- sub("^--file=", "", script_argument)
}
comparison_source <- normalizePath(comparison_source, mustWork = TRUE)
comparison_tool_dir <- dirname(comparison_source)
source(file.path(comparison_tool_dir, "scientific_record.R"))
rm(comparison_sources, comparison_source, comparison_tool_dir)

compare_scientific_objects <- function(reference, candidate) {
  paper_validate_table_record(reference)
  paper_validate_table_record(candidate)
  comparison <- paper_compare_table_records(reference, candidate)
  list(
    equal = isTRUE(comparison),
    comparison = comparison,
    reference = reference$published_tables,
    candidate = candidate$published_tables
  )
}

compare_scientific_rds <- function(reference_path, candidate_path) {
  if (!file.exists(reference_path) || !file.exists(candidate_path)) {
    stop("both RDS paths must exist", call. = FALSE)
  }
  compare_scientific_objects(
    readRDS(reference_path),
    readRDS(candidate_path)
  )
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L) {
    stop(
      paste(
        "usage: Rscript compare_scientific_objects.R",
        "reference.rds candidate.rds"
      ),
      call. = FALSE
    )
  }
  result <- compare_scientific_rds(args[[1L]], args[[2L]])
  if (!result$equal) {
    print(result$comparison)
    quit(status = 1L)
  }
  cat("schema-3 published table-result comparison passed\n")
}
