source(file.path(
  "docs",
  "bootstrap-single-stage-refactor",
  "validation-tools",
  "scientific_record.R"
))

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
