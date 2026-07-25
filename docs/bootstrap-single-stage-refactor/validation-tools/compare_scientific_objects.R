# Compare final published table numbers at their displayed precision.

source(file.path(
  "scripts-paper",
  "tests",
  "support",
  "published_table_comparison.R"
))

compare_scientific_objects <- function(reference, candidate) {
  stopifnot(
    identical(reference$schema_version, 2L),
    identical(candidate$schema_version, 2L)
  )
  comparison <- paper_published_tables_compare(
    reference$published_tables,
    candidate$published_tables
  )
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
  cat("published table-number comparison passed\n")
}
