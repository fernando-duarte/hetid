# Compare public/bootstrap scientific records under the universal rule.

source(file.path(
  "scripts-paper", "tests", "support", "scientific_comparison.R"
))

compare_scientific_objects <- function(reference, candidate) {
  reference_projection <- paper_scientific_projection(reference)
  candidate_projection <- paper_scientific_projection(candidate)
  comparison <- paper_scientific_compare(reference, candidate)
  list(
    equal = isTRUE(comparison),
    comparison = comparison,
    reference = reference_projection,
    candidate = candidate_projection
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
  cat("scientific comparison passed\n")
}
