PAPER_SCIENTIFIC_TOLERANCE <- 1e-4

PAPER_OPERATIONAL_METADATA_NAMES <- c(
  "elapsed", "elapsed_time", "elapsed_seconds",
  "elapsed_minutes", "duration", "duration_seconds",
  "timestamp", "timestamps", "started_at", "finished_at",
  "created_at", "updated_at",
  "cache_source", "cache_path", "cache_dir", "cache_file",
  "temporary_path", "temp_path", "tmp_path",
  "git_commit", "git_sha", "git_commit_sha",
  "runtime_sha",
  "r_version", "r_platform", "platform", "os", "blas", "lapack",
  "pdf_creation_date", "pdf_modification_date",
  "pdf_creator", "pdf_producer"
)

paper_scientific_project_attributes <- function(value_attributes) {
  if (is.null(value_attributes)) {
    return(NULL)
  }
  keep <- setdiff(
    names(value_attributes),
    PAPER_OPERATIONAL_METADATA_NAMES
  )
  lapply(value_attributes[keep], paper_scientific_projection)
}

paper_scientific_projection <- function(value) {
  if (is.environment(value) || is.function(value) || isS4(value)) {
    return(value)
  }
  value_attributes <- attributes(value)
  if (is.list(value) || is.pairlist(value)) {
    value_names <- names(value)
    keep <- if (is.null(value_names)) {
      seq_along(value)
    } else {
      !nzchar(value_names) |
        !value_names %in% PAPER_OPERATIONAL_METADATA_NAMES
    }
    value <- lapply(value[keep], paper_scientific_projection)
    if (!is.null(value_attributes)) {
      value_attributes <- paper_scientific_project_attributes(
        value_attributes
      )
      value_attributes$names <- if (is.null(value_names)) {
        NULL
      } else {
        value_names[keep]
      }
      attributes(value) <- value_attributes
    }
    return(value)
  }
  if (!is.null(value_attributes)) {
    attributes(value) <- paper_scientific_project_attributes(
      value_attributes
    )
  }
  value
}

paper_scientific_compare <- function(reference, candidate) {
  all.equal(
    paper_scientific_projection(reference),
    paper_scientific_projection(candidate),
    tolerance = PAPER_SCIENTIFIC_TOLERANCE,
    check.attributes = TRUE
  )
}

paper_scientific_equal <- function(reference, candidate) {
  isTRUE(paper_scientific_compare(reference, candidate))
}
