# Queries over the canonical artifact manifest.

artifact_variant_id <- function(family, variant) {
  family <- .artifact_scalar(family, "Artifact family")
  variant <- .artifact_scalar(variant, "Artifact variant")
  hit <- artifact_manifest$id[
    artifact_manifest$family == family &
      artifact_manifest$variant == variant
  ]
  if (length(hit) != 1L) {
    stop(
      sprintf("Unknown artifact variant: %s/%s", family, variant),
      call. = FALSE
    )
  }
  hit
}

artifact_variant_path <- function(family, variant) {
  artifact_path(artifact_variant_id(family, variant))
}

artifact_records_by_status <- function(status) {
  status <- .artifact_scalar(status, "Artifact status")
  if (!status %in% PAPER_ARTIFACT_STATUSES) {
    stop(
      sprintf("Unknown artifact status: %s", status),
      call. = FALSE
    )
  }
  artifact_manifest[
    artifact_manifest$status == status, ,
    drop = FALSE
  ]
}

artifact_paths_by_status <- function(status) {
  unname(artifact_records_by_status(status)$new_path)
}
