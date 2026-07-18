# Canonical generated-artifact manifest and query API.

paper_source_once(paper_path("config", "artifact_manifest_data.R"))

.artifact_scalar <- function(x, what) {
  if (!is.character(x) || length(x) != 1L ||
    is.na(x) || !nzchar(x)) {
    stop(
      sprintf("%s must be one nonempty string.", what),
      call. = FALSE
    )
  }
  x
}

.artifact_record <- function(id) {
  id <- .artifact_scalar(id, "Artifact ID")
  row <- artifact_manifest[
    artifact_manifest$id == id, ,
    drop = FALSE
  ]
  if (nrow(row) != 1L) {
    stop(sprintf("Unknown artifact ID: %s", id), call. = FALSE)
  }
  row
}

paper_source_once(paper_path(
  "config", "artifact_registry_queries.R"
))

artifact_id <- function(basename) {
  basename <- .artifact_scalar(basename, "Artifact basename")
  id <- artifact_manifest$id[
    artifact_manifest$basename == basename
  ]
  if (length(id) != 1L) {
    stop(
      sprintf(
        "Unknown or ambiguous artifact basename: %s",
        basename
      ),
      call. = FALSE
    )
  }
  id
}

artifact_path <- function(id) .artifact_record(id)$new_path[[1L]]
artifact_basename <- function(id) .artifact_record(id)$basename[[1L]]
artifact_dir <- function(id) dirname(artifact_path(id))

artifact_group_dir <- function(group) {
  group <- .artifact_scalar(group, "Artifact group")
  groups <- unname(PAPER_ARTIFACT_GROUPS)
  if (!group %in% groups) {
    stop(sprintf("Unknown artifact group: %s", group), call. = FALSE)
  }
  file.path(out_dir, group)
}

create_artifact_directories <- function() {
  dirs <- unique(dirname(artifact_manifest$new_path))
  invisible(vapply(
    dirs,
    dir.create,
    logical(1),
    recursive = TRUE,
    showWarnings = FALSE
  ))
}

.variant_present <- nzchar(artifact_manifest$family)
stopifnot(
  !anyNA(artifact_manifest),
  !anyDuplicated(artifact_manifest$id),
  !anyDuplicated(artifact_manifest$basename),
  all(artifact_manifest$status %in% PAPER_ARTIFACT_STATUSES),
  identical(
    .variant_present,
    nzchar(artifact_manifest$variant)
  ),
  !anyDuplicated(paste(
    artifact_manifest$family[.variant_present],
    artifact_manifest$variant[.variant_present],
    sep = "/"
  ))
)
rm(.variant_present)

paper_source_once(paper_path("config", "artifact_lifecycle.R"))
