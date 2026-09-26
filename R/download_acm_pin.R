# Publish the data and provenance together by renaming a sibling staging directory
download_acm_pin <- function(pin, force, quiet) {
  destination <- dirname(pin$path)
  if (dir.exists(destination)) {
    read_acm_pin(pin)
    if (!force) {
      return(invisible(pin$path))
    }
  }
  parent <- dirname(destination)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(parent)) {
    stop_hetid(paste0("Cannot create pinned ACM cache directory: ", parent))
  }
  stage <- tempfile("acm_stage_", tmpdir = parent)
  if (!dir.create(stage, showWarnings = FALSE)) {
    stop_hetid(paste0("Cannot create ACM staging directory: ", stage))
  }
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  staged_path <- file.path(stage, basename(pin$path))
  existing <- get_acm_data_path("github", pin$frequency)
  acquisition <- "download"
  if (!force && acm_pin_digest_matches(existing, pin)) {
    if (!file.copy(existing, staged_path)) {
      stop_hetid("Could not copy the existing ACM asset into the pinned snapshot")
    }
    acquisition <- "verified-existing-asset"
  } else {
    fetch_url_to_file(pin$source_url, staged_path, quiet, "pinned ACM data")
  }
  read_acm_pin_data(staged_path, pin)
  tryCatch(
    write.dcf(as.list(acm_pin_metadata(pin, acquisition)), paste0(staged_path, ".meta")),
    error = function(e) {
      stop_hetid(paste0("Could not write ACM provenance: ", conditionMessage(e)))
    },
    warning = function(w) {
      stop_hetid(paste0("Could not write ACM provenance: ", conditionMessage(w)))
    }
  )
  validate_acm_pin_metadata(pin, staged_path)
  # A force request verifies a fresh download but retains the identical immutable snapshot.
  # A concurrent publisher may also have completed while this download was in flight
  if (dir.exists(destination)) {
    read_acm_pin(pin)
    return(invisible(pin$path))
  }
  if (!file.rename(stage, destination)) {
    if (!dir.exists(destination)) {
      stop_hetid(paste0("Could not publish pinned ACM snapshot: ", destination))
    }
    read_acm_pin(pin)
  }
  invisible(pin$path)
}
